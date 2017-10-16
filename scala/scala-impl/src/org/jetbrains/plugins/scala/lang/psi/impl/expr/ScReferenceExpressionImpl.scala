package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import com.intellij.openapi.progress.ProgressManager
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.IncorrectOperationException
import org.jetbrains.plugins.scala.annotator.intention.ScalaImportTypeFix
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.completion.lookups.LookupElementManager
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSelfTypeElement, ScSimpleTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScFieldId, ScPrimaryConstructor, ScReferenceElement}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScParameterType}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportStmt
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.api.{ScPackage, ScalaElementVisitor, ScalaRecursiveElementVisitor}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.{createExpressionFromText, createExpressionWithContextFromText}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticValue
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{DesignatorOwner, ScDesignatorType, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.ScTypePolymorphicType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable.TypingContext
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScTypeUtil.AliasType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.lang.resolve._
import org.jetbrains.plugins.scala.lang.resolve.processor._

import scala.collection.mutable.ArrayBuffer

/**
  * @author AlexanderPodkhalyuzin
  *         Date: 06.03.2008
  */
class ScReferenceExpressionImpl(node: ASTNode) extends ScReferenceElementImpl(node) with ScReferenceExpression {
  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => accept(visitor)
      case _ => super.accept(visitor)
    }
  }

  override def toString: String = "ReferenceExpression: " + ifReadAllowed(getText)("")

  def nameId: PsiElement = findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER)

  override def accept(visitor: ScalaElementVisitor) {
    visitor.visitReferenceExpression(this)
  }

  def multiResolve(incomplete: Boolean): Array[ResolveResult] = {
    if (resolveFunction != null) resolveFunction()
    else this.multiResolveImpl(incomplete)
  }

  def shapeResolve: Array[ResolveResult] = {
    ProgressManager.checkCanceled()
    if (shapeResolveFunction != null) shapeResolveFunction()
    else this.shapeResolveImpl
  }

  def doResolve(processor: BaseProcessor, accessibilityCheck: Boolean = true): Array[ResolveResult] =
    new ReferenceExpressionResolver().doResolve(this, processor, accessibilityCheck)

  def bindToElement(element: PsiElement): PsiElement = bindToElement(element, None)

  def bindToElement(element: PsiElement, containingClass: Option[PsiClass]): PsiElement = {
    def tail(qualName: String)(simpleImport: => PsiElement): PsiElement = {
      safeBindToElement(qualName, {
        case (qual, true) =>
          createExpressionWithContextFromText(qual, getContext, this).asInstanceOf[ScReferenceExpression]
        case (qual, false) =>
          createExpressionFromText(qual).asInstanceOf[ScReferenceExpression]
      })(simpleImport)
    }

    if (isReferenceTo(element)) return this
    element match {
      case _: ScTrait | _: ScClass =>
        ScalaPsiUtil.getCompanionModule(element.asInstanceOf[ScTypeDefinition]) match {
          case Some(obj: ScObject) => bindToElement(obj, containingClass)
          case _ => this
        }
      case c: PsiClass =>
        val kinds = getKinds(incomplete = false)
        if (!ResolveUtils.kindMatches(element, kinds))
          throw new IncorrectOperationException(s"class $c does not match expected kind,\nexpected: ${kinds.mkString(", ")}")
        if (!ScalaNamesUtil.equivalent(refName, c.name))
          throw new IncorrectOperationException(s"class $c does not match expected name $refName")
        val qualName = c.qualifiedName
        if (qualName != null) {
          return tail(qualName) {
            ScalaImportTypeFix.getImportHolder(ref = this, project = getProject).addImportForClass(c, ref = this)
            //need to use unqualified reference with new import
            if (!this.isQualified) this
            else this.replace(createExpressionFromText(this.refName).asInstanceOf[ScReferenceExpression])
            //todo: conflicts with other classes with same name?
          }
        }
        this
      case _: ScTypeAlias =>
        throw new IncorrectOperationException("type does not match expected kind")
      case fun: ScFunction if ScalaPsiUtil.hasStablePath(fun) && fun.name == "apply" =>
        bindToElement(fun.containingClass)
      case pack: ScPackage =>
        val qualName = pack.getQualifiedName
        tail(qualName) {
          ScalaImportTypeFix.getImportHolder(this, getProject).addImportForPath(qualName, this)
          this
        }
      case elem: PsiNamedElement =>
        if (!ScalaNamesUtil.equivalent(refName, elem.name))
          throw new IncorrectOperationException(s"named element $elem does not match expected name $refName")
        ScalaPsiUtil.nameContext(elem) match {
          case memb: PsiMember =>
            val cClass = containingClass.getOrElse(memb.containingClass)
            if (cClass != null && cClass.qualifiedName != null) {
              val qualName: String = cClass.qualifiedName + "." + elem.name
              return tail(qualName) {
                ScalaImportTypeFix.getImportHolder(this, getProject).addImportForPsiNamedElement(elem, this, Some(cClass))
                this
              }
            }
          case _ =>
        }
        this
      case _ => throw new IncorrectOperationException("Cannot bind to element: " + element)
    }
  }

  def getVariants: Array[Object] = getVariants(implicits = true, filterNotNamedVariants = false)

  /**
    * Important! Do not change types of Object values, this can cause errors due to bad architecture.
    */
  override def getVariants(implicits: Boolean, filterNotNamedVariants: Boolean): Array[Object] = {
    val isInImport: Boolean = ScalaPsiUtil.getParentOfType(this, classOf[ScImportStmt]) != null

    getSimpleVariants(implicits, filterNotNamedVariants).flatMap {
      case res: ScalaResolveResult =>
        val qualifier = res.fromType.getOrElse(Nothing)
        LookupElementManager.getLookupElement(res, isInImport = isInImport, qualifierType = qualifier)
      case r => Seq(r.getElement)
    }
  }

  def getSimpleVariants(implicits: Boolean, filterNotNamedVariants: Boolean): Array[ResolveResult] = {
    val kinds = getKinds(incomplete = true)
    val processor = if (implicits) new ImplicitCompletionProcessor(kinds, this)
    else new CompletionProcessor(kinds, this)

    doResolve(processor).filter {
      case _ if !filterNotNamedVariants => true
      case res: ScalaResolveResult => res.isNamedParameter
      case _ => false
    }
  }

  def getSameNameVariants: Array[ResolveResult] = this.doResolve(
    new ImplicitCompletionProcessor(getKinds(incomplete = true), this) {

      override protected val forName = Some(refName)
    })

  def getKinds(incomplete: Boolean, completion: Boolean = false): _root_.org.jetbrains.plugins.scala.lang.resolve.ResolveTargets.ValueSet = {
    getContext match {
      case _ if completion => StdKinds.refExprQualRef // SC-3092
      case _: ScReferenceExpression => StdKinds.refExprQualRef
      case postf: ScPostfixExpr if this == postf.operation || this == postf.getBaseExpr => StdKinds.refExprQualRef
      case pref: ScPrefixExpr if this == pref.operation || this == pref.getBaseExpr => StdKinds.refExprQualRef
      case inf: ScInfixExpr if this == inf.operation || this == inf.getBaseExpr => StdKinds.refExprQualRef
      case _ => StdKinds.refExprLastRef
    }
  } // See SCL-3092

  def multiType: Array[TypeResult[ScType]] = {
    val buffer = ArrayBuffer[TypeResult[ScType]]()
    val iterator = multiResolve(incomplete = false).iterator
    while (iterator.hasNext) {
      iterator.next() match {
        case srr: ScalaResolveResult => buffer += convertBindToType(srr)
        case _ =>
      }
    }
    buffer.toArray
  }

  protected override def innerType: TypeResult[ScType] = {
    this.bind() match {
      case Some(srr) => convertBindToType(srr)
      case _ => resolveFailure
    }
  }

  def shapeType: TypeResult[ScType] = {
    shapeResolve match {
      case Array(bind: ScalaResolveResult) if bind.isApplicable() => convertBindToType(bind)
      case _ => resolveFailure
    }
  }

  def shapeMultiType: Array[TypeResult[ScType]] = {
    val buffer = ArrayBuffer[TypeResult[ScType]]()
    val iterator = shapeResolve.iterator
    while (iterator.hasNext) {
      iterator.next() match {
        case srr: ScalaResolveResult => buffer += convertBindToType(srr)
        case _ =>
      }
    }
    buffer.toArray
  }

  private def isMetaInlineDefn(p: ScParameter): Boolean = {
    p.owner match {
      case f: ScFunctionDefinition if f.getModifierList != null =>
        f.getModifierList.findFirstChildByType(ScalaTokenTypes.kINLINE) != null
      case _ => false
    }
  }

  protected def convertBindToType(bind: ScalaResolveResult): TypeResult[ScType] = {
    val fromType: Option[ScType] = bind.fromType
    val unresolvedTypeParameters: Seq[TypeParameter] = bind.unresolvedTypeParameters.getOrElse(Seq.empty)

    def stableTypeRequired: Boolean = {
      //SLS 6.4

      //The expected type pt is a stable type or
      //The expected type pt is an abstract type with a stable type as lower bound,
      // and the type T of the entity referred to by p does not conforms to pt,
      this.expectedTypeEx() match {
        case Some((tp, typeElementOpt)) =>
          (tp match {
            case ScAbstractType(_, lower, _) => lower
            case _ => tp
          }).isAliasType match {
            case Some(AliasType(_, Success(lower: DesignatorOwner, _), _)) if lower.isStable =>
              return true
            case _ =>
              tp match {
                case designatorOwner: DesignatorOwner if designatorOwner.isStable =>
                  return true
                case _ =>
              }
              typeElementOpt match {
                case Some(te) =>
                  te.getContext match {
                    case pt: ScParameterType =>
                      pt.getContext match {
                        case p: ScParameter if !p.getDefaultExpression.contains(this) =>
                          p.owner match {
                            case f: ScFunction =>
                              var found = false
                              val visitor = new ScalaRecursiveElementVisitor {
                                override def visitSimpleTypeElement(simple: ScSimpleTypeElement): Unit = {
                                  if (simple.singleton) {
                                    simple.reference match {
                                      case Some(ref) if ref.refName == p.name && ref.resolve() == p => found = true
                                      case _ =>
                                    }
                                  }
                                  super.visitSimpleTypeElement(simple)
                                }
                              }
                              f.returnTypeElement.foreach(_.accept(visitor))
                              if (found) return true
                            case _ => //looks like it's not working for classes, so do nothing here.
                          }
                        case _ =>
                      }
                    case _ =>
                  }
                case _ =>
              }
          }
        case _ =>
      }
      //The path p occurs as the prefix of a selection and it does not designate a constant
      //todo: It seems that designating constant is not a problem, while we haven't type like Int(1)
      getContext match {
        case i: ScSugarCallExpr if this == i.getBaseExpr => true
        case m: ScMethodCall if this == m.getInvokedExpr => true
        case ref: ScReferenceExpression if ref.qualifier.contains(this) => true
        case _ => false
      }
    }

    val inner: ScType = bind match {
      case ScalaResolveResult(fun: ScFun, s) =>
        s.subst(fun.polymorphicType)
      //prevent infinite recursion for recursive pattern reference
      case ScalaResolveResult(self: ScSelfTypeElement, _) =>
        val clazz = PsiTreeUtil.getContextOfType(self, true, classOf[ScTemplateDefinition])
        ScThisReferenceImpl.getThisTypeForTypeDefinition(clazz, this) match {
          case success: Success[ScType] => success.get
          case failure => return failure
        }
      case r@ScalaResolveResult(refPatt: ScBindingPattern, s) =>
        ScalaPsiUtil.nameContext(refPatt) match {
          case pd: ScPatternDefinition if PsiTreeUtil.isContextAncestor(pd, this, true) => pd.declaredType match {
            case Some(t) => t
            case None => return Failure("No declared type found", Some(this))
          }
          case vd: ScVariableDefinition if PsiTreeUtil.isContextAncestor(vd, this, true) => vd.declaredType match {
            case Some(t) => t
            case None => return Failure("No declared type found", Some(this))
          }
          case _ =>
            if (stableTypeRequired && refPatt.isStable) {
              r.fromType match {
                case Some(fT) => ScProjectionType(fT, refPatt, superReference = false)
                case None => ScalaType.designator(refPatt)
              }
            } else {
              val result = refPatt.getType()
              result match {
                case Success(tp, _) => s.subst(tp)
                case _ => return result
              }
            }
        }
      case ScalaResolveResult(param: ScParameter, _) if isMetaInlineDefn(param) =>
        ScalaPsiElementFactory.createTypeFromText("scala.meta.Stat", param.getContext, null).get
      case r@ScalaResolveResult(param: ScParameter, s) =>
        val owner = param.owner match {
          case f: ScPrimaryConstructor => f.containingClass
          case _: ScFunctionExpr => null
          case f => f
        }
        def isMethodDependent(function: ScFunction): Boolean = {
          def checkte(te: ScTypeElement): Boolean = {
            var res = false
            te.accept(new ScalaRecursiveElementVisitor {
              override def visitReference(ref: ScReferenceElement): Unit = {
                if (ref.resolve() == param) res = true
                super.visitReference(ref)
              }
            })
            res
          }
          function.returnTypeElement match {
            case Some(te) if checkte(te) => return true
            case _ =>
          }
          !function.parameters.forall { case param =>
            param.typeElement match {
              case Some(te) => !checkte(te)
              case _ => true
            }
          }
        }

        r.fromType match {
          case Some(fT) if param.isVal && stableTypeRequired => ScProjectionType(fT, param, superReference = false)
          case Some(ScThisType(clazz)) if owner != null && PsiTreeUtil.isContextAncestor(owner, this, true) &&
            stableTypeRequired && owner.isInstanceOf[ScTypeDefinition] && owner == clazz => ScalaType.designator(param) //todo: think about projection from this type?
          case _ if owner != null && PsiTreeUtil.isContextAncestor(owner, this, true) &&
            stableTypeRequired && !owner.isInstanceOf[ScTypeDefinition] => ScalaType.designator(param)
          case _ =>
            owner match {
              case function: ScFunction if PsiTreeUtil.isContextAncestor(function, this, true) &&
                isMethodDependent(function) => ScalaType.designator(param)
              case _ =>
                val result = param.getRealParameterType
                s.subst(result match {
                  case Success(tp, _) => tp
                  case _ => return result
                })
            }
        }
      case ScalaResolveResult(value: ScSyntheticValue, _) => value.tp
      case ScalaResolveResult(fun: ScFunction, s) if fun.isProbablyRecursive =>
        val optionResult: Option[ScType] = {
          fun.definedReturnType match {
            case s: Success[ScType] => Some(s.get)
            case _: Failure => None
          }
        }
        s.subst(fun.polymorphicType(optionResult))
      case result@ScalaResolveResult(fun: ScFunction, s) =>
        val functionType = s.subst(fun.polymorphicType())
        if (result.isDynamic) DynamicResolveProcessor.getDynamicReturn(functionType)
        else functionType
      case ScalaResolveResult(param: ScParameter, s) if param.isRepeatedParameter =>
        val result = param.getType(TypingContext)
        val computeType = s.subst(result match {
          case Success(tp, _) => tp
          case _ => return result
        })
        elementScope.getCachedClass("scala.collection.Seq")
          .map {
            ScalaType.designator
          }.map {
          ScParameterizedType(_, Seq(computeType))
        }.getOrElse(computeType)
      case ScalaResolveResult(obj: ScObject, _) =>
        def tail = {
          fromType match {
            case Some(tp) => ScProjectionType(tp, obj, superReference = false)
            case _ => ScalaType.designator(obj)
          }
        }
        //hack to add Eta expansion for case classes
        if (obj.isSyntheticObject) {
          ScalaPsiUtil.getCompanionModule(obj) match {
            case Some(clazz) if clazz.isCase && !clazz.hasTypeParameters =>
              this.expectedType() match {
                case Some(tp) =>
                  if (FunctionType.isFunctionType(tp)) {
                    val tp = tail
                    val processor =
                      new MethodResolveProcessor(this, "apply", Nil, Nil, Nil)
                    processor.processType(tp, this)
                    val candidates = processor.candidates
                    if (candidates.length != 1) tail
                    else convertBindToType(candidates(0)).getOrElse(tail)
                  } else tail
                case _ => tail
              }
            case _ => tail
          }
        } else tail
      case r@ScalaResolveResult(f: ScFieldId, s) =>
        if (stableTypeRequired && f.isStable) {
          r.fromType match {
            case Some(fT) => ScProjectionType(fT, f, superReference = false)
            case None => ScalaType.designator(f)
          }
        } else {
          val result = f.getType()
          result match {
            case Success(tp, _) => s.subst(tp)
            case _ => return result
          }
        }
      case ScalaResolveResult(typed: ScTypedDefinition, s) =>
        val result = typed.getType()
        result match {
          case Success(tp, _) => s.subst(tp)
          case _ => return result
        }
      case ScalaResolveResult(pack: PsiPackage, _) => ScalaType.designator(pack)
      case ScalaResolveResult(clazz: ScClass, s) if clazz.isCase =>
        s.subst(clazz.constructor.
          getOrElse(return Failure("Case Class hasn't primary constructor", Some(this))).polymorphicType)
      case ScalaResolveResult(clazz: ScTypeDefinition, s) if clazz.typeParameters.nonEmpty =>
        s.subst(ScParameterizedType(ScalaType.designator(clazz),
          clazz.typeParameters.map(TypeParameterType(_, Some(s)))))
      case ScalaResolveResult(clazz: PsiClass, _) => new ScDesignatorType(clazz, true) //static Java class
      case ScalaResolveResult(field: PsiField, s) =>
        s.subst(field.getType.toScType())
      case ScalaResolveResult(method: PsiMethod, s) =>
        val returnType = Option(method.containingClass).filter {
          method.getName == "getClass" && _.getQualifiedName == "java.lang.Object"
        }.flatMap { _ =>
          val maybeReference = qualifier.orElse {
            val result: Option[Typeable] = getContext match {
              case infixExpr: ScInfixExpr if infixExpr.operation == this => Some(infixExpr.lOp)
              case postfixExpr: ScPostfixExpr if postfixExpr.operation == this => Some(postfixExpr.operand)
              case _ => ScalaPsiUtil.drvTemplate(this)
            }
            result
          }

          def getType(element: PsiNamedElement): Option[ScType] = Option(element).collect {
            case pattern: ScBindingPattern => pattern
            case fieldId: ScFieldId => fieldId
            case parameter: ScParameter => parameter
          }.flatMap {
            _.getType().toOption
          }

          def removeTypeDesignator(`type`: ScType): ScType = {
            val maybeType = `type` match {
              case ScDesignatorType(element) =>
                getType(element)
              case projectionType: ScProjectionType =>
                getType(projectionType.actualElement).map {
                  projectionType.actualSubst.subst
                }
              case _ => None
            }
            maybeType.map(removeTypeDesignator).getOrElse(`type`)
          }

          def convertQualifier(jlClass: PsiClass): ScType = {
            val maybeType = maybeReference.flatMap {
              _.getType().toOption
            }

            val upperBound = maybeType.flatMap {
              case ScThisType(clazz) => Some(ScDesignatorType(clazz))
              case ScDesignatorType(_: ScObject) => None
              case ScCompoundType(comps, _, _) => comps.headOption.map(removeTypeDesignator)
              case tp => Some(tp).map(removeTypeDesignator)
            }.getOrElse(Any)

            val argument = ScExistentialArgument("_$1", Nil, Nothing, upperBound)
            ScExistentialType(ScParameterizedType(ScDesignatorType(jlClass), Seq(argument)), List(argument))
          }

          elementScope.getCachedClass("java.lang.Class")
            .map(convertQualifier)
        }

        ResolveUtils.javaPolymorphicType(method, s, this.resolveScope, returnType)
      case _ => return resolveFailure
    }
    qualifier match {
      case Some(_: ScSuperReference) =>
      case None => //infix, prefix and postfix
        getContext match {
          case sugar: ScSugarCallExpr if sugar.operation == this =>
            sugar.getBaseExpr.getNonValueType() match {
              case Success(ScTypePolymorphicType(_, typeParams), _) =>
                inner match {
                  case ScTypePolymorphicType(internal, typeParams2) =>
                    return Success(ScalaPsiUtil.removeBadBounds(ScTypePolymorphicType(internal, typeParams ++ typeParams2 ++ unresolvedTypeParameters)), Some(this))
                  case _ =>
                    return Success(ScTypePolymorphicType(inner, typeParams ++ unresolvedTypeParameters), Some(this))
                }
              case _ if unresolvedTypeParameters.nonEmpty =>
                inner match {
                  case ScTypePolymorphicType(internal, typeParams) =>
                    return Success(ScTypePolymorphicType(internal, unresolvedTypeParameters ++ typeParams), Some(this))
                  case _ =>
                    return Success(ScTypePolymorphicType(inner, unresolvedTypeParameters), Some(this))
                }
              case _ =>
            }
          case _ =>
        }
      case Some(qualifier) =>
        qualifier.getNonValueType() match {
          case Success(ScTypePolymorphicType(_, typeParams), _) =>
            inner match {
              case ScTypePolymorphicType(internal, typeParams2) =>
                return Success(ScalaPsiUtil.removeBadBounds(ScTypePolymorphicType(internal, typeParams ++ typeParams2 ++ unresolvedTypeParameters)), Some(this))
              case _ =>
                return Success(ScTypePolymorphicType(inner, typeParams ++ unresolvedTypeParameters), Some(this))
            }
          case _ if unresolvedTypeParameters.nonEmpty =>
            inner match {
              case ScTypePolymorphicType(internal, typeParams) =>
                return Success(ScTypePolymorphicType(internal, unresolvedTypeParameters ++ typeParams), Some(this))
              case _ =>
                return Success(ScTypePolymorphicType(inner, unresolvedTypeParameters), Some(this))
            }
          case _ =>
        }
    }
    Success(inner, Some(this))
  }

  def getPrevTypeInfoParams: Seq[TypeParameter] = {
    qualifier match {
      case Some(_: ScSuperReference) => Seq.empty
      case Some(qual) =>
        qual.getNonValueType().map {
          case t: ScTypePolymorphicType => t.typeParameters
          case _ => Seq.empty
        }.getOrElse(Seq.empty)
      case _ => getContext match {
        case sugar: ScSugarCallExpr if sugar.operation == this =>
          sugar.getBaseExpr.getNonValueType().map {
            case t: ScTypePolymorphicType => t.typeParameters
            case _ => Seq.empty
          }.getOrElse(Seq.empty)
        case _ => Seq.empty
      }
    }
  }

  private def resolveFailure = Failure("Cannot resolve expression", Some(this))
}
