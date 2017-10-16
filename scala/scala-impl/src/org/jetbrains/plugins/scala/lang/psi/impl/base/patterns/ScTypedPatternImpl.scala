package org.jetbrains.plugins.scala
package lang
package psi
package impl
package base
package patterns

import com.intellij.lang.ASTNode
import com.intellij.psi._
import com.intellij.psi.scope.PsiScopeProcessor
import org.jetbrains.plugins.scala.extensions.{PsiTypeExt, ifReadAllowed}
import org.jetbrains.plugins.scala.lang.lexer._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.types.api.{Any, Nothing, ParameterizedType}
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable.TypingContext
import org.jetbrains.plugins.scala.lang.psi.types.result.{Failure, Success, TypeResult}
import org.jetbrains.plugins.scala.lang.psi.types.{ScExistentialType, api, _}

/**
* @author Alexander Podkhalyuzin
* Date: 28.02.2008
*/

class ScTypedPatternImpl(node: ASTNode) extends ScalaPsiElementImpl(node) with ScTypedPattern {
  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => super.accept(visitor)
      case _ => super.accept(visitor)
    }
  }

  def nameId: PsiElement = findChildByType[PsiElement](TokenSets.ID_SET)

  def isWildcard: Boolean = findChildByType[PsiElement](ScalaTokenTypes.tUNDER) != null

  override def isIrrefutableFor(t: Option[ScType]): Boolean = {
    t match {
      case Some(t) => getType() match {
        case Success(tp, _) if t conforms tp => true
        case _ => false
      }
      case _ => false
    }
  }

  override def toString: String = "TypedPattern: " + ifReadAllowed(name)("")

  override def getType(ctx: TypingContext.type = TypingContext): TypeResult[ScType] = {
    typePattern match {
      case Some(tp) =>
        if (tp.typeElement == null) return Failure("No type element for type pattern", Some(this))
        val typeElementType: TypeResult[ScType] =
          tp.typeElement.getType(ctx).map {
            case tp: ScExistentialType =>
              val skolem = tp.quantified
              skolem.extractClassType match {  //todo: type aliases?
                case Some((clazz: ScTypeDefinition, subst)) =>
                  val typeParams = clazz.typeParameters
                  skolem match {
                    case ParameterizedType(des, typeArgs) if typeArgs.length == typeParams.length =>
                      ScParameterizedType(des, typeArgs.zip(typeParams).map {
                        case (arg: ScExistentialArgument, param: ScTypeParam) =>
                          val lowerBound =
                            if (arg.lower.equiv(Nothing)) subst subst param.lowerBound.getOrNothing
                            else arg.lower //todo: lub?
                          val upperBound =
                          if (arg.upper.equiv(Any)) subst subst param.upperBound.getOrAny
                            else arg.upper //todo: glb?
                          ScExistentialArgument(arg.name, arg.args, lowerBound, upperBound)
                        case (tp: ScType, _: ScTypeParam) => tp
                      }).unpackedType
                    case _ => tp
                  }
                case Some((clazz: PsiClass, subst)) =>
                  val typeParams: Array[PsiTypeParameter] = clazz.getTypeParameters
                  skolem match {
                    case ParameterizedType(des, typeArgs) if typeArgs.length == typeParams.length =>
                      ScParameterizedType(des, typeArgs.zip(typeParams).map {
                        case (arg: ScExistentialArgument, param: PsiTypeParameter) =>
                          val lowerBound = arg.lower
                          val upperBound =
                            if (arg.upper.equiv(api.Any)) {
                              val listTypes: Array[PsiClassType] = param.getExtendsListTypes
                              if (listTypes.isEmpty) api.Any
                              else subst.subst(listTypes.toSeq.map(_.toScType()).glb(checkWeak = true))
                            } else arg.upper //todo: glb?
                          ScExistentialArgument(arg.name, arg.args, lowerBound, upperBound)
                        case (tp: ScType, _) => tp
                      }).unpackedType
                    case _ => tp
                  }
                case _ => tp
              }
            case tp: ScType => tp
          }
        this.expectedType match {
          case Some(expectedType) =>
            typeElementType.map {
              case resType => expectedType.glb(resType, checkWeak = false)
            }
          case _ => typeElementType
        }
      case None => Failure("No type pattern", Some(this))
    }
  }

  override def processDeclarations(processor: PsiScopeProcessor, state: ResolveState, lastParent: PsiElement,
                                   place: PsiElement): Boolean = {
    ScalaPsiUtil.processImportLastParent(processor, state, place, lastParent, getType())
  }

  override def getOriginalElement: PsiElement = super[ScTypedPattern].getOriginalElement
}
