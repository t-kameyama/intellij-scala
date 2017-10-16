package org.jetbrains.plugins.scala.codeInspection.methodSignature

import com.intellij.codeInspection._
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.methodSignature.quickfix.AddCallParentheses
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.FunctionType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker

/**
 * Pavel Fatin
 *
 * TODO test:
 * {{{
 *   object A {
 *     def foo(): Int = 1
 *     foo // warn
 *
 *     def goo(x: () => Int) = 1
 *     goo(foo) // okay
 *
 *     foo : () => Int // okay
 *
 *     def bar[A]() = 0
 *     bar[Int] // warn
 *     bar[Int]: () => Any // okay
 *   }
 * }}}
 */
class EmptyParenMethodAccessedAsParameterlessInspection extends AbstractMethodSignatureInspection(
  "ScalaEmptyParenMethodAccessedAsParameterless", "Empty-paren method accessed as parameterless") {

  override def actionFor(implicit holder: ProblemsHolder): PartialFunction[PsiElement, Unit] = {
    case e: ScReferenceExpression if e.isValid && IntentionAvailabilityChecker.checkInspection(this, e) =>
      e.getParent match {
        case gc: ScGenericCall =>
          ScalaPsiUtil.findCall(gc) match {
            case None => check(e, holder, gc.getType())
            case Some(_) =>
          }
        case _: ScMethodCall | _: ScInfixExpr | _: ScPrefixExpr | _: ScUnderscoreSection => // okay
        case _ => check(e, holder, e.getType())
      }
  }

  private def check(e: ScReferenceExpression, holder: ProblemsHolder, callType: TypeResult[ScType]) {
    e.resolve() match {
      case (f: ScFunction) if !f.isInCompiledFile && f.isEmptyParen =>
        callType.toOption match {
          case Some(FunctionType(_, Seq())) =>
          // might have been eta-expanded to () => A, so don't worn.
          // this avoids false positives. To be more accurate, we would need an 'etaExpanded'
          // flag in ScalaResolveResult.
          case _ => holder.registerProblem(e.nameId, getDisplayName, new AddCallParentheses(e))
        }
      case _ =>
    }
  }
}