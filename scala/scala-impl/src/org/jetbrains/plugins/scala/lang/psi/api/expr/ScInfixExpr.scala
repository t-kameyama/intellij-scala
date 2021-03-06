package org.jetbrains.plugins.scala
package lang
package psi
package api
package expr

import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeArgs
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

/**
  * @author Alexander Podkhalyuzin
  */
trait ScInfixExpr extends ScExpression with ScSugarCallExpr {

  import ScInfixExpr._

  def lOp: ScExpression = unapply._1

  def operation: ScReferenceExpression = unapply._2

  def rOp: ScExpression = unapply._3

  def typeArgs: Option[ScTypeArgs] = findChildrenByClassScala(classOf[ScTypeArgs]) match {
    case Array(args) => Some(args)
    case _ => None
  }

  def getBaseExpr: ScExpression = {
    val withAssoc(left, _, _) = this
    left
  }

  def getInvokedExpr: ScExpression = operation

  def argsElement: ScExpression = {
    val withAssoc(_, _, right) = this
    right
  }

  def isAssignmentOperator: Boolean =
    ParserUtils.isAssignmentOperator(operation.getText)

  override def accept(visitor: ScalaElementVisitor): Unit = {
    visitor.visitInfixExpression(this)
  }

  private def unapply = findChildrenByClassScala(classOf[ScExpression]) match {
    case Array(left, operation: ScReferenceExpression, right) =>
      (left, operation, right)
    case _ => throw new RuntimeException("Wrong infix expression: " + getText)
  }
}

object ScInfixExpr {

  def unapply(expression: ScInfixExpr): Some[(ScExpression, ScReferenceExpression, ScExpression)] =
    Some(expression.unapply)

  object withAssoc {

    def unapply(expression: ScInfixExpr): Some[(ScExpression, ScReferenceExpression, ScExpression)] = {
      val (left, operation, right) = expression.unapply

      if (ScalaNamesUtil.clean(operation.refName).endsWith(":")) Some(right, operation, left)
      else Some(left, operation, right)
    }
  }

}