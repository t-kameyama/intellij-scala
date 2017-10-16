package org.jetbrains.plugins.scala
package format

import java.util.{IllegalFormatConversionException, IllegalFormatException}

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockExpr, ScExpression}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText
import org.jetbrains.plugins.scala.lang.psi.types.result.Success
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}
import org.jetbrains.plugins.scala.project.ProjectContext

/**
 * Pavel Fatin
 */

sealed trait StringPart

case class Text(s: String) extends StringPart {
  def withEscapedPercent(implicit ctx: ProjectContext): List[StringPart] = {
    val literal = createExpressionFromText("\"%\"")
    if (s == "%") List(Text(""), Injection(literal, None), Text(""))
    else {
      val splitted = s.split('%')
      val list = splitted.flatMap(text => List(Injection(literal, None), Text(text))).toList
      if (list.nonEmpty) list.tail else Nil
    }
  }
}

case class Injection(expression: ScExpression, specifier: Option[Specifier]) extends StringPart {
  private implicit def ctx: ProjectContext = expression

  def text: String = expression.getText

  def value: String = expression match {
    case literal: ScLiteral => literal.getValue.toString
    case block: ScBlockExpr => block.exprs.headOption.map(_.getText).mkString
    case element => element.getText
  }

  def format: String = specifier.map(_.format).getOrElse("")

  def expressionType: Option[ScType] = expression.getType().toOption

  def isLiteral: Boolean = expression.isInstanceOf[ScLiteral]

  def isAlphanumericIdentifier: Boolean = !isLiteral && expression.getText.forall(it => it.isLetter || it.isDigit)

  def isFormattingRequired: Boolean = specifier.exists(_.format.length > 2)

  def isComplexBlock: Boolean = expression match {
    case block: ScBlockExpr => block.exprs.length > 1
    case _ => false
  }

  def problem: Option[InjectionProblem] = specifier.flatMap {
    it =>
      val _type = expressionType.map(ScalaType.expandAliases(_)).getOrElse(new Object())
      _type match {
        case Success(result, _) => result match {
          case res: ScType =>
            try {
              val value = Types.valueOf(res)
              value.formatted(it.format)
              None
            } catch {
              case _: IllegalFormatConversionException => Some(Inapplicable)
              case _: IllegalFormatException => Some(Malformed)
            }
          case _ => Some(Malformed)
        }
        case _ => Some(Malformed)
      }
  }
}

case class UnboundSpecifier(specifier: Specifier) extends StringPart

case class UnboundPositionalSpecifier(specifier: Specifier, position: Int) extends StringPart

case class UnboundExpression(expression: ScExpression) extends StringPart

case class Specifier(span: Span, format: String)

case class Span(element: PsiElement, start: Int, end: Int)

sealed trait InjectionProblem

case object Inapplicable extends InjectionProblem

case object Malformed extends InjectionProblem
