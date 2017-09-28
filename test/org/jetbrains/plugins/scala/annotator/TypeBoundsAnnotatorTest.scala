package org.jetbrains.plugins.scala.annotator

import org.intellij.lang.annotations.Language
import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.jetbrains.plugins.scala.extensions.{IteratorExt, PsiElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeBoundsOwner

/**
  * Nikolay.Tropin
  * 28-Sep-17
  */
class TypeBoundsAnnotatorTest extends SimpleTestCase {

  def testBoundItself(): Unit = {
    assertMatches(messages("def f[A <: A](a: A) = ???")) {
      case Error("A <: A", "A has itself as a bound") :: Nil =>
    }
    assertMatches(messages("def f[A >: A <: AnyRef](a: A) = ???")) {
      case Error("A >: A <: AnyRef", "A has itself as a bound") :: Nil =>
    }
    assertMatches(messages("type A <: A")) {
      case Error("type A <: A", "A has itself as a bound") :: Nil =>
    }
  }

  def testConformance(): Unit = {
    assertMatches(messages("def f[A, B, C >: A <: B] (a: A) = ???")) {
      case Error("C >: A <: B", "Lower bound doesn't conform to upper bound") :: Nil =>
    }
    assertMatches(messages("def f[A, B >: A <: AnyRef](a: A) = ???")) {
      case Nil =>
    }
  }

  def messages(@Language(value = "Scala", prefix = "object Test {\n", suffix = "\n}") code: String): List[Message] = {
    val file = code.parse

    val mock = new AnnotatorHolderMock(file)
    val annotator = ScalaAnnotator.forProject

    val owners = file.depthFirst().filterByType[ScTypeBoundsOwner]

    owners.foreach(annotator.annotate(_, mock))
    mock.errorAnnotations
  }
}
