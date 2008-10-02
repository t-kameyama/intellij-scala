package org.jetbrains.plugins.scala.lang.psi.api

import com.intellij.psi.impl.source.PsiFileWithStubSupport
import com.intellij.psi.PsiClassOwner
import toplevel.packaging.{ScPackageStatement, ScPackaging}
import toplevel.ScToplevelElement

/**
 * @author ilyas
 */

trait ScalaFile extends ScalaPsiElement with ScToplevelElement with PsiClassOwner with ScDeclarationSequenceHolder 
    with ScImportsHolder with PsiFileWithStubSupport {

  def getPackagings: Array[ScPackaging]

  def getPackageName: String

  def packageStatement: Option[ScPackageStatement]

}