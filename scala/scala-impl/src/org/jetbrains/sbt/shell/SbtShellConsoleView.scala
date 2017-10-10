package org.jetbrains.sbt.shell

import com.intellij.execution.configurations.RemoteConnection
import com.intellij.execution.console.LanguageConsoleImpl
import com.intellij.execution.filters.UrlFilter.UrlFilterProvider
import com.intellij.execution.filters._
import com.intellij.execution.impl.ConsoleViewImpl.ClearAllAction
import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.editor.actions.ToggleUseSoftWrapsToolbarAction
import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.sbt.shell.action._

/**
  * Created by jast on 2017-05-17.
  */
class SbtShellConsoleView private(project: Project, debugConnection: Option[RemoteConnection]) extends
  LanguageConsoleImpl(project, SbtShellFileType.getName, SbtShellLanguage) {

  override def createConsoleActions(): Array[AnAction] = {

    // hackery because we can't construct those actions directly
    val defaultActions = super.createConsoleActions()
    val toggleSoftWrapsAction = defaultActions.find { a => a.isInstanceOf[ToggleUseSoftWrapsToolbarAction] }.get
    val clearAllAction = defaultActions.find { a => a.isInstanceOf[ClearAllAction] }.get

    val debugAction = debugConnection.map(new DebugShellAction(project, _))
    val conditionalActions = Array(debugAction).flatten

    val myToolbarActions: Array[AnAction] = Array(
      toggleSoftWrapsAction,
      new SbtShellScrollToTheEndToolbarAction(getEditor),
      clearAllAction,
      new EOFAction(project),
      new RestartAction(project),
      new StopAction(project),
      new ExecuteTaskAction(this, "products", Option(AllIcons.Actions.Compile))
    )

    val all = conditionalActions ++ myToolbarActions
    all.foreach {act => act.registerCustomShortcutSet(act.getShortcutSet, this) }

    all
  }

}

object SbtShellConsoleView {

  def apply(project: Project, debugConnection: Option[RemoteConnection]): SbtShellConsoleView = {
    val cv = new SbtShellConsoleView(project, debugConnection)
    cv.getConsoleEditor.setOneLineMode(true)

    // stack trace file links
    cv.addMessageFilter(new ExceptionFilter(GlobalSearchScope.allScope(project)))
    // other file links
    cv.addMessageFilter(filePatternFilters(project))
    // url links
    new UrlFilterProvider().getDefaultFilters(project).foreach(cv.addMessageFilter)

    cv
  }

  private def filePatternFilters(project: Project) = {
    import PatternHyperlinkPart._

    def pattern(patternMacro: String) = new RegexpFilter(project, patternMacro).getPattern

    // file with line number
    val fileWithLinePattern = pattern(s"${RegexpFilter.FILE_PATH_MACROS}:${RegexpFilter.LINE_MACROS}")
    // FILE_PATH_MACROS includes a capturing group at the beginning that the format only can handle if the first linkPart is null
    val fileWithLineFormat = new PatternHyperlinkFormat(fileWithLinePattern, false, false, null, PATH, LINE)

    // file output without lines in messages
    val fileOnlyPattern = pattern(RegexpFilter.FILE_PATH_MACROS)
    val fileOnlyFormat = new PatternHyperlinkFormat(fileOnlyPattern, false, false, null, PATH)

    val dataFinder = new PatternBasedFileHyperlinkRawDataFinder(Array(fileWithLineFormat, fileOnlyFormat))
    new PatternBasedFileHyperlinkFilter(project, null, dataFinder)
  }


}
