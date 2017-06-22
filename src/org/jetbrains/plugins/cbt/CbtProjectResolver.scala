package org.jetbrains.plugins.cbt

import java.io.File
import java.nio.file._

import com.intellij.openapi.externalSystem.model.project._
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, ProjectKeys}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver
import org.jetbrains.plugins.cbt.project.CbtProjectSystem
import org.jetbrains.plugins.cbt.project.settings.CbtExecutionSettings
import org.jetbrains.plugins.cbt.structure.{CbtModuleExtData, CbtProjectData}
import org.jetbrains.plugins.scala.project.Version

import scala.xml.{Node, XML}
import org.jetbrains.sbt.RichFile

class CbtProjectResolver extends ExternalSystemProjectResolver[CbtExecutionSettings] {

  private case class ProjectInfo(libraries: Map[String, LibraryData],
                                 modules: Map[String, ModuleData])

  override def resolveProjectInfo(id: ExternalSystemTaskId,
                                  projectPath: String,
                                  isPreviewMode: Boolean,
                                  settings: CbtExecutionSettings,
                                  listener: ExternalSystemTaskNotificationListener): DataNode[ProjectData] = {
    val projectPath = settings.realProjectPath
    val root = new File(projectPath)
    println("Cbt resolver called")
    val xml = XML.loadString(CBT.runAction(Seq("buildInfoXml"), root, Some(id, listener)))
    println(xml.toString)
    val r = convert(xml, settings.isCbt)
    r
  }

  private def convert(project: Node, isCbt: Boolean) =
    convertProject(project, isCbt)

  private def convertProject(project: Node, isCbt: Boolean) = {
    val projectData = new ProjectData(CbtProjectSystem.Id,
      (project \ "@name").text,
      (project \ "@root").text,
      (project \ "@root").text)

    val projectNode = new DataNode[ProjectData](ProjectKeys.PROJECT, projectData, null)

    val libraries = (project \ "libraries" \ "library")
      .map(createLibraryData)
      .map(l => l.getExternalName -> l)
      .toMap
    val cbtLibraries =
      if (!isCbt)
        (project \ "cbtLibraries" \ "library")
          .map(createLibraryData)
      else
        Seq.empty

    Seq(libraries.values, cbtLibraries)
      .flatten
      .map(createLibraryNode(projectNode))
      .foreach(projectNode.addChild)

    val modules = (project \ "modules" \ "module")
      .map(createModuleData)
      .map(m => m.getExternalName -> m)
      .toMap
    (project \ "modules" \ "module")
      .map(m => createModuleNode(projectNode, libraries, cbtLibraries, modules, modules((m \ "@name").text.trim), isCbt, m))
      .foreach(projectNode.addChild)

    projectNode.addChild(createProjectData(projectNode, project))
    projectNode
  }

  private def createProjectData(projectDateNode: DataNode[ProjectData], node: Node) =
    new DataNode(CbtProjectData.Key, new CbtProjectData(), projectDateNode)

  private def createExtModuleData(moduleDataNode: DataNode[ModuleData], module: Node) = {
    val scalacClasspath = (module \ "classpath" \ "classpathItem")
      .map(t => new File(t.text.trim))
    val scalacOptions =
      (module \ "scalacOptions" \ "option")
        .map(_.text)
    val moduleExtData = new CbtModuleExtData(Version((module \ "@scalaVersion").text.trim), scalacClasspath, scalacOptions)
    new DataNode(CbtModuleExtData.Key, moduleExtData, moduleDataNode)
  }

  private def createModuleData(module: Node) =
    new ModuleData((module \ "@name").text,
      CbtProjectSystem.Id,
      "JAVA_MODULE",
      (module \ "@name").text,
      (module \ "@root").text,
      (module \ "@root").text)

  private def createModuleNode(parent: DataNode[_], libraries: Map[String, LibraryData], cbtLibraries: Seq[LibraryData],
                               modules: Map[String, ModuleData], moduleData: ModuleData, isCbt: Boolean, module: Node) = {
    val moduleNode = new DataNode(ProjectKeys.MODULE, moduleData, parent)
    moduleNode.addChild(createContentRoot(module, moduleNode, isCbt))
    (module \ "dependencies" \ "binaryDependency")
      .map(d => createLibraryDependencyNode(moduleNode, libraries(d.text.trim)))
      .foreach(moduleNode.addChild)
    cbtLibraries
      .map(createLibraryDependencyNode(moduleNode, _))
      .foreach(moduleNode.addChild)
    Seq(module \ "dependencies" \ "moduleDependency", module \ "parentBuild")
      .flatten
      .flatMap(m => modules.get(m.text.trim))
      .map(createModuleDependency(moduleNode))
      .foreach(moduleNode.addChild)
    moduleNode.addChild(createExtModuleData(moduleNode, module))
    if (isCbt) { //Dirty hack for now :)
      val name = moduleData.getExternalName
      modules.values
        .filter { m =>
          m.getExternalName != name &&
          m.getExternalName.contains("libraries") &&
          !m.getExternalName.contains("build")
        }
        .map(createModuleDependency(moduleNode))
        .foreach(moduleNode.addChild)

      if (name != "cbt") {
        modules.get("cbt")
          .map(createModuleDependency(moduleNode))
          .foreach(moduleNode.addChild)
      }
    }
    moduleNode
  }

  private def createModuleDependency(parent: DataNode[ModuleData])(moduleData: ModuleData) = {
    val moduleDependencyData = new ModuleDependencyData(parent.getData, moduleData)
    new DataNode(ProjectKeys.MODULE_DEPENDENCY, moduleDependencyData, parent)
  }

  private def createContentRoot(module: Node, parent: DataNode[_], isCbt: Boolean) = {
    val rootPath = Paths.get((module \ "@root").text).toAbsolutePath
    val contentRootData = new ContentRootData(CbtProjectSystem.Id, rootPath.toString)
    (module \ "sources" \ "source")
      .map(s => Paths.get(s.text.trim))
      .filter(s => Files.isDirectory(s))
      .filter(s => s.toAbsolutePath.startsWith(rootPath))
      .foreach(s => contentRootData.storePath(ExternalSystemSourceType.SOURCE, s.toString))
    contentRootData.storePath(ExternalSystemSourceType.EXCLUDED, (module \ "@target").text.trim)
    if (isCbt) {
      val moduleName = (module \ "@name").text.trim
      if (moduleName == "cbt") {
        contentRootData.storePath(ExternalSystemSourceType.EXCLUDED, rootPath.resolve("cache").toAbsolutePath.toString)
      }
    }
    new DataNode(ProjectKeys.CONTENT_ROOT, contentRootData, parent)
  }

  private def createLibraryDependencyNode(parent: DataNode[ModuleData], libraryData: LibraryData) = {
    val dependencyData = new LibraryDependencyData(parent.getData, libraryData, LibraryLevel.PROJECT)
    new DataNode(ProjectKeys.LIBRARY_DEPENDENCY, dependencyData, parent)
  }

  private def createLibraryData(library: Node) = {
    val libraryData = new LibraryData(CbtProjectSystem.Id, (library \ "@name").text.trim)
    (library \ "jar")
      .map(_.text.trim)
      .foreach(libraryData.addPath(LibraryPathType.BINARY, _))
    libraryData
  }

  private def createLibraryNode(parent: DataNode[_])(libraryData: LibraryData) = {
    new DataNode(ProjectKeys.LIBRARY, libraryData, parent)
  }

  override def cancelTask(taskId: ExternalSystemTaskId, listener: ExternalSystemTaskNotificationListener): Boolean = true
}

