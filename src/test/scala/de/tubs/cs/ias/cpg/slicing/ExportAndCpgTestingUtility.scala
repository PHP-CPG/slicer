package de.tubs.cs.ias.cpg.slicing

import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedAST
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}

import java.io.File
import scala.jdk.CollectionConverters.IteratorHasAsScala

trait ExportAndCpgTestingUtility {

  implicit val exportConf: ExportConfig = ExportConfig.apply(new File("export.json"))

  protected def getMethod(cpg: Cpg, method: String): Method = {
    cpg.graph.nodes().asScala.toList.find(node => node.isInstanceOf[Method] && node.asInstanceOf[Method].name == method) match {
      case Some(value: Method) => value
      case _ => throw new RuntimeException(s"there is no method named $method")
    }
  }

  protected def getCallNodes(cpg: Cpg, method: Method, codeRegexp: String*): List[Call] = {
    val callNodes = cpg.graph.nodes().asScala.toList.filter(_.isInstanceOf[Call]).map(_.asInstanceOf[Call])
      .filter(_.getParentMethod.get.name == method.name)
    codeRegexp.map {
      regexp =>
        callNodes.find(node => regexp.r.matches(node.name))
          .getOrElse(throw new RuntimeException(s"no node matching $regexp of method ${method.name} exists we have ${callNodes.map(_.code)}"))
    }.toList
  }

  protected def getCallNodesByName(cpg: Cpg, codeRegexp: String*): List[Call] = {
    val callNodes = cpg.graph.nodes().asScala.toList.filter(_.isInstanceOf[Call]).map(_.asInstanceOf[Call])
    codeRegexp.map {
      regexp =>
        callNodes.find(node => regexp.r.matches(node.name))
          .getOrElse(throw new RuntimeException(s"no node matching $regexp exists we have ${callNodes.map(_.code)}"))
    }.toList
  }

  protected def getCallNodes(cpg: Cpg, codeRegexp: String*): List[Call] = {
    val callNodes = cpg.graph.nodes().asScala.toList.filter(_.isInstanceOf[Call]).map(_.asInstanceOf[Call])
    codeRegexp.map {
      regexp =>
        callNodes.find(node => regexp.r.matches(node.code))
          .getOrElse(throw new RuntimeException(s"no node matching $regexp exists we have ${callNodes.map(_.code)}"))
    }.toList
  }

}
