package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.slicing.ExportAndCpgTestingUtility
import de.tubs.cs.ias.cpg.slicing.representation.{ProgramSlice, SliceNode}
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedAST
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FunctionSlicingTest extends AnyWordSpec with Matchers with PHPVersions with ExportAndCpgTestingUtility {


  implicit val version: PHPVersion.Value = PHPVersion.V8

  "give me the correct slice for going only down" in new CpgFromCodeTestFixture(
    """function call($var) {
      | echo $var;
      |}
      |$x = $_GET['test'];
      |call($x);
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    val echo :: Nil = getCallNodesByName(cpg, "ECHO")
    val slice: ProgramSlice = new ProgramSlice(echo)
    FunctionSlicing.slice(slice, SliceNode(echo)(slice), List())
  }
  "give me the correct slice for going only up" in new CpgFromCodeTestFixture(
    """function filter($var) {
      |   return $var;
      |}
      |$x = $_GET['test'];
      |$y = filter($x);
      |echo $y;
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    val echo :: Nil = getCallNodesByName(cpg, "ECHO")
    val slice: ProgramSlice = new ProgramSlice(echo)
    FunctionSlicing.slice(slice, SliceNode(echo)(slice), List())
  }
  "give me the correct slice for going up and down" in new CpgFromCodeTestFixture(
    """function filter($x) {
      |   return $x;
      |}
      |
      |function doStuff($x) {
      |   $y = filter($x);
      |   echo $y;
      |}
      |
      |function uninvoled($x) {
      |  filter($x);
      |}
      |
      |$z = $_GET['test'];
      |doStuff($z);
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    val echo :: Nil = getCallNodesByName(cpg, "ECHO")
    val slice: ProgramSlice = new ProgramSlice(echo)
    FunctionSlicing.slice(slice, SliceNode(echo)(slice), List())
  }

  "inter function slicing account for named params" in new CpgFromCodeTestFixture(
    """function call($prev,$var) {
      | return $var;
      |}
      |$buff = call(var:"test",prev:"test");
      |echo $buff;
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    val echo :: Nil = getCallNodesByName(cpg, "ECHO")
    val slice: ProgramSlice = new ProgramSlice(echo)
    FunctionSlicing.slice(slice, SliceNode(echo)(slice), List())
  }
}
