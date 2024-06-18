package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.dotFileExporter.DotFileCreatorExpansion.CpgDotFileCreator
import de.tubs.cs.ias.cpg.slicing.ExportAndCpgTestingUtility
import de.tubs.cs.ias.cpg.slicing.representation.ProgramSlice
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import wvlet.log.LogLevel

class OldBugsTest extends AnyWordSpec with Matchers with PHPVersions with ExportAndCpgTestingUtility {


  implicit val version: PHPVersion.Value = PHPVersion.V8

  "recursion StackOverflow" in new CpgFromCodeTestFixture(
    """function f($u, $i){
      |  if($i < 1) {f($u, $i + 1);}
      |  echo $u;
      |}
      |f($_GET["a"], 0);""".stripMargin) {
    val sink :: Nil: List[Call] = getCallNodesByName(cpg, "ECHO.*")
    try {
      wvlet.log.Logger.rootLogger.setLogLevel(LogLevel.DEBUG)
      val slice: ProgramSlice = ProgramSlice.slice(sink)
    } catch {
      case e: StackOverflowError => fail(e)
    } finally {
      wvlet.log.Logger.rootLogger.setLogLevel(LogLevel.INFO)
    }
  }
  "closure should be sane" in new CpgFromCodeTestFixture(
    """
      |array_map(fn($n) => $n, array_filter($a['b'], fn($n) => !is_null($n)));
      """.stripMargin
  ) {
    //new CpgDotFileCreator(cpg.graph).show()
  }
}
