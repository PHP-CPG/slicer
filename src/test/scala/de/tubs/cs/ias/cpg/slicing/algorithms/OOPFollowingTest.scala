package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.slicing.ExportAndCpgTestingUtility
import de.tubs.cs.ias.cpg.slicing.representation.{ProgramSlice, SliceEdgeTypes}
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OOPFollowingTest extends AnyWordSpec with Matchers with PHPVersions with ExportAndCpgTestingUtility {

  //val versions: List[PHPVersion.Value] = List(PHPVersion.V8,PHPVersion.V7)
  val versions: List[PHPVersion.Value] = List(PHPVersion.V8)

  versions.foreach {
    implicit version =>

      "OOP" should {
        "result in object being added as DDG edge" in new CpgFromCodeTestFixture(
          """$obj = new Obj();
            |$var = "42";
            |$ret = $obj->doStuff($var);
            |echo $ret;
            |""".stripMargin, configFile = Some("cpg.conf")
        ) {
          //new CpgDotFileCreator(cpg.graph).show()
          val echo :: ass :: Nil = getCallNodes(cpg, "ECHO.*","ASSIGN.*obj.*")
          //new CpgDotFileCreator(cpg.graph).show()
          val slice: ProgramSlice = ProgramSlice.slice(echo)
          val assS :: Nil = slice.getNodes(ass)
          assS.outE().length shouldBe 1
          assS.outE(SliceEdgeTypes.DATA_DEPENDENCE).head.attributes.contains("obj") shouldBe true
        }
        "work with coalesce in parameter" in new CpgFromCodeTestFixture(
          """$ret = $obj->doStuff($a ?? 'aa');
            |echo $ret;
            |""".stripMargin, configFile = Some("cpg.conf")
        ) {
          //new CpgDotFileCreator(cpg.graph).show()
          val echo :: Nil = getCallNodes(cpg, "ECHO.*")
          ProgramSlice.slice(echo)
          // no error means this test passes
        }
      }

  }
}
