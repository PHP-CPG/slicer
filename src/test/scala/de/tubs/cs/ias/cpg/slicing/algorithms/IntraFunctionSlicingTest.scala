package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.dotFileExporter.DotFileCreatorExpansion.CpgDotFileCreator
import de.tubs.cs.ias.cpg.slicing.ExportAndCpgTestingUtility
import de.tubs.cs.ias.cpg.slicing.algorithms.util.implicits.OneableIterableOnce
import de.tubs.cs.ias.cpg.slicing.representation.{ProgramSlice, SliceEdgeTypes, SliceNode}
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IntraFunctionSlicingTest extends AnyWordSpec with Matchers with PHPVersions with ExportAndCpgTestingUtility {


  implicit val version: PHPVersion.Value = PHPVersion.V8

  s"ProgramSlice with PHP $version intra function" should {
    "work for a trivial two step program" in new CpgFromCodeTestFixture(
      """$x = "test";
        |echo $x;
        |""".stripMargin, configFile = Some("cpg.conf")
    ) {
      val starts: List[Call] = getCallNodes(cpg, "ECHO.*")
      starts.length shouldBe 1
      //new CpgDotFileCreator(cpg.graph).show()
      val slice: ProgramSlice = ProgramSlice.slice(starts.head)
      slice.getNodes.size shouldBe 2
      slice.getEdges(SliceEdgeTypes.CONTROL_DEPENDENCE).size shouldBe 0
      slice.getEdges(SliceEdgeTypes.DATA_DEPENDENCE).size shouldBe 1
      slice.getEdges(SliceEdgeTypes.DATA_DEPENDENCE).head.in() shouldBe slice.getNodes("ECHO.*".r).head
    }
    "work for program with trivial if only" in new CpgFromCodeTestFixture(
      """$x = "other";
        |if ($probe)
        |    $x = "test";
        |echo $x;
        |""".stripMargin, configFile = Some("cpg.conf")
    ) {
      val echo :: test :: cond :: other :: Nil =
        getCallNodes(cpg, "ECHO.*", "ASS.*test.*", "J.*", "ASS.*other.*")
      val slice: ProgramSlice = ProgramSlice.slice(echo)
      //slice.showGraph()
      slice.getNodes.size shouldBe 4
      val secho :: stest :: scond :: sother :: Nil = slice.getNodes(echo, test, cond, other)
      secho.in(SliceEdgeTypes.DATA_DEPENDENCE).toSet shouldBe Set(stest, sother)
      secho.in(SliceEdgeTypes.CONTROL_DEPENDENCE).toSet shouldBe Set()
      stest.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List(scond)
    }
    "work for program with trivial if that converges on initial value" in new CpgFromCodeTestFixture(
      """$x = "other";
        |if ($x == "42") {
        |    $x = "test";
        |}
        |echo $x;
        |""".stripMargin, configFile = Some("cpg.conf")
    ) {
      val echo :: atest :: jmp :: e42 :: aother :: Nil: List[Call] =
        getCallNodes(cpg, "ECHO.*", "ASS.*test.*", "JM.*", "T2 = IS_EQUAL.*42.*", "ASS.*other.*")
      val slice: ProgramSlice = ProgramSlice.slice(echo)
      //slice.showGraph()
      val secho :: satest :: sjmp :: se42 :: saother :: Nil = slice.getNodes(echo, atest, jmp, e42, aother)
      slice.getNodes.size shouldBe 6
      secho.in(SliceEdgeTypes.DATA_DEPENDENCE).toSet shouldBe Set(satest, saother)
      satest.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List(sjmp)
      sjmp.in(SliceEdgeTypes.DATA_DEPENDENCE) shouldBe List(se42)
      //slice.showGraph()
    }
    "work for program with sequential control dependency" in new CpgFromCodeTestFixture(
      """$x = $_GET['test'];
        |if($x == "zz")
        |   $y = "43";
        |if ($y == "42")
        |    $x = $x;
        |echo $x;
        |""".stripMargin, configFile = Some("cpg.conf")
    ) {
      val echo :: axtest :: jmpT7 :: ay43 :: jmpT5 :: axget :: Nil =
        getCallNodes(cpg, "ECHO.*", "ASS.*x.*x.*", "JMP.*T7.*", "ASS.*y.*43.*", "JMP.*T5.*", "ASS.*x.*T.*")
      val slice: ProgramSlice = ProgramSlice.slice(echo)
      val secho :: saxtest :: sjmpT7 :: say43 :: sjmpT5 :: saxget :: Nil =
        slice.getNodes(echo, axtest, jmpT7, ay43, jmpT5, axget)
      secho.in(SliceEdgeTypes.DATA_DEPENDENCE).toSet shouldBe Set(saxtest, saxget)
      saxtest.in(SliceEdgeTypes.DATA_DEPENDENCE) shouldBe List(saxget)
      secho.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List()
      saxtest.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List(sjmpT7)
      say43.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List(sjmpT5)
    }
    "work with surfer slicing issue code" in new CpgFromCodeTestFixture(
      """$url = $_POST['url'];
        |$content;
        |if ($url) {
        |    print("Getting " . $url. 'for you..');
        |    $content = file_get_contents($url);
        |} else{
        |    $content = "Nothing to show right now.";
        |}
        |print($content."</body>");
        |""".stripMargin, configFile = Some("cpg.conf")
    ) {
      val sink :: sendVar :: jmp :: ass :: Nil = getCallNodesByName(cpg, "file_get_contents", "SEND_VAR.*", "JMP.*", "ASS.*")
      val slice: ProgramSlice = ProgramSlice.slice(sink)
      val ssink :: ssendVar :: sjmp :: sass :: Nil =
        slice.getNodes(sink, sendVar, jmp, ass)
      //slice.showGraph()

      sjmp.code shouldBe "JMPZ CV($url) int(12)"
      sjmp.outE(SliceEdgeTypes.CONTROL_DEPENDENCE).map(_.getAttribute("fallthrough").get shouldBe "true")
      ssink.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List(sjmp)
      ssink.in(SliceEdgeTypes.DATA_DEPENDENCE) shouldBe List(ssendVar)
      ssendVar.in(SliceEdgeTypes.DATA_DEPENDENCE) shouldBe List(sass)
      ssendVar.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List(sjmp)
    }
    "not give me an control dependency edge for a previous if if unrelated" in new CpgFromCodeTestFixture(
      """$test = "value";
        |if($test == "other") {
        |   $wayne = "vulture";
        |}
        |echo $test;
        |""".stripMargin
    ) {
      val sink :: Nil: List[Call] = getCallNodes(cpg, "ECHO.*")
      val slice: ProgramSlice = ProgramSlice.slice(sink)
      val ssink :: Nil =
        slice.getNodes(sink)
      //slice.showGraph()
      ssink.in(SliceEdgeTypes.CONTROL_DEPENDENCE) shouldBe List()
    }
    "set fallthrough label correctly" in new CpgFromCodeTestFixture(
      """
        |$x = "x";
        |if($x != 1) {
        |   $y = $x."foo";
        |}else{
        |   $y = $x;
        |}
        |echo $y;
        |""".stripMargin)(PHPVersion.V8) {
      val sink :: ass :: concat :: jmp :: Nil =
        getCallNodesByName(cpg, "ECHO.*", "ASS.*", "CONCAT.*", "JMP.*")
      val slice: ProgramSlice = ProgramSlice.slice(sink)
      val _ :: _ :: _ :: sjmp :: Nil = slice.getNodes(sink, ass, concat, jmp)
      sjmp.code should startWith("JMPZ ")
      sjmp.outE(SliceEdgeTypes.CONTROL_DEPENDENCE)
        .filter(_.in().code == "ASSIGN CV($y) CV($x)").one.getAttribute("fallthrough") shouldBe Some("false")
      sjmp.outE(SliceEdgeTypes.CONTROL_DEPENDENCE)
        .filterNot(_.in().code.startsWith("ASSIGN ")).map(_.getAttribute("fallthrough")).toSet shouldBe Set(Some("true"))
    }
    "or" in new CpgFromCodeTestFixture(
      """$x = 19;
        |if($x == 33) {
        |    $wayne = $_GET['test'];
        |    file_exists($wayne);
        |}
        |if($x == 42) {
        |    $wayne = $_GET['test'];
        |    echo $wayne;
        |}
        |""".stripMargin
    )(PHPVersion.V8) {
      val sink :: Nil: List[Call] = getCallNodes(cpg, "INIT.*")
      val slice: ProgramSlice = ProgramSlice.slice(sink)
      slice.getNodes("JMP.*".r).map(_.name).toSet shouldBe Set("JMPZ")
      slice.getEdges(SliceEdgeTypes.CONTROL_DEPENDENCE).map(_.getAttribute("fallthrough")) shouldBe Set(Some("true"))

      val sinkb :: Nil: List[Call] = getCallNodes(cpg, "INIT.*")
      val sliceb: ProgramSlice = ProgramSlice.slice(sinkb)
      sliceb.getNodes("JMP.*".r).map(_.name).toSet shouldBe Set("JMPZ")
      sliceb.getEdges(SliceEdgeTypes.CONTROL_DEPENDENCE).map(_.getAttribute("fallthrough")) shouldBe Set(Some("true"))
    }
    "ASSIGN_DIM is missing DATA_DEPENDENCY edge to used global" in new CpgFromCodeTestFixture(
      """$x = $array;
        |$x['foo'] = 'bar';
        |extract($x);
        |""".stripMargin
    ) {
      val sink :: Nil : List[Call] = getCallNodesByName(cpg,"extract")
      val slice : ProgramSlice = ProgramSlice.slice(sink)
      //slice.showGraph()
      val assign_dim :: Nil : List[SliceNode] = slice.getNodes("ASSIGN_DIM.*".r)
      assign_dim.in(SliceEdgeTypes.DATA_DEPENDENCE).length shouldBe 2
    }
    "ADD_ARRAY_ELEMENT missing data dependency edge" in new CpgFromCodeTestFixture(
      """$x = [
        |   'f' => f,
        |   'g' => $_GET[1],
        |   'h' => 2,
        |   'j' => 2,
        |];
        |extract($x);
        |""".stripMargin
    ) {
      //new CpgDotFileCreator(cpg.graph).show()
      val sink :: Nil: List[Call] = getCallNodesByName(cpg, "extract")
      val slice: ProgramSlice = ProgramSlice.slice(sink)
      //slice.showGraph()
    }
  }
}
