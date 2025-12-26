import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.*
import twenty.InputReader

import scala.Seq

class InputReaderTest extends AnyFreeSpec with should.Matchers{

  "should parse input" - {
    val parseFn: String => (Int, Int) = (x: String) =>{
      val a = x.split(" ")
      (a(0).toInt, a(1).toInt)
    }
    val file = "src/test/resources/testreader.txt"
    val output = InputReader.readAndFormat(file, parseFn)
    val expectedOutput = Seq((10,20), (30,50), (33,69), (71,42), (31,51))

    output shouldBe expectedOutput
  }
}
