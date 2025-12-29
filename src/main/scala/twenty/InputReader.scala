package twenty

import scala.io.Source

object InputReader {
  def read(file: String): Seq[String] = {
    val source = Source.fromFile(file)
    try {
      source
        .getLines()
        .toSeq
    } finally {
      source.close()
    }
  }

  def readAndFormat[A](file: String, fn: String => A): Seq[A] = {
    val source = Source.fromFile(file)
    try {
      source
        .getLines()
        .toSeq
        .map { line =>
          fn(line.trim)
        }
    } finally {
      source.close()
    }

  }

}
