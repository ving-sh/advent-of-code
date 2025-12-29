package twenty.twentyfive

import twenty.InputReader

object Day2 extends App{

  val inputParser: String => Seq[(Long, Long)] = {
    (s: String) =>
      s.split(",").toSeq.map { i =>
        val array = i.split("-")
        val (first, second) = (array(0).toLong, array(1).toLong)
        (first, second)
      }
  }

  val sampleInput = InputReader.readAndFormat(
    "src/resources/input/25Day2Sample.txt",
    inputParser
  ).flatten

  val input = InputReader.readAndFormat(
    "src/resources/input/25Day2.txt",
    inputParser
  ).flatten

  def part1(input: Seq[(Long, Long)]): Long = {
    val ranges = input.map {
      case (a, b) => a to b
    }

    def invalid(i: Long): Boolean = {
      val s = i.toString
      val (a, b) = s.splitAt(s.length / 2)
      a == b
    }

    ranges
      .iterator
      .flatten
      .filter(invalid)
      .sum
  }

  println(part1(sampleInput))
  println(part1(input))

  def part2(input: Seq[(Long, Long)]): Long = {
    val ranges = input.map {
      case (a, b) => a to b
    }

    def invalid2(i: Long): Boolean = {
      val s = i.toString
      val n = s.length
      val divisors = (1 to n/2).filter(n%_ == 0)
      divisors.exists{d =>
        s.take(d)* (n/d) == s
      }
    }

    ranges
      .iterator
      .flatten
      .filter(invalid2)
      .sum
  }

  println(part2(sampleInput))
  println(part2(input))
}
