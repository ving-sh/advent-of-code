package twenty.twentyfive

import twenty.InputReader

import scala.annotation.tailrec

object Day4 extends App{
  val sampleInput = InputReader.readAndFormat(
    "src/resources/input/25Day4sample.txt",
    identity
  )

  val input = InputReader.readAndFormat(
    "src/resources/input/25Day4.txt",
    identity
  )

  val neighboringPos: Seq[(Int, Int)] = Seq((-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1))

  def part1(input: Seq[String]): Int = {
    val length = input.length
    val bredth = input(0).length

    def surroundingRollsCount(i: Int, j: Int): Int = {
      neighboringPos.map{(x,y) =>
        if (i+x < length && i+x >= 0) && (j+y < bredth && j+y >=0)  && (input(i+x)(j+y) == '@') then 1 else 0
      }.sum
    }

    List.tabulate(length, bredth) { (x, y) =>
      if (input(x)(y) == '@' && surroundingRollsCount(x, y) < 4) then {
        1
      } else 0
    }.flatten.sum
  }

  println(part1(sampleInput))
  println(part1(input))

  def part2(input: Seq[String]): Int = {
    val length = input.length
    val bredth = input(0).length

    @tailrec
    def calGrid(grid: Seq[String], result: Int): Int = {
      def surroundingRollsCount(i: Int, j: Int): Int = {
        neighboringPos.map { (x, y) =>
          if (i + x < length && i + x >= 0) && (j + y < bredth && j + y >= 0) && (grid(i + x)(j + y) == '@') then 1 else 0
        }.sum
      }

      var updatedGrid = grid
      val rolls = List.tabulate(length, bredth) { (x, y) =>
        if (grid(x)(y) == '@' && surroundingRollsCount(x, y) < 4) then {
          updatedGrid = updatedGrid.updated(x, updatedGrid(x).updated(y, 'X'))
          1
        } else 0
      }.flatten.sum

      if rolls > 0 then
        calGrid(updatedGrid, result+rolls)
      else
        result
    }
    calGrid(input, 0)
  }

  println("----Part2: Answer----")
  println(part2(sampleInput))
  println(part2(input))

}
