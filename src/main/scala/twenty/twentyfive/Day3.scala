package twenty.twentyfive

import twenty.InputReader


object Day3 extends App{
  val inputParser: String => Seq[Int] = {
    (line: String) =>
      line.map{ x =>
        x.toString.toInt
      }
  }

  val sampleInput = InputReader.readAndFormat(
    "src/resources/input/25Day3Sample.txt",
    inputParser
  )

  val input = InputReader.readAndFormat(
    "src/resources/input/25Day3.txt",
    inputParser
  )

  def maxTwoDigitBank(bank: Seq[Int]): Long = {
    (0 until bank.length-1).map{ i =>
      (i+1 until bank.length).map{ j=>
        s"${bank(i)}${bank(j)}".toLong
      }.max
    }.max
  }

  def part1(input: Seq[Seq[Int]]):Long =
    input.map(maxTwoDigitBank).sum

// part1
  println(part1(sampleInput))
  println(part1(input))


  // part2
  def maxNBatteryJoltage(bank: Seq[Int], reqBatteryCount: Int): Seq[Int] = {
    val firstBatteryIdx = (0 until (bank.length-reqBatteryCount+1))
      .maxBy(i => bank(i))

    val rest =
      if (reqBatteryCount > 1) {
        maxNBatteryJoltage(bank.slice(firstBatteryIdx + 1, bank.length), reqBatteryCount - 1)
      } else {
        Nil
      }
    bank(firstBatteryIdx) +: rest
  }

  def part2(input: Seq[Seq[Int]]):Long =
    input.map(i => maxNBatteryJoltage(i,12).mkString.toLong).sum


  println(part2(sampleInput))
  println(part2(input))
}
