package twenty.twentyfive

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {
  val sampleInput =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin
      .split("\n")
      .toVector
      .map(
        _.trim
          .splitAt(1)
      )
      .map { case (s, n) =>
        (s.head, n.toInt)
      }

  val input = {
    val source = Source.fromFile("src/resources/input/25Day1.txt")
    try {
      source
        .getLines()
        .toVector
        .map(
          _.trim
            .splitAt(1)
        )
        .map { case (s, n) =>
          (s.head, n.toInt)
        }
    } finally {
      source.close()
    }
  }

  @tailrec
  def roundUpPos(pos: Int): Int = {
    val newpos = if (pos < 0) {
      pos + 100
    } else if (pos >= 100) {
      pos - 100
    } else {
      pos
    }
    if (newpos >= 0 && newpos <= 99) newpos else roundUpPos(newpos)
  }

  @tailrec
  def result1(
      curPos: Int,
      moves: Vector[(Char, Int)],
      zeros: Int
  ): (Int, Int) = {
    if (moves.isEmpty) {
      (curPos, zeros)
    } else {
      val newPos = moves.head match {
        case ('L', n) => roundUpPos(curPos - n)
        case ('R', n) => roundUpPos(curPos + n)
      }
      val updatedzero = if (newPos == 0) zeros + 1 else zeros
      result1(newPos, moves.tail, updatedzero)
    }
  }

  println(result1(50, input, 0))


  @tailrec
  def roundUpPosCountZeroClicks(pos: Int, zeroClicks: Int): (Int, Int) = {
    val (newpos, clicks) = if (pos < 0) {
      (pos + 100, zeroClicks+1)
    } else if (pos >= 100) {
      (pos - 100, zeroClicks+1)
    } else {
      (pos, zeroClicks)
    }

    println(s"newpos - $newpos - $zeroClicks")

    if (newpos >= 0 && newpos <= 99) {
      (newpos, clicks)
    }
    else roundUpPosCountZeroClicks(newpos, clicks)
  }

  // this part2 result gives exactly one extra count, classic off-by-one error.
  @tailrec
  def result2(
      curPos: Int,
      moves: Vector[(Char, Int)],
      zeros: Int
  ): (Int, Int) = {
    if (moves.isEmpty) {
      (curPos, zeros)
    } else {
      println(s"moves - ${moves.head}")
      val newPos = moves.head match {
        case ('L', n) => roundUpPosCountZeroClicks(curPos - n, 0)
        case ('R', n) => roundUpPosCountZeroClicks(curPos + n, 0)
      }
      result2(newPos._1, moves.tail, newPos._2+zeros)
    }
  }

  println(s"Result2 ${result2(50, input, 0)}")
  }

