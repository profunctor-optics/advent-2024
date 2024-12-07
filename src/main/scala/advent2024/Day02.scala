package advent2024

import scala.annotation.tailrec

object Day02 extends Day:
  type Input = Array[Int]
  private type Monotonic = (Input, Int, Int) => Boolean
  private val safeRange = 1 to 3

  private val increasing: Monotonic = (report, i, j) =>
    val indices = report.indices
    !indices.contains(i) || !indices.contains(j)
    || safeRange.contains(report(j) - report(i))

  private val decreasing: Monotonic =
    (report, i, j) => increasing(report, j, i)

  def parse(line: String): Option[Input] =
    Some(line.split("\\s+").map(_.toInt))

  private def safeReport(report: Input) =
    def isSafely(monotonic: Monotonic) =
      report.indices.forall(i => monotonic(report, i, i + 1))
    isSafely(increasing) || isSafely(decreasing)

  def safeReport1(report: Input): Boolean =
    def isSafely(monotonic: Monotonic) =
      @tailrec def isSafe(i: Int, rm: Boolean): Boolean =
        if i >= report.length then true
        else if monotonic(report, i, i + 1) then isSafe(i + 1, rm)
        else if rm then false // prefer to remove the second level
        else if monotonic(report, i, i + 2) then isSafe(i + 2, rm = true)
        else if monotonic(report, i - 1, i + 1) then isSafe(i + 1, rm = true)
        else false
      isSafe(0, rm = false)
    isSafely(increasing) || isSafely(decreasing)

  def part1(file: String): Int =
    withResource(file)(_.count(safeReport))

  def part2(file: String): Int =
    withResource(file)(_.count(safeReport1))

  def main(args: Array[String]): Unit =
    println(part1("day02.txt"))
    println(part2("day02.txt"))
