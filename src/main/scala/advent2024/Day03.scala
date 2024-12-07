package advent2024

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day03 extends Day:
  type Input = String
  private val mul = """mul\((\d{1,3}),(\d{1,3})\)""".r
  private val mulCond = s"do\\(\\)|don't\\(\\)|$mul".r

  def parse(line: String): Option[String] =
    Some(line)

  private def multiply(m: Regex.Match) =
    m.group(1).toLong * m.group(2).toLong

  def compute(memory: Iterator[String]): Long =
    memory.flatMap(mul.findAllMatchIn).map(multiply).sum

  def computeCond(memory: Iterator[String]): Long =
    val matches = memory.flatMap(mulCond.findAllMatchIn)
    @tailrec def loop(enabled: Boolean, sum: Long): Long =
      matches.nextOption() match
        case Some(m) if m.matched == "do()" => loop(true, sum)
        case Some(m) if m.matched == "don't()" => loop(false, sum)
        case Some(m) if enabled => loop(enabled, sum + multiply(m))
        case Some(_) => loop(enabled, sum)
        case None => sum
    loop(true, 0L)

  def part1(file: String): Long =
    withResource(file)(compute)

  def part2(file: String): Long =
    withResource(file)(computeCond)

  def main(args: Array[String]): Unit =
    println(part1("day03.txt"))
    println(part2("day03.txt"))
