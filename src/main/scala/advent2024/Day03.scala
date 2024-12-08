package advent2024

import scala.annotation.tailrec
import scala.util.matching.Regex

case object Day03 extends Day:
  type Input = String

  private val mul = """mul\((\d{1,3}),(\d{1,3})\)""".r
  private val mulCond = s"do\\(\\)|don't\\(\\)|$mul".r

  private def multiply(m: Regex.Match) =
    m.group(1).toLong * m.group(2).toLong

  def compute(memory: Iterator[String]): Long =
    memory.flatMap(mul.findAllMatchIn).map(multiply).sum

  def computeCond(memory: Iterator[String]): Long =
    val matches = memory.flatMap(mulCond.findAllMatchIn)
    @tailrec def loop(enabled: Boolean, sum: Long): Long =
      matches.nextOption() match
        case Some(m) if m.matched == "do()" => loop(enabled = true, sum)
        case Some(m) if m.matched == "don't()" => loop(enabled = false, sum)
        case Some(m) if enabled => loop(enabled, sum + multiply(m))
        case Some(_) => loop(enabled, sum)
        case None => sum
    loop(true, 0L)

  def parse(line: String): Parsed[String] =
    Right(line)

  def run(): Unit =
    printPart(1)(withFile(compute))
    printPart(2)(withFile(computeCond))
