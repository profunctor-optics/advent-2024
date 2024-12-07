package advent2024

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
    var enabled = true
    var sum = 0L
    val matches = memory.flatMap(mulCond.findAllMatchIn)
    while matches.hasNext do
      val m = matches.next()
      m.matched match
        case "do()" => enabled = true
        case "don't()" => enabled = false
        case _ => if enabled then sum += multiply(m)
    sum

  def part1(file: String): Long =
    withResource(file)(compute)

  def part2(file: String): Long =
    withResource(file)(computeCond)

  def main(args: Array[String]): Unit =
    println(part1("day03.txt"))
    println(part2("day03.txt"))
