package advent2024

import scala.annotation.tailrec

case object Day13 extends Day:
  type Input = IArray[Char]

  final case class Rational(num: Int, den: Int):
    def +(that: Rational): Rational =
      Rational(this.num * that.den + that.num * this.den, this.den * that.den)
    def -(that: Rational): Rational =
      Rational(this.num * that.den - that.num * this.den, this.den * that.den)
    def *(that: Rational): Rational =
      Rational(this.num * that.num, this.den * that.den)
    def /(that: Rational): Rational =
      Rational(this.num * that.den, this.den * that.num)

  object Rational:
    @tailrec private def gcd(x: Int, y: Int): Int =
      if y == 0 then x else gcd(y, x % y)

    def apply(num: Int, den: Int = 1): Rational =
      val norm = gcd(num, den)
      new Rational(num / norm, den / norm)

  def parse(line: String): Parsed[Input] =
    Right(IArray.unsafeFromArray(line.toCharArray))

  def part1(input: Iterator[Input]): Long = ???
  def part2(input: Iterator[Input]): Long = ???

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
