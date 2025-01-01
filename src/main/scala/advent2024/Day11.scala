package advent2024

import scala.collection.mutable

case object Day11 extends Day:
  type Input = IArray[Long]

  private val pow10 =
    val pow10 = Iterator.iterate(1L)(_ * 10).takeWhile(_ > 0).toArray
    pow10(0) = 0
    IArray.unsafeFromArray(pow10)

  private def numDigits(stone: Long) =
    pow10.indexWhere(stone < _)

  private def numStones(memo: IArray[mutable.LongMap[Long]], steps: Int)(stone: Long): Long =
    def split: Long =
      val steps1 = steps - 1
      if stone == 0 then return numStones(memo, steps1)(1)
      val digits = numDigits(stone)
      if digits % 2 == 1 then return numStones(memo, steps1)(stone * 2024)
      val half = pow10(digits / 2)
      val left = numStones(memo, steps1)(stone % half)
      val right = numStones(memo, steps1)(stone / half)
      left + right

    if steps == 0 then 1
    else memo(steps).getOrElseUpdate(stone, split)

  private def numStones(steps: Int)(input: Input): Long =
    val memo = IArray.fill(steps + 1)(mutable.LongMap.empty[Long])
    input.iterator.map(numStones(memo, steps)).sum

  def parse(line: String): Parsed[Input] =
    val stones = line.split(' ').map(_.toLong)
    Right(IArray.unsafeFromArray(stones))

  def part1(input: Iterator[Input]): Long =
    input.map(numStones(25)).sum

  def part2(input: Iterator[Input]): Long =
    input.map(numStones(75)).sum

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
