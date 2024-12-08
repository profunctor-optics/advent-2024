package advent2024

import scala.collection.immutable.{BitSet, IntMap}

case object Day05 extends Day:
  type Input = Either[(Int, Int), Array[Int]]

  private class Printer(var after: IntMap[BitSet]):
    def this() = this(IntMap.empty)

    private def isAfter(p: Int) =
      after.getOrElse(p, BitSet.empty)

    private def add(p: Int, q: Int): Unit =
      after += p -> isAfter(p).incl(q)

    // bubble sort
    private def sortedMid(queue: Array[Int], fix: Boolean): Long =
      def swap(i: Int, j: Int) =
        val tmp = queue(i)
        queue(i) = queue(j)
        queue(j) = tmp
        true

      val length = queue.length
      if length == 1 then return queue.head
      val inv = for
        j <- queue.indices.iterator
        i <- 0 until j
        if isAfter(queue(j))(queue(i))
      yield fix && swap(i, j)

      val fixed = if fix then inv.count(identity) > 0 else inv.isEmpty
      if fixed then queue(length / 2).toLong else 0L

    def sortedSum(input: Iterator[Input], fix: Boolean): Long =
      val (pairs, queues) = input.span(_.isLeft)
      for case Left((p, q)) <- pairs do add(p, q)
      (for case Right(queue) <- queues yield sortedMid(queue, fix)).sum

  def parse(line: String): Parsed[Input] = line.split('|') match
    case Array(lt, gt) =>
      Right(Left(lt.toInt -> gt.toInt))
    case Array(line) =>
      val queue = line.split(',').map(_.toInt)
      Either.cond(queue.length % 2 == 1, Right(queue), "even queue")
    case _ =>
      Left("malformed rule")

  def part1(input: Iterator[Input]): Long =
    Printer().sortedSum(input, fix = false)

  def part2(input: Iterator[Input]): Long =
    Printer().sortedSum(input, fix = true)

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
