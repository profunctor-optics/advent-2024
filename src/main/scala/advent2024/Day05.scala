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

    def sortedSum(fix: Boolean)(input: Iterator[Input]): Long =
      val (pairs, queues) = input.span(_.isLeft)
      for case Left((p, q)) <- pairs do add(p, q)
      (for case Right(queue) <- queues yield sortedMid(queue, fix)).sum

  def parse(line: String): Option[Input] = line.split('|') match
    case Array(lt, gt) =>
      Some(Left((lt.toInt, gt.toInt)))
    case Array(line) =>
      val queue = line.split(',').map(_.toInt)
      Option.when(queue.length % 2 == 1)(Right(queue))
    case _ =>
      None

  def part1(file: String): Long =
    withResource(file)(Printer().sortedSum(fix = false))

  def part2(file: String): Long =
    withResource(file)(Printer().sortedSum(fix = true))

  def run(file: String): Unit =
    println(part1(file))
    println(part2(file))
