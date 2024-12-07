package advent2024

import scala.collection.immutable.{BitSet, IntMap}

object Day05 extends Day:
  type Input = Either[(Int, Int), Array[Int]]

  private class State(var after: IntMap[BitSet]):
    def this() = this(IntMap.empty)

    private def isAfter(p: Int) =
      after.getOrElse(p, BitSet.empty)

    private def add(p: Int, q: Int): Unit =
      after += p -> isAfter(p).incl(q)

    // bubble sort
    private def sortedMid(queue: Array[Int], fix: Boolean): Long =
      def swap(i: Int, j: Int): Boolean =
        val tmp = queue(i)
        queue(i) = queue(j)
        queue(j) = tmp
        true

      val length = queue.length
      if length == 1 then return queue.head
      assert(length % 2 == 1, "even queue")

      val inv = for
        j <- queue.indices.iterator
        i <- 0 until j
        if isAfter(queue(j))(queue(i))
      yield swap(i, j)

      val fixed = if fix then inv.hasNext && inv.forall(identity) else inv.isEmpty
      if fixed then queue(length / 2).toLong else 0L

    def sortedSum(fix: Boolean)(input: Iterator[Input]): Long =
      after = IntMap.empty
      val (pairs, queues) = input.span(_.isLeft)
      for case Left((p, q)) <- pairs do add(p, q)
      (for case Right(queue) <- queues yield sortedMid(queue, fix)).sum

  def parse(line: String): Option[Input] = line.split('|') match
    case Array(lt, gt) => Some(Left((lt.toInt, gt.toInt)))
    case Array(line) => Some(Right(line.split(',').map(_.toInt)))
    case _ => None

  def part1(file: String): Long =
    withResource(file)(State().sortedSum(fix = false))

  def part2(file: String): Long =
    withResource(file)(State().sortedSum(fix = true))

  def main(args: Array[String]): Unit =
    println(part1("day05.txt"))
    println(part2("day05.txt"))
