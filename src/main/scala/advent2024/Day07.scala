package advent2024

import scala.annotation.switch

object Day07 extends Day:
  type Input = (Long, Array[Int])

  enum Op:
    case Add, Mul, Concat

    private def eval(i: Long, j: Int): Long = (this: @switch) match
      case Add => i + j
      case Mul => i * j
      case Concat => i * Op.orderOf(j) + j

    override def toString: String = this match
      case Add => "+"
      case Mul => "*"
      case Concat => "||"

  private object Op:
    private val pow10 =
      Array(0, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000)

    private def orderOf(m: Int) =
      pow10(pow10.indexWhere(m < _))

    // assumes only positive measurements
    def evaluateTo(test: Long, measures: Array[Int])(ops: List[Op]): Boolean =
      val opsIter = ops.iterator
      val msIter = measures.iterator
      if !msIter.hasNext then return false
      var result = msIter.next().toLong
      while result <= test && opsIter.hasNext && msIter.hasNext
      do result = opsIter.next().eval(result, msIter.next())
      result == test && opsIter.isEmpty && msIter.isEmpty

  private def isCalibrated(possible: Seq[Op])(test: Long, measures: Array[Int]): Boolean =
    if measures.isEmpty then return false
    if measures.length == 1 then return measures.head == test
    val indices = 0 until measures.length - 2
    val initial = possible.iterator.map(_ :: Nil)
    val combinations = indices.foldLeft(initial): (ops, _) =>
      ops.flatMap(ops => possible.iterator.map(_ :: ops))
    combinations.exists(Op.evaluateTo(test, measures))

  private def calibratedSum(ops: Op*)(input: Iterator[Input]) =
    input.filter(isCalibrated(ops)).map(_._1).sum

  def parse(line: String): Option[Input] = line.split(':') match
    case Array(test, rest) =>
      val measurements = rest.trim.split(' ').map(_.toInt)
      Option.when(measurements.forall(_ > 0))(test.toLong -> measurements)
    case _ =>
      None

  def part1(file: String): Long =
    withResource(file)(calibratedSum(Op.Add, Op.Mul))

  def part2(file: String): Long =
    withResource(file)(calibratedSum(Op.values*))

  def main(args: Array[String]): Unit =
    println(part1("day07.txt"))
    println(part2("day07.txt"))
