package advent2024

import scala.annotation.switch

case object Day07 extends Day:
  type Input = (Long, List[Int])

  enum Op:
    case Add, Mul, Cat

    private def eval(i: Long, j: Int) = (this: @switch) match
      case Add => i + j
      case Mul => i * j
      case Cat => i * Op.orderOf(j) + j

    override def toString: String = this match
      case Add => "+"
      case Mul => "*"
      case Cat => "||"

  private object Op:
    private val pow10 =
      Array(0, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000)

    private def orderOf(m: Int) =
      pow10(pow10.indexWhere(m < _))

    // assumes only positive measures
    def areCalibrated(ops: Seq[Op])(test: Long, measures: List[Int]): Boolean =
      def check(acc: Long, measures: List[Int]): Boolean = measures match
        case Nil => acc == test
        case _ if acc > test => false
        case m :: ms => ops.exists(op => check(op.eval(acc, m), ms))

      measures match
        case m :: ms => check(m, ms)
        case Nil => false

  private def calibratedSum(input: Iterator[Input])(ops: Op*) =
    input.filter(Op.areCalibrated(ops)).map(_._1).sum

  def parse(line: String): Parsed[Input] = line.split(':') match
    case Array(test, rest) =>
      val measures = rest.trim.split(' ').iterator.map(_.toInt).toList
      Either.cond(measures.forall(_ > 0), test.toLong -> measures, "non-positive measures")
    case _ =>
      Left("malformed test")

  def part1(input: Iterator[Input]): Long =
    calibratedSum(input)(Op.Add, Op.Mul)

  def part2(input: Iterator[Input]): Long =
    calibratedSum(input)(Op.values*)

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
