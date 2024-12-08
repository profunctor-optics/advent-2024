package advent2024

case object Day08 extends Day:
  type Input = IArray[Char]

  private class Puzzle(map: IArray[IArray[Char]]):
    private val row = map.indices
    private val col = map.head.indices
    private val blank = Set('.', '#')
    private val antennas = (for
      i <- row
      j <- col
      if !blank(map(i)(j))
    yield Pos(i, j)).groupBy(_.frequency)

    def this(lines: Iterator[Input]) =
      this(IArray.from(lines))

    case class Pos(i: Int, j: Int):
      def frequency: Char = map(i)(j)
      def +(that: Pos): Pos = Pos(this.i + that.i, this.j + that.j)
      def -(that: Pos): Pos = Pos(this.i - that.i, this.j - that.j)

      private def inBounds: Boolean =
        row.contains(i) && col.contains(j)

      def interfering(that: Pos): Iterator[Pos] =
        val delta = that - this
        Iterator(this - delta, that + delta).filter(_.inBounds)

      def harmonic(that: Pos): Iterator[Pos] =
        val delta = that - this
        val decr = Iterator.iterate(this)(_ - delta)
        val incr = Iterator.iterate(that)(_ + delta)
        decr.takeWhile(_.inBounds) ++ incr.takeWhile(_.inBounds)

    private def antiNodes(by: (Pos, Pos) => Iterator[Pos])(compatible: Seq[Pos]): Iterator[Pos] =
      compatible.combinations(2).flatMap(pair => by(pair.head, pair.last))

    def countAntiNodes(by: (Pos, Pos) => Iterator[Pos]): Int =
      antennas.valuesIterator.flatMap(antiNodes(by)).toSet.size

  def parse(line: String): Parsed[Input] =
    Right(IArray.unsafeFromArray(line.toCharArray))

  def part1(input: Iterator[Input]): Int =
    Puzzle(input).countAntiNodes(_.interfering(_))

  def part2(input: Iterator[Input]): Int =
    Puzzle(input).countAntiNodes(_.harmonic(_))

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
