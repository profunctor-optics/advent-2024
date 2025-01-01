package advent2024

import scala.reflect.ClassTag

case object Day10 extends Day:
  type Input = IArray[Int]
  final case class Pos(i: Int, j: Int)

  trait Measure[T]:
    def empty: T
    def single(pos: Pos): T
    extension (x: T)
      def +(y: T): T
      def measure: Int

  given Measure[Set[Pos]] with
    def empty: Set[Pos] = Set.empty
    def single(pos: Pos): Set[Pos] = Set(pos)
    extension (x: Set[Pos])
      def +(y: Set[Pos]): Set[Pos] = x | y
      def measure: Int = x.size

  given Measure[Int] with
    def empty: Int = 0
    def single(pos: Pos): Int = 1
    extension (x: Int)
      def +(y: Int): Int = x + y
      def measure: Int = x

  private def trailSum[T: ClassTag](map: IArray[IArray[Int]])(using T: Measure[T]): Int =
    if map.isEmpty then return 0
    val n = map.length
    val m = map.head.length
    val positions = Array.fill(10)(List.empty[Pos])
    val trails = Array.ofDim[T](n, m)

    for i <- map.indices do
      for j <- map(i).indices do
        val h = map(i)(j)
        if h < positions.length
        then positions(h) ::= Pos(i, j)

    def reachable(pos: Pos, h: Int): T =
      if h == 9 then return T.single(pos)
      val Pos(i, j) = pos
      val h1 = h + 1
      var reachable = T.empty
      if i > 0 && map(i - 1)(j) == h1 then reachable += trails(i - 1)(j)
      if j > 0 && map(i)(j - 1) == h1 then reachable += trails(i)(j - 1)
      if i < n - 1 && map(i + 1)(j) == h1 then reachable += trails(i + 1)(j)
      if j < m - 1 && map(i)(j + 1) == h1 then reachable += trails(i)(j + 1)
      reachable

    for h <- positions.indices.reverse
    do for p <- positions(h) do trails(p.i)(p.j) = reachable(p, h)
    positions.head.iterator.map(p => trails(p.i)(p.j).measure).sum

  def parse(line: String): Parsed[Input] =
    val heights = line.toCharArray.map: c =>
      if c.isDigit then c.asDigit else 10
    Right(IArray.unsafeFromArray(heights))

  def part1(input: Iterator[Input]): Int =
    trailSum[Set[Pos]](IArray.from(input))

  def part2(input: Iterator[Input]): Int =
    trailSum[Int](IArray.from(input))

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
