package advent2024

case object Day12 extends Day:
  type Input = IArray[Char]

  private class Farm(plants: IArray[IArray[Char]]):
    private val row = plants.indices
    private val col = plants.head.indices
    private val plots = Array.ofDim[Int](row.end, col.end)
    private var plot = 0

    for i <- plants.indices do
      for j <- plants(i).indices do
        if plots(i)(j) == 0 then
          plot += 1
          val plant = plants(i)(j)
          fill(i, j, plant, plot)

    private val areas, perimeters, sides =
      Array.ofDim[Long](plot + 1)

    for i <- plots.indices do
      for j <- plots(i).indices do
        val plot = plots(i)(j)
        areas(plot) += 1
        perimeters(plot) += 4
        sides(plot) += corners(i, j, plot)
        if i > 0 && plots(i - 1)(j) == plot
        then perimeters(plot) -= 2
        if j > 0 && plots(i)(j - 1) == plot
        then perimeters(plot) -= 2

    def this(input: Iterator[Input]) =
      this(IArray.from(input))

    private def inPlot(i: Int, j: Int, plot: Int) =
      row.contains(i) && col.contains(j) && plots(i)(j) == plot

    private def fill(i: Int, j: Int, plant: Char, plot: Int): Unit =
      if inPlot(i, j, 0) && plants(i)(j) == plant then
        plots(i)(j) = plot
        fill(i - 1, j, plant, plot)
        fill(i, j - 1, plant, plot)
        fill(i + 1, j, plant, plot)
        fill(i, j + 1, plant, plot)

    private def corners(i: Int, j: Int, plot: Int) =
      var corners = 0
      val neighbours = for
        x <- i - 1 to i + 1
        y <- j - 1 to j + 1
      yield inPlot(x, y, plot)
      neighbours match
        case Seq(nw, n, ne, w, _, e, sw, s, se) =>
          if !n && !w then corners += 1
          if !n && !e then corners += 1
          if !s && !w then corners += 1
          if !s && !e then corners += 1
          if n && w && !nw then corners += 1
          if n && e && !ne then corners += 1
          if s && w && !sw then corners += 1
          if s && e && !se then corners += 1
      corners

    def fencePrice: Long =
      areas.iterator.zip(perimeters.iterator).map(_ * _).sum

    def discountedPrice: Long =
      areas.iterator.zip(sides.iterator).map(_ * _).sum

  def parse(line: String): Parsed[Input] =
    Right(IArray.unsafeFromArray(line.toCharArray))

  def part1(input: Iterator[Input]): Long =
    Farm(input).fencePrice

  def part2(input: Iterator[Input]): Long =
    Farm(input).discountedPrice

  def run(): Unit =
    printPart(1)(withFile(part1))
    printPart(2)(withFile(part2))
