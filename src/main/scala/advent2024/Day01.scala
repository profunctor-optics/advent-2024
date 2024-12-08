package advent2024

case object Day01 extends Day:
  type Input = (Int, Int)

  private def totalDistance(locations: Iterator[Input]) =
    val (left, right) = locations.toVector.unzip
    left.sorted.iterator.zip(right.sorted.iterator).map((l, r) => (r - l).abs.toLong).sum

  private def similarityScore(locations: Iterator[Input]) =
    val (left, right) = locations.toVector.unzip
    val frequencies = right.groupMapReduce(identity)(_ => 1)(_ + _)
    left.iterator.map(loc => loc.toLong * frequencies.getOrElse(loc, 0)).sum

  def parse(line: String): Parsed[Input] = line.split("\\s+") match
    case Array(left, right) => Right(left.toInt -> right.toInt)
    case _ => Left("expected two locations")

  def part1(file: String): Long =
    withResource(file)(totalDistance)

  def part2(file: String): Long =
    withResource(file)(similarityScore)

  def run(file: String): Unit =
    println(part1(file))
    println(part2(file))
