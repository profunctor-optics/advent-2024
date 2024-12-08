package advent2024

case object Day01 extends Day:
  type Input = (Int, Int)

  def totalDistance(locations: Iterator[Input]): Long =
    val (left, right) = locations.toVector.unzip
    left.sorted.iterator.zip(right.sorted.iterator).map((l, r) => (r - l).abs.toLong).sum

  def similarityScore(locations: Iterator[Input]): Long =
    val (left, right) = locations.toVector.unzip
    val frequencies = right.groupMapReduce(identity)(_ => 1)(_ + _)
    left.iterator.map(loc => loc.toLong * frequencies.getOrElse(loc, 0)).sum

  def parse(line: String): Parsed[Input] = line.split("\\s+") match
    case Array(left, right) => Right(left.toInt -> right.toInt)
    case _ => Left("expected two locations")

  def run(): Unit =
    printPart(1)(withFile(totalDistance))
    printPart(2)(withFile(similarityScore))
