package advent2024

object Day01 extends Day[(Int, Int)]:
  def parse(line: String): Option[Input] = line.split("\\s+") match
    case Array(left, right) => Some((left.toInt, right.toInt))
    case _ => None

  private def totalDistance(locations: Iterator[Input]) =
    val (left, right) = locations.toArray.unzip
    left.sortInPlace()
    right.sortInPlace()
    left.iterator.zip(right.iterator).map((l, r) => (r - l).abs.toLong).sum

  private def similarityScore(locations: Iterator[Input]) =
    val (left, right) = locations.toArray.unzip
    val frequencies = right.groupMapReduce(identity)(_ => 1)(_ + _)
    left.iterator.map(loc => loc.toLong * frequencies.getOrElse(loc, 0)).sum

  def part1(file: String): Long =
    withResource(file)(totalDistance)

  def part2(file: String): Long =
    withResource(file)(similarityScore)

  def main(args: Array[String]): Unit =
    println(part1("day01.txt"))
    println(part2("day01.txt"))
