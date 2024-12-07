package advent2024

import scala.collection.mutable

object Day06 extends Day:
  type Input = Array[Point]

  enum Point(private val char: Char):
    case Blank extends Point('.')
    case Blocked extends Point('#')
    case Left extends Point('<')
    case Up extends Point('^')
    case Right extends Point('>')
    case Down extends Point('v')

    def isBlank: Boolean = this == Blank
    def isBlocked: Boolean = this == Blocked
    def isVisited: Boolean = !isBlank && !isBlocked
    override def toString: String = char.toString

    def next: Point = this match
      case Left => Up
      case Up => Right
      case Right => Down
      case Down => Left
      case Blank => Blank
      case Blocked => Blocked

  private object Point:
    val byChar = values.iterator.map(p => p.char -> p).toMap

  private class Guard(lab: Array[Array[Point]], var i: Int, var j: Int):
    private def point = lab(i)(j)
    private var heading = point
    private val si = i
    private val sj = j
    private val x = lab.length
    private val y = lab.head.length

    var visited = 0
    var obstructed = 0

    private def moveTo(pos: (Int, Int)): this.type =
      i = pos._1
      j = pos._2
      this

    private def headTo(heading: Point): this.type =
      this.heading = heading
      this

    def reset(): this.type =
      moveTo(si, sj).headTo(lab(si)(sj))

    private def nextPos = heading match
      case Point.Left => (i, j - 1)
      case Point.Right => (i, j + 1)
      case Point.Up => (i - 1, j)
      case Point.Down => (i + 1, j)
      case Point.Blank | Point.Blocked => (i, j)

    private def isLeaving: Boolean = heading match
      case Point.Left => j <= 0
      case Point.Up => i <= 0
      case Point.Right => j >= y - 1
      case Point.Down => i >= x - 1
      case Point.Blank | Point.Blocked => false

    private def isBlocked =
      val (i, j) = nextPos
      lab(i)(j).isBlocked

    private def track(): Unit =
      if point.isBlank then
        lab(i)(j) = heading
        visited += 1

    private def move(track: Boolean): Boolean =
      var turned = 0
      while isBlocked && turned < 4 do
        headTo(heading.next)
        turned += 1
      if track then this.track()
      if turned < 4 then moveTo(nextPos)
      turned > 0

    def patrol(track: Boolean): this.type =
      if point.isBlocked then return this
      val turns = mutable.Set.empty[(Int, Int, Point)]
      var stuck = false
      while !stuck && !isLeaving
      do if move(track) then stuck = !turns.add((i, j, heading))
      if track then this.track()
      if stuck then obstructed += 1
      this

  def solve(input: Iterator[Input]): (Int, Int) =
    val lab = input.toArray
    if lab.isEmpty then return (0, 0)

    val initial = for
      i <- lab.indices.iterator
      j <- lab(i).indices
      if lab(i)(j).isVisited
    yield try Guard(lab, i, j)
    finally lab(i)(j) = Point.Blank

    val guard = initial.nextOption() match
      case Some(guard) => guard.patrol(track = true)
      case None => return (0, 0)

    for
      i <- lab.indices.iterator
      j <- lab(i).indices
      point = lab(i)(j)
      if point.isVisited
    do
      lab(i)(j) = Point.Blocked
      guard.reset().patrol(track = false)
      lab(i)(j) = point

    (guard.visited, guard.obstructed)

  def parse(line: String): Option[Input] =
    Some(line.toCharArray.map(Point.byChar))

  def part12(file: String): (Int, Int) =
    withResource(file)(solve)

  def main(args: Array[String]): Unit =
    val (visited, obstructed) = part12("day06.txt")
    println(visited)
    println(obstructed)
