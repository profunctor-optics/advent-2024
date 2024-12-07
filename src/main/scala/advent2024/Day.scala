package advent2024

import scala.io.Source
import scala.util.{Failure, Success, Using}

trait Day:
  type Input
  
  def parse(line: String): Option[Input]

  def withResource[R](file: String)(solve: Iterator[Input] => R): R =
    def doParse(line: String, i: Int) =
      parse(line).getOrElse(throw new IllegalArgumentException(s"Unexpected input on line #$i: $line"))
    def doSolve(source: Source) =
      solve(source.getLines().zipWithIndex.filterNot(_._1.isBlank).map(doParse))
    Using(Source.fromResource(file))(doSolve) match
      case Success(result) => result
      case Failure(error) => throw error
