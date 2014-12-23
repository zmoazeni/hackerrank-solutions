// solution for https://www.hackerrank.com/challenges/saveprincess

import scala.io.StdIn
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Solution {
  type Position = (Int, Int)

  sealed abstract class Action
  case class DirUp()    extends Action { override def toString = "UP"    }
  case class DirDown()  extends Action { override def toString = "DOWN"  }
  case class DirRight() extends Action { override def toString = "RIGHT" }
  case class DirLeft()  extends Action { override def toString = "LEFT"  }

  lazy val matrix = {
    var theMatrix: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer()

    val source = Source.stdin.getLines
    source.next // drop the first line which is an integer
    for (line <- source) {
      theMatrix += line.toList.to[ArrayBuffer]
    }

    theMatrix
  }

  lazy val marioPosition    = findPositionForChar('m').get
  lazy val princessPosition = findPositionForChar('p').get

  def main(args: Array[String]) = {
    println(getDirectionBetweenPositions(marioPosition, princessPosition))
  }

  private def findPositionForChar(lookFor: Char): Option[Position] = {
    for (y <- matrix.indices) {
      val row = matrix(y)

      for (x <- row.indices) {
        val value = row(x)

        if (lookFor == value) {
          return Some(x, y)
        }
      }
    }

    return None
  }

  private def getDirectionBetweenPositions(origin: Position, destination: Position): Action = {
    (origin, destination) match {
      case((x1, y1), (x2, y2)) => {
        if (x1 < x2) {
          DirRight()
        } else if (x1 > x2) {
          DirLeft()
        } else if (y1 < y2) {
          DirDown()
        } else {
          DirUp()
        }
      }
    }
  }
}
