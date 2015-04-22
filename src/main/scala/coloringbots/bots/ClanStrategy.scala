package coloringbots.bots

import coloringbots.Utils._
import coloringbots._
import scala.util.{Try, Random}

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 31.12.14
 * Time: 11:17
 * Since: 
 *
 */
trait ClanStrategy extends RandomLogic {
  implicit def cell2tuple(cell: Cell): (Int, Int) = (cell.coord.x, cell.coord.y)
  private var lasts = List[Turn]()

  override def nextTurn: Turn = {
    var i: Int = Math.min(lasts.length, 2)

    var turn: Option[Turn] = None
    while (i > 0 && turn.isEmpty){
      turn = findNeighbour(i)
      i = i - 1
    }
    save(turn.getOrElse(randomTurn))
  }

  private def findNeighbour(n: Int): Option[Turn] = {
    val last = lasts.toIterator

    var res: Option[Turn] = None
    while (res.isEmpty && last.hasNext) {
      val next: Turn = last.next()
      res = findNeighbour(next.cell, n)
    }
    res
  }

  private def findNeighbour(cell: Cell, n: Int): Option[Turn] = {
    val neighbours = cell.neighbours.toIterator

    var res: Option[Turn] = None
    while (res.isEmpty && neighbours.hasNext) {
      val next: Cell = neighbours.next()
      res = Try(this -> next).toOption
      if (!validate(res, n)) res = None
    }
    res
  }

  private def validate(turn: Option[Turn], n: Int): Boolean = turn.isDefined && turn.get.validate && turn.get.my >= n

  private def randomTurn: Turn = 1 to 20 map turn find validate get
  private def save(turn: Turn): Turn = {lasts = turn :: lasts; turn}


  override def notify(cell: Cell): Unit = {}
}
