package coloringbots.bots

import coloringbots._

import scala.util.{Try, Random}

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 31.12.14
 * Time: 15:56
 *
 */
trait NearestStrategy extends RandomLogic{
  private var current: Option[Turn] = None

  override def nextTurn: Turn = {
    if (current.isEmpty)
      current = this find first
    else
      current = this find next
    current.get
  }

  private def find(f: => (Int, Int)): Option[Turn] = {
    var i = 40
    var res: Option[Turn] = None
    while (i > 0 && res.isEmpty ) {
      res = Try(turn(f)).toOption.find(validate)
      i = i - 1
    }
    res
  }
  def turn(f: => (Int, Int)): Turn = this -> f
  private def validate(turn: Option[Turn]): Boolean = turn exists validate

  private def first: (Int, Int) = (x, y) <~ 2
  private def next:  (Int, Int) = (x, y) + ( (0, 0) <~ -2 )
  private def x = coord.x
  private def y = coord.y
  private def coord: Coord = current.map(_.cell.coord).getOrElse(field.size)

  implicit def cell2tuple(c: Cell): (Int, Int)= (c.coord.x, c.coord.y)

}
