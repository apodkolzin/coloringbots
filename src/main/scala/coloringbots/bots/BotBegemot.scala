package coloringbots.bots

import coloringbots.{Field, Cell, Turn, Bot}

import scala.util.Random

/**
 * User: rgordeev
 * Date: 31.12.14
 * Time: 13:53
 */
case class BotBegemot(val color: String) extends Bot {

  override var field: Field = null

  /* Оповещение о закрашивании ячейки cell */
  override def notify(cell: Cell): Unit = {}

  /* Возвращает следующий ход */
  override def nextTurn: Turn = {
    1 to (field.size.x + 1) * (field.size.y + 1) map turn find validate getOrElse(dummy)
  }

  private def turn(i: Int): Turn = this -> (x, y)
  private def validate(turn: Turn) = turn.validate
  private def dummy : Turn = {
    (0 to field.size.y).foreach {y=>
      (0 to field.size.x).foreach{ x =>
        val t: Turn = this ->(x, y)
        if (t.validate)
          return t
      }
    }
    throw new Exception("Turn can not define")
  }

  val rnd = new Random

  private def x : Int = rnd.nextInt(field.size.x + 1)
  private def y : Int = rnd.nextInt(field.size.y + 1)
}
