package coloringbots.zombies

import coloringbots._

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 25.12.14
 * Time: 18:20
 * Since: 
 *
 */
object PrintZombi extends Bot("priter") with BotLogic
{
  override def notify(cell: Cell): Unit = this print cell
  override def nextTurn: Turn = throw new UnsupportedOperationException()


  private def print(cell: Cell): Unit = {
    val counter = new Counter
    val field: Field = cell.field

    (0 to field.size.y).reverse.foreach{y=>
      (0 to field.size.x).map(x => field.get(Coord(x, y)).get).foreach(c => print(char(c, cell, counter)))
      println()
    }
    println(counter)
  }

  private def print(c: Char): Unit = Predef.print(c + "\t")
  private def char(i: Cell, cell: Cell, counter: Counter): Char = {
    i.whose.foreach(counter.inc)

    color(i).map(c =>
      if (i.coord == cell.coord)
        c.toUpperCase
      else
        c.toLowerCase)
      .getOrElse("_")(0)
  }
  private def color(i: Cell): Option[String] = i.whose.map(_.color)
}

class Counter {
  private val map = new mutable.HashMap[Bot, Int]()

  def inc(bot: Bot) = map(bot) = map.getOrElse(bot, 0) + 1

  override def toString: String = map.toString
}