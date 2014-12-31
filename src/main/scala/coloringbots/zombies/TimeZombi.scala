package coloringbots.zombies

import coloringbots.{Bot, Cell, Field, Turn}

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 30.12.14
 * Time: 16:35
 * Since: 
 *
 */
class TimeZombi extends Bot {
  val map = new mutable.HashMap[Bot, Long]
  var last: Long = currentTime

  override def notify(cell: Cell): Unit = {
    val bot = cell.whose.get
    val time = currentTime - last
    map(bot) = map.getOrElse(bot, 0L) + time
    last = currentTime
  }


  def currentTime = System.nanoTime

  def print = println(map.map{case(bot, time) => (bot, format(time))})

  override var field: Field = null
  override def color: String = "time"
  override def nextTurn: Turn = throw new UnsupportedOperationException

  private def format(time: Long): String = s"${time/1e6}  ms"
}
