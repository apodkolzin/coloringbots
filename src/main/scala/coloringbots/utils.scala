package coloringbots

import scala.collection.mutable
import scala.util.Try

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 25.12.14
 * Time: 10:57
 *
 */

/* неявные преобразования */
object Utils {
  //  implicit def bots2Round(bots: Bots): Round = new Round(bots)
  implicit def bot2TurnMaker(bot: Bot with BotLogic):TurnMaker = new TurnMaker(Some(bot), None)
  implicit def option2Cell(o: Option[Cell]): Cell = o.get
  implicit def neighbours2Set(n: Neighbours): Set[Cell] = n.toSet
  implicit def tuple2Coord(t: (Int, Int)): Coord = Coord(t._1, t._2)
  def N = Neighbours(Set())

  type LogicBot = Bot with BotLogic
}

import Utils._

/* Данный объект потребовался для поддержки синтаксиса, используетмого в объекте Round */
class TurnMaker(bot: Option[LogicBot], cell: Option[Cell]) {
  type TurnType = (LogicBot) => Turn
  /**
   * Получаем ход, валидируем его и выполняем и возвращаем пустой TurnMaker.
   * Если была ошибка - возвращаем TurnMaker с установленым ботом  */
   def paint(turn: TurnType): TurnMaker = this take turn map validate map perform getOrElse bad

  /* Если результатом paint возвращен TurnMaker с ботом - выполняем alternative, иначе ничего не делаем  */
  def or(alternative: (LogicBot) => Unit):   TurnMaker = { bot  foreach alternative; this }
  private def take(turn: TurnType): Try[Turn] = Try(bot map turn get)

  def send(optional: (Cell) => Unit): TurnMaker = { cell foreach optional;    this}
  private def validate(turn: Turn): Turn = if (turn.validate) turn else exception
  private def perform(turn: Turn): TurnMaker = {
    val cell: CellImpl = turn.cell.asInstanceOf[CellImpl]
    cell.set(turn.bot)
    new TurnMaker(None, Some(cell))
  }
  private def bad: TurnMaker = new TurnMaker(bot, None)
  private def exception = throw new IllegalStateException("Некорректный ход у бота " + bot)
}

/**
 * Список ботов. Если в ходе выполнения бот совершает недопустимое действие, он дисквалифицируется
 */
class Bots{
  private val bots = new mutable.HashSet[LogicBot]
  private val losers = new mutable.HashSet[LogicBot]

  private def isActive(bot: LogicBot) = !(losers contains bot)

  def register(bot: LogicBot): Bots = { bots += bot; this }
  def disqualify(bot: LogicBot) = losers += bot

  def players: Seq[LogicBot] = bots filter isActive toSeq
  def all: Seq[LogicBot] = bots toSeq
  def dead: Seq[LogicBot] = losers toSeq
  def foreach(turn: (LogicBot) => Unit) = players foreach turn
  def forall(turn: (LogicBot) => Unit)  = all foreach turn
}

class Timer{
  private val map = new mutable.HashMap[Bot, Long]()

  def action(bot: LogicBot, f: LogicBot => Unit) = {
    val start = System.nanoTime
    f(bot)
    map(bot) = map.getOrElse(bot, 0L) + System.nanoTime - start
  }

  override def toString: String = "Timer: " + map.map{case(bot, time) => (bot, format(time))}.toString

  private def format(time: Long): String = s"${time/1e6}  ms"
}

/* Данный объект создан для поддержки синтаксиса, используемого в методе CellImpl.neighbours */
case class Neighbours(private val set: Set[Cell]){
  def +(f: => Cell): Neighbours = Try(f).toOption.fold(this){c =>Neighbours(set + c)}
  def toSet = set.toSet
}


