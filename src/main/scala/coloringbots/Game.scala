package coloringbots

import scala.util.Try
import Utils._

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 18.12.14
 * Time: 17:43
 * Since: 
 *
 */

/* Реализация ячейки */
case class CellImpl (override val coord: Coord, override val field: Field) extends Cell{
  protected var bot: Option[Bot] = None

  private def x: Int = coord.x
  private def y: Int = coord.y
  private def get(x: Int, y: Int) = field.get(x -> y)

  override def whose   = bot
  override def left  = get(x - 1, y    )
  override def right = get(x + 1, y    )
  override def up    = get(x    , y + 1)
  override def down  = get(x    , y - 1)
  override def neighbours: Set[Cell] = N + up.left + up + up.right + left + right + down.left + down + down.right

  def set(bot: Bot): Boolean = { this.bot = Some(bot); println(s"$coord marked ${bot.color}"); true }
}

/* Реализация поля */
case class FieldImpl(override val size: Coord) extends Field{
  override val cells:Array[Array[Cell]] = Array.ofDim[Cell](size.x + 1, size.y + 1)
  override def get(coord: Coord): Option[Cell] = Try(this getOrNew coord).toOption

  private def getOrNew(coord: Coord): Cell = Option(this cell coord).getOrElse{
    cells(coord.x)(coord.y) = CellImpl(coord,this)
    this cell coord
  }
  private def cell(coord: Coord): Cell = cells(coord.x)(coord.y)

}


/* Объект Раунд */
class Round(val bots: Bots, timer: Timer){
  /* Делает ходы всех ботов по разу */
  def run(i: Int) = bots foreach turnAndTime
  /* Выполняет ход бота. Если ход с ошибкой или некорректный - дисквалификация бота */
  def turn(bot: Bot): Unit = {
    // вся красивость реализована в объекте TurnMaker
    bot paint cell send notify or disqualify
  }

  def turnAndTime(bot: Bot) = timer action(bot, turn)

  /* Дисквалификация бота */
  private def disqualify(bot: Bot): Unit = {bots disqualify bot; None}
  /* Ячейка представляется объектом ход */
  private def cell(bot: Bot): Turn = bot.asInstanceOf[BotLogic].nextTurn
  private def notify(cell: Cell): Unit = bots.forall(_.asInstanceOf[BotLogic].notify(cell))
}

/**
 * Объект Игра
 * @param size   - размер поля
 * @param rounds - количество раундов
 */
case class Game (size: Coord, rounds: Int) extends GameContext{
  val field = FieldImpl(size)
  val bots = new Bots
  private val timer = new Timer


  /* регистрация бота */
  def register(bot: Bot with BotLogic) = { bots register bot; this }

  /* запуск игры */
  def play = {
    1 to rounds foreach round
    println(s"${bots.players} are still alive")
//    println(s"${bots.dead} was dead")
    this
  }

  /* Выполняет i-й раунд */
  private def round(i: Int) = {
    println(s"round $i")
    println(s"${bots.players} are ready to fight")
    new Round(bots, timer).run(i)
    println(timer)
  }
}

