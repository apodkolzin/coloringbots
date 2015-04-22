package coloringbots

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 18.12.14
 * Time: 17:28
 * Since: 
 *
 */
/**
 * Объект Координаты (x,y)
 */
case class Coord(x: Int, y: Int) {
  override def toString = s"{$x,$y}"
}

/**
 * трейт Ячейка: координаты, соседние ячейки, бот закрасивший ячейку
 */
trait Cell {
  /* Координаты ячейки на поле */
  def coord: Coord
  /* Поле */
  def field: Field
  /* Бот */
  def whose: Option[Bot]
  /* соседи */
  def neighbours: Set[Cell]
  /* Ячейка сверху */
  def up: Option[Cell]
  /* Ячейка снизу */
  def down: Option[Cell]
  /* Ячейка слева */
  def left: Option[Cell]
  /* Ячейка вправа */
  def right: Option[Cell]

  def isEmpty: Boolean = whose.isEmpty
}

/* Поле */
trait Field {
  /* Размер поля (координаты крайней ячейки) */
  def size: Coord
  /* Возвращает ячейку на поле по координате coord */
  def get(coord: Coord): Option[Cell]
  /* Матрица всех ячеек */
  def cells: Array[Array[Cell]]
}

/* Бот */
trait BotLogic{
  /* Возвращает следующий ход */
  def nextTurn: Turn
  /* Оповещение о закрашивании ячейки cell */
  def notify(cell: Cell) {}
}

trait GameContext {
  def field: Field

  protected final def ->(cell: Cell): Turn = new Turn(this.asInstanceOf[Bot], cell)
  implicit def tuple2coord(tuple: (Int, Int)): Cell = field.get(Coord(tuple._1, tuple._2)).get
}

class Bot(val color: String) {
  override def toString: String = s"$color bot"

  override def hashCode(): Int = color.hashCode

  override def equals(obj: scala.Any): Boolean = obj match {
    case bot: Bot => bot.color == color
    case _ => false
  }
}

/* объект Ход, определяется намерениями бота @param bot закрасить ячейку cell */
final class Turn(val bot: Bot, val cell: Cell){
  /* Определяет корректность данного хода */
  def validate: Boolean = canPaint
  def canPaint = isBlank || canFill || canOver

  /* ячейка пуста */
  def isBlank: Boolean = cell.isEmpty && alien == 0
  /* ячейка пуста и есть как минимум 2х соседних своих ячеек */
  def canFill: Boolean = cell.isEmpty && my > 1
  /* есть как минимум 3х соседних своих ячеек  */
  def canOver: Boolean = !cell.whose.contains(bot) && my > 2

  def alien = cell.neighbours.count(_.whose.exists(_ != bot))
  def my = cell.neighbours.count(_.whose.contains(bot))
  override def toString: String = s"$bot -> (${cell.coord.x}, ${cell.coord.y}})"
}
