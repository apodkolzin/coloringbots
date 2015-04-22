package coloringbots.bots

import coloringbots._


/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 31.12.14
 * Time: 15:58
 *
 */

trait RandomLogic extends BotLogic with GameContext{

  protected def validate(turn: Turn) = turn.validate
  protected def turn(i: Int): Turn = this -> random
  private def random: (Int, Int) = (field.size.x, field.size.y) <~ 2

  override def notify(cell: Cell) = {}

  implicit class Random(xy: (Int, Int)) {
    private val r = new scala.util.Random
    def <~(border: Int): (Int, Int) = (r.nextInt(xy._1 - border * 2 + 1) + border, r.nextInt(xy._2 - border * 2 + 1) + border)
    def +(tuple: (Int, Int)): (Int, Int) = (xy._1 + tuple._1, xy._2 + tuple._2)
  }
}

