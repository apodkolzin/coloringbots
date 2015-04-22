package coloringbots.bots

import coloringbots.{Bot, Cell, Field, Turn}

import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 18.12.14
 * Time: 17:37
 * Since: 
 *
 */
//todo use random method
trait SimpleRandom extends RandomLogic{
  override def nextTurn: Turn = 1 to 200 map turn find validate get
}