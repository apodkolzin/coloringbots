package coloringbots.bots

import coloringbots._

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 26.12.14
 * Time: 17:43
 * Since: 
 *
 */
trait SeqBot extends BotLogic with GameContext{

  override def nextTurn: Turn = {
    (0 to field.size.y).foreach {y=>
      (0 to field.size.x).foreach{ x =>
        val t: Turn = this ->(x, y)
        if (t.validate)
          return t
      }
    }
    throw new Exception("Turn can not define")
  }
}