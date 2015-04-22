package coloringbots.bots

import coloringbots._

trait XoBot extends BotLogic with GameContext{
  override def nextTurn: Turn = {

    var t: Turn = this ->(1, 1)
    if (t.validate) return t

    (1 to (field.size.x + field.size.y + 1) ).foreach {step =>

      var startX = 0;
      if (step - field.size.y - 1 > 0) startX = step - field.size.y - 1

      (startX to math.min (step - 1, field.size.x) ).foreach {x =>
        val t: Turn = this -> (x, step - x - 1)
        if (t.validate) return t
      }
    }

    //throw new Exception("Turn can not define")
    this->(0, 0)
  }
}