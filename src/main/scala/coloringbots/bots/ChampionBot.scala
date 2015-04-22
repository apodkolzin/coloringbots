package coloringbots.bots

import coloringbots._

/**
 * Created by azimka on 30.12.14.
 */
trait ChampionBot extends BotLogic with GameContext {

  override def nextTurn: Turn = {
    var innerDiagSize = 0

    val diagonalMax = Math.max(field.size.y, field.size.x)
    val diagonalMin = Math.min(field.size.y, field.size.x)

    (1 to diagonalMax).foreach { diagonalCounter =>
      if (diagonalCounter < diagonalMin - 2) {
        innerDiagSize = diagonalCounter;
      } else {
        innerDiagSize = diagonalMin - 2
      }
      (1 to innerDiagSize).foreach { innerDiagCounter =>
        val t: Turn = this ->(diagonalCounter - innerDiagCounter, innerDiagCounter)
        if (t.validate)
          return t
      }
    }
    (0 to field.size.y).foreach { y =>
      (0 to field.size.x).foreach { x =>
        val t: Turn = this ->(x, y)
        if (t.validate)
          return t
      }
    }
    throw new Exception("Turn can not define")
  }

  var isFirstStep = true;

  def doFirstStep: Turn = {
    isFirstStep = false;
    val t: Turn = this ->(field.size.x - 2, field.size.y - 2)
    return t;
  }


}