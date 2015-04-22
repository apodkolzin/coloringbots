package coloringbots.bots

import coloringbots._

/**
 * Created by pavel on 30.12.14.
 */
trait ImmortalBot extends BotLogic with GameContext {
  implicit def cellOpt(opt:Option[Cell]) = opt.get
  override def nextTurn: Turn = {

    var i = -1
    while( i < field.size.y - 3)
    {
      var j = -1
      while(j < field.size.x - 3)
      {
        if( validateStart( j, i ) ) {
          val move = this ->(j + 1, i + 1)
          if( move.validate )
            return move
        }
        j=j+1
      }
      i=i+1
    }

    var r = 2
    while( r > 0 )
    {
      var i = 0
      while( i < field.size.y - 2)
      {
        var j = 0
        while(j < field.size.x - 2)
        {
          field.get(Coord(j, i)).whose match {
            case Some(bot) if bot == this =>
              val turn = findInRadius(j, i, r)
              if (turn.isDefined)
                return turn.get
            case _ =>
          }
          j=j+1
        }
        i=i+1
      }
      r=r-1
    }
    this ->(0, 0)
  }

  def validateStart( startX:Int, startY:Int):Boolean = {
    (0 to 1).map( _ * 2 + startY).filter { y => y >= 0 && y < field.size.y }.find { y =>
      ((0 to 1).map(_ * 2 + startX).filter { x => x >= 0 && x < field.size.x}.find { x =>
        val t: Turn = this ->(x, y)
        !(t.validate)
      }) != None
    } == None
  }

  def findInRadius(centerX:Int, centerY:Int, radius:Int):Option[Turn] = {

    List( (-radius + 1 to radius - 1).map(_ + centerY).map{ y => List( (centerX - radius, y), (centerX + radius, y) )}.flatten ,
      (-radius + 1 to radius - 1).map(_ + centerX).map{ x => List( (x, centerY - radius), (x, centerY + radius) )}.flatten )
      .flatten.find{ case(x,y) =>
      { x >= 0 && x < field.size.x } &&
        { y >= 0 && y < field.size.y } &&
        (this->(x,y)).validate }.map { case (x,y) => (this->(x,y)) }
  }
}
