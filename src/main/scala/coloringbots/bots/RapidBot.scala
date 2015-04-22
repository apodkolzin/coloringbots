package coloringbots.bots

import java.util

import coloringbots._

import scala.util.Random

/**
 * Created by ����� on 31.12.2014.
 */
trait RapidBot extends BotLogic with GameContext{
val random = new Random
  val cellMap:collection.mutable.ArrayBuffer[(Int,Int)]= collection.mutable.ArrayBuffer.empty[(Int,Int)]

  lazy val init = {
    var i:Int = 0
    while( i < field.size.x ) {
      var j:Int = 0
      while( j < field.size.y ) {
        field.get(Coord(i, j)) match {
          case Some(cell) if cell.isEmpty && cell.neighbours.count(_.whose.exists(_ != this)) == 0 =>
            cellMap += ((i, j))
          case _ =>
        }
        j=j+1
      }
      i=i+1
    }
  }

  override def nextTurn: Turn = {
    if( cellMap.isEmpty) init

    val idx = random.nextInt( cellMap.size )
    val (x:Int,y:Int) = cellMap( random.nextInt( cellMap.size ) )

    cellMap.remove( idx )

    val t = this -> (x,y)
    if( !t.validate )
    {
      (0 to field.size.y).foreach {y=>
        (0 to field.size.x).foreach{ x =>
          val t: Turn = this ->(x, y)
          if (t.validate)
            return t
        }
      }
    }

    t
  }

  override def notify(cell: Cell) = {
    val c = cell.coord
    List(
      ( c.x + 1, c.y + 1),
      ( c.x - 1, c.y + 1),
      ( c.x + 1, c.y - 1),
      ( c.x - 1, c.y - 1),
      ( c.x + 1, c.y ),
      ( c.x - 1, c.y ),
      ( c.x, c.y + 1 ),
      ( c.x, c.y - 1),
      (c.x, c.y))
      .withFilter { case(x,y) => x >= 0 && x < field.size.x && y >= 0 && y < field.size.y }
      .map { case(x,y) => field.get( Coord(x,y) ).get }
      .foreach( traverse _ )


    def traverse(cell:Cell) = {
      if ( !cell.whose.contains(this) )
      {
        val turn = this -> cell
        val c = (cell.coord.x, cell.coord.y)
        if (turn.validate)
        {
          if (!cellMap.contains(c))
            cellMap += (c)
        }
        else
          cellMap -= (c)
      }
    }
  }
}
