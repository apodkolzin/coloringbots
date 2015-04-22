package coloringbots

import coloringbots.bots._
import coloringbots.zombies.{TimeZombi, PrintZombi}

/**
 * User: rgordeev
 * Date: 19.12.14
 * Time: 16:29
 */
object Launcher extends App{
  private val timer = new TimeZombi

  val game: Game = Game(Coord(14, 14), 100)

  private trait Context extends GameContext{
    override def field = game.field
  }

  game
    .register(new Bot("red") with SimpleRandom with Context)
    .register(new Bot("blue") with ImmortalBot with Context)
    .register(new Bot("yellow") with SeqBot with Context)
    .register(new Bot("orange") with SeqBotV2 with Context)
    .register(new Bot("pink") with ChampionBot with Context)
    .register(new Bot("white") with RapidBot with Context)
    .register(new Bot("café clan") with ClanStrategy with Context)
    .register(new Bot("dart") with BotBegemot with Context)
    .register(new Bot("xoxo") with XoBot with Context)
    .register(new Bot("neon nearest") with NearestStrategy with Context)
    .register(PrintZombi)
    .register(timer)
    .play
//  timer.print
}
