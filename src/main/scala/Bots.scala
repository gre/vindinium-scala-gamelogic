package bot

import scala.util.Random
import scala.util.{Success, Try}
import org.vindinium.server.{Game => VGame, Hero => VHero, Arbiter}
import Dir._
import Tile._
import Vindiniumify._

trait Bot {
  def move(input: Input): Dir
}

// This bot example only maximize the next move score
class BestNextMoveBot extends Bot {

  def score (player: Int, game: VGame) = {
    val hero = game.hero(player)
    val golds = game.heroes.map(_.gold).sum
    100.0 * (2.0 * game.board.countMines(player).toDouble / game.board.countMines.toDouble - 1.0) +
    10.0 * (if (golds==0) 0.0 else 2.0 * hero.gold.toDouble / golds.toDouble - 1.0) +
    2.0 * (2.0 * hero.life.toDouble / VHero.maxLife.toDouble - 1.0)
  }

  def move(input: Input) = {
    val game: VGame = input.game
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West))
      .filter(dir => input.game.board at input.hero.pos.to(dir) exists (Wall!=))
      .map(dir => (dir, Try(Arbiter.move(game, tokenForHero(game.heroId), dir)).flatten))
      .collect{ case (dir, Success(newGame)) => (dir, score(input.hero.id, newGame)) }
      .sortBy(_._2)
      .lastOption.map(_._1)
      .getOrElse(Dir.Stay)
  }
  
}

