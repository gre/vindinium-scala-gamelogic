package bot

import org.vindinium.server.{Game => VGame}
import org.vindinium.server.{Board => VBoard}
import org.vindinium.server.{Hero => VHero}
import org.vindinium.server.{Pos => VPos}
import org.vindinium.server.{Tile => VTile}
import org.vindinium.server.{Status => VStatus}
import org.vindinium.server.{Dir => VDir}

object Vindiniumify {

  def tokenForHero (id: Int) = ""+id

  implicit def convertPos (p: Pos): VPos = {
    VPos(p.x, p.y)
  }

  implicit def convertHero (h: Hero): VHero = {
    VHero(h.id, tokenForHero(h.id), h.name, None, None, h.pos, h.life, h.gold, h.crashed)
  }

  implicit def convertTile (t: Tile): VTile = {
    t match {
      case Tile.Air => VTile.Air
      case Tile.Wall => VTile.Wall
      case Tile.Tavern => VTile.Tavern
      case Tile.Mine(owner) => VTile.Mine(owner)
      case _ => VTile.Air
    }
  }

  implicit def convertBoard (b: Board): VBoard = {
    VBoard(b.tiles.map(convertTile))
  }

  implicit def convertGame (g: Game): VGame = {
    VGame(g.id, false, g.board, g.heroes(0), g.heroes(1), g.heroes(2), g.heroes(3), spawnPosForGame(g), g.turn, g.maxTurns, VStatus.Started)
  }
  def spawnPosForGame (g: Game): VPos = g.heroes.find(_.id == 1).get.spawnPos

  implicit def convertDir (d: Dir.Dir): VDir = d match {
    case Dir.North => VDir.North
    case Dir.South => VDir.South
    case Dir.East  => VDir.East
    case Dir.West  => VDir.West
    case _         => VDir.Stay
  }
  implicit def reverseDir (d: VDir): Dir.Dir = d match {
    case VDir.North => Dir.North
    case VDir.South => Dir.South
    case VDir.East  => Dir.East
    case VDir.West  => Dir.West
    case _          => Dir.Stay
  }

}
