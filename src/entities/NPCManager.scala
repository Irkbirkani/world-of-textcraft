package entities

import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala
import entities._
import mud.Main
import room.RoomManager


class NPCManager extends Actor {
  import NPCManager._
  def receive = {
    case NewNPC(name, lvl, health, loc, atk, armr, spd, itms, desc) =>
      val n = context.actorOf(Props(new NPC(name, lvl, health, loc, atk, armr, spd, itms, desc)), name.toUpperCase())
      Main.roomManager ! RoomManager.EnterRoom(loc, n)
  }

}
object NPCManager {
  //NPC Management
  case class NewNPC(name: String, lvl: Int, health: Double, loc: String, atk: Int, armr: Int, spd: Int, itms: List[Item], desc: String)
}