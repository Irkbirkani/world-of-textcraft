package TextMUD

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

class NPCManager extends Actor {
  import NPCManager._
  def receive = {
    case NewNPC(name, health, loc, atk, armr,spd) =>
      val n = context.actorOf(Props(new NPC(name, health, atk, armr,spd)), name)
      Main.roomManager ! RoomManager.EnterRoom(loc, n)
    case RemoveNPC(npc) =>
      context.children.filter(_ != npc)
  }

}
object NPCManager {
  //NPC Management
  case class RemoveNPC(npc:ActorRef)
  case class NewNPC(name: String, health: Double, loc: String, atk: Int, armr: Int, spd:Int)
}