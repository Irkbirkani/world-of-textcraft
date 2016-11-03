package TextMUD

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

class NPCManager extends Actor {
  import NPCManager._
  def receive = {
    case NewNPC(name, health, loc, atk, armr) =>
      val n = context.actorOf(Props(new NPC(name, health, atk, armr)), name)
      Main.roomManager ! RoomManager.EnterRoom(loc, n)
  }

}
object NPCManager {
  //NPC Management
  case class NewNPC(name: String, health: Double, loc: String, atk: Int, armr: Int)
}