package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef

case class NPC(name: String, health: Double) extends Actor {
  import NPC._
  import Character._
  Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove)

  private var _location: ActorRef = null
  def location = _location

  def receive = {
    case TakeExit(dir) =>
      dir match {
        case Some(dest) =>
          if (_location != null) location ! Room.LeaveRoom(self, name)
          _location = dest
          location ! Room.EnterRoom(self, name)
        case None =>
      }
    case RequestMove =>
      println("Message Recieved")
      this.move(util.Random.nextInt(6))
      Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove)

  }

  def move(direction: Int): Unit = {
    location ! Room.GetExit(direction)
  }

}

object NPC {
  def apply(n: xml.Node): Unit = {
    Main.npcManager ! NPCManager.NewNPC((n \ "@name").text, n.text.toDouble, (n \ "@location").text)
  }
  case object RequestMove
  val moveTime = 50

}