package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef

case class NPC(name: String, health: Double) extends Actor {
  import NPC._
  
  private var _location: ActorRef = null
  def location = _location
  
  def receive = {
    case EnterRoom(dir) =>
      dir match {
        case Some(dest) =>
          _location = dest
          location ! Room.EnterRoom(self, name)
        case None =>
      }
  }
  
  
  private def move(direction: Int): Unit = {
    location ! Room.GetExit(direction)
  }

  private var moveTime = 25
  def update():Unit = {
    moveTime -= 1
    if (moveTime == 0) {
      moveTime = 50
      move(util.Random.nextInt(6))
    }
    
  }
}

object NPC {
  def apply(n: xml.Node): NPC = {
    val npc = new NPC((n \ "@name").text, n.text.toDouble)
    Main.entityManager ! EntityManager.NewNPC(npc.name, npc.health, (n \ "@location").text)
    npc
  }
  case class EnterRoom(dir: Option[ActorRef])
  
}