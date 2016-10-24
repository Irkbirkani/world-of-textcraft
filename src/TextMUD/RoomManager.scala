package TextMUD

import akka.actor.Actor
import scala.io.Source
import akka.actor.Props
import akka.actor.ActorRef

class RoomManager extends Actor {
  import RoomManager._
  def receive = {
    case EnterRoom(loc, p) =>
      p ! Player.TakeExit(Some(rooms(loc)))
  }
  val rooms = {
    (xml.XML.loadFile("map.xml") \ "room").map { n =>
      val key =  (n \ "@keyword").text 
     key -> context.actorOf(Props(Room(n)),key)
    }.toMap
  }
  context.children.foreach(_ ! Room.LinkRooms(rooms))
}

object RoomManager {
  case class EnterRoom(loc: String, p: ActorRef)
}
