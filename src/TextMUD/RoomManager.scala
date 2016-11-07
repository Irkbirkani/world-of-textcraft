package TextMUD

import akka.actor.Actor
import scala.io.Source
import akka.actor.Props
import akka.actor.ActorRef

class RoomManager extends Actor {
  import RoomManager._
  //Actor Management
  def receive = {
    case EnterRoom(loc, p) =>
      p ! Character.TakeExit(Some(rooms(loc)))
      
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
  //Puts char in a room
  case class EnterRoom(loc: String, p: ActorRef)
 
}
