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
    case LinkingRooms(key, exits) =>
      roomExits + (key -> exits)
    case ShortestPath(curr, dest) =>
      shortestPath(curr, dest, roomExits, Array()).foreach { p => sender ! Player.PrintMessage(p) }
  }
  val rooms = {
    (xml.XML.loadFile("map.xml") \ "room").map { n =>
      val key = (n \ "@keyword").text
      key -> context.actorOf(Props(Room(n)), key)
    }.toMap
  }
  val roomExits: Map[String, Array[String]] = Map()
  context.children.foreach(_ ! Room.LinkRooms(rooms))

  def shortestPath(curr: String, dest: String, exitsMap: Map[String, Array[String]], visited: Array[String]): Array[String] = {
    curr ++ visited
    if (curr == dest) visited
    else {
      for (i <- exitsMap(curr); if (!visited.contains(i))) yield {
        i ++ (shortestPath(i, dest, exitsMap, visited))
      }
      visited
    }
  }
}

object RoomManager {
  //Puts char in a room
  case class EnterRoom(loc: String, p: ActorRef)
  case class LinkingRooms(key: String, exits: Array[String])
  case class ShortestPath(curr: String, dest: String)

}
