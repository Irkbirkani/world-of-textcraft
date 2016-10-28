package TextMUD

import akka.actor.Actor
import akka.actor.actorRef2Scala
import akka.actor.ActorRef

class Room(
    keyword: String,
    val name: String,
    val description: String,
    private var _items: MutableDLList[Item],
    private val exits: Array[String]) extends Actor {

  import Room._

  def receive = {
    case PrintDescription =>
      sender ! Player.PrintMessage(printDescription)
    case GetExit(dir) =>
      sender ! Player.TakeExit(getExit(dir))
    case LinkRooms(rooms) =>
      actorExits = exits.map(s => if (s.isEmpty) None else Some(rooms(s)))
    case GetItem(name, itemName) =>
      sender ! Player.AddToInventory(getItem(itemName))
      players.foreach(p => p ! Player.PrintMessage(name + " picked up " + itemName + "."))
    case DropItem(name, item) =>
      dropItem(item)
      players.foreach(p => p ! Player.PrintMessage(name + " dropped " + item.name + "."))
    case EnterRoom(pl, name) =>
      players.foreach(p => p ! Player.PrintMessage(name + " entered the room."))
      addPlayer(pl)
    case LeaveRoom(pl, name) =>
      players.foreach(p => p ! Player.PrintMessage(name + " left the room."))
      removePlayer(pl)
    case LeaveGame(pl, name) =>
      players.foreach(p => p ! Player.PrintMessage(name + " left the game."))
      removePlayer(pl)
    case SayMessage(msg, name) =>
      players.foreach(p => p ! Player.PrintMessage(s"$name: $msg"))
  }

  //Print Description
  def printDescription(): String = {
    name + "\n" + description + "\n" +
      "You see: \n" + (if (items.length == 0) "nothing" else (for (i <- 0 until items.length) yield (items(i).name)).mkString("\n")) +
      "\nPlayers in room: \n" + players.map(_.path.name).mkString("\n")
  }

  //Room Exit Management
  def getExit(dir: Int): Option[ActorRef] = {
    actorExits(dir)
  }

  private var actorExits: Array[Option[ActorRef]] = Array.fill(6)(None)

  //Room Player Management
  private var _players: List[ActorRef] = List()

  def players = _players

  def addPlayer(player: ActorRef): Unit = _players = player :: players

  def removePlayer(player: ActorRef): Unit = _players = _players.filter(_ != player)

  //Room Item Management
  def items = _items

  def getItem(itemName: String): Option[Item] = {
    items.find(_.name == itemName) match {
      case Some(item) => {
        _items = _items.filter(_ != item)
        Some(item)
      }
      case None =>
        None
    }
  }

  def dropItem(item: Item): Unit = {
    _items += item
  }
}

object Room {
  case object PrintDescription

  case class GetExit(dir: Int)
  case class LinkRooms(rooms: Map[String, ActorRef])

  case class GetItem(name: String, itemName: String)
  case class DropItem(name: String, item: Item)

  case class LeaveRoom(p: ActorRef, name: String)
  case class EnterRoom(p: ActorRef, name: String)
  case class LeaveGame(p: ActorRef, name: String)

  case class SayMessage(msg: String, name: String)

  def apply(n: xml.Node): Room = {
    val keyword = (n \ "@keyword").text
    val name = (n \ "@name").text
    val description = (n \ "description").text
    val item = new MutableDLList[Item]()
    (n \ "item").map { inode => Item(inode) }.toList.foreach(i=> item += i)
    val exits = (n \ "exits").text.split(",").padTo(6, "")
    new Room(keyword, name, description, item, exits)
  }
}

