package TextMUD

import akka.actor.Actor
import akka.actor.actorRef2Scala
import akka.actor.ActorRef
import scala.Console._

class Room(
    keyword: String,
    val name: String,
    val description: String,
    private var _items: MutableDLList[Item],
    private val exits: Array[String]) extends Actor {

  import Room._
  import Character._

  def receive = {
    case PrintDescription =>
      sender ! Player.PrintMessage(printDescription)
    //Exit Management
    case GetExit(dir) =>
      sender ! TakeExit(getExit(dir))
    case LinkRooms(rooms) =>
      actorExits = exits.map(e => if (e.isEmpty) None else Some(rooms(e)))
      sender ! RoomManager.LinkingRooms(name,exits)
    //Item Management
    case GetItem(name, itemName) =>
      sender ! Player.AddToInventory(getItem(itemName))
    case DropItem(name, item) =>
      dropItem(item)
    //Player Management
    case EnterRoom(pl, name) =>
      chars.foreach(p => p ! Player.PrintMessage(name + " entered the room."))
      addPlayer(pl)
    case LeaveRoom(pl, name) =>
      chars.foreach(p => p ! Player.PrintMessage(name + " left the room."))
      removePlayer(pl)
    case LeaveGame(pl, name) =>
      chars.foreach(p => p ! Player.PrintMessage(name + " left the game."))
      removePlayer(pl)
    case HasDied(pl,name)=>
      chars.foreach(p => p ! Player.PrintMessage(name + " has died!"))
      removePlayer(pl)
    //Messages  
    case SayMessage(msg, name) =>
      chars.foreach(p => p ! Player.PrintMessage(s"${RESET}${MAGENTA}$name: $msg${RESET}"))
    case CheckInRoom(c) =>
      val ch = chars.filter(_.path.name == c)
      if (ch.length == 0) {
        sender ! Player.PrintMessage("A swing and a miss!")
      } else {
        sender ! KillCmnd(ch(0))
      }
  }

  //Print Description
  def printDescription(): String = {
    s"${RESET}${CYAN}$name\n$description${RESET}" +
      s"${RESET}${GREEN}\nYou see: \n" +
      {GREEN}+(if (items.length == 0) "nothing" else (for (i <- 0 until items.length) yield (items(i).name)).mkString("\n"))+"\n========" +{RESET} + 
      {RESET}+{YELLOW}+"\nPlayers in room: \n" + chars.map(_.path.name).mkString("\n")+"\n========== "+{RESET}
  }

  //Room Exit Management
  def getExit(dir: Int): Option[ActorRef] = {
    actorExits(dir)
  }

  private var actorExits: Array[Option[ActorRef]] = Array.fill(6)(None)

  //Room Player Management
  private var _chars: List[ActorRef] = List()

  def chars = _chars

  def addPlayer(char: ActorRef): Unit = _chars = char :: chars

  def removePlayer(char: ActorRef): Unit = _chars = _chars.filter(_ != char)

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
  //Description
  case object PrintDescription
  //Inventory Management
  case class GetItem(name: String, itemName: String)
  case class DropItem(name: String, item: Item)
  //Exit Management
  case class GetExit(dir: Int)
  case class LinkRooms(rooms: Map[String, ActorRef])
  //Room Character Management
  case class LeaveRoom(p: ActorRef, name: String)
  case class EnterRoom(p: ActorRef, name: String)
  case class HasDied(p: ActorRef, name: String)
  case class LeaveGame(p: ActorRef, name: String)
  //Messaging
  case class SayMessage(msg: String, name: String)
  //Combat Management
  case class CheckInRoom(c: String)

  def apply(n: xml.Node): Room = {
    val keyword = (n \ "@keyword").text
    val name = (n \ "@name").text
    val description = (n \ "description").text
    (n \ "npcs").map { npc => NPC(npc) }
    val item = new MutableDLList[Item]()
    (n \ "item").map { inode => Item(inode) }.toList.foreach(_ +=: item)
    val exits = (n \ "exits").text.split(",").padTo(6, "")
    new Room(keyword, name, description, item, exits)
  }
}