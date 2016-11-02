package TextMUD

import scala.io.Source
import akka.actor.Actor
import java.io.InputStream
import java.io.PrintStream
import akka.actor.ActorRef
import java.io.BufferedReader
import java.net.Socket

class Player(
    val name: String,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    val input: BufferedReader,
    val output: PrintStream,
    val sock: Socket) extends Actor {

  import Player._
  import Character._

  def health = _health
  private var victim: Option[ActorRef] = None

  //location access
  private var _location: ActorRef = null
  def location = _location

  //Actor Management
  def receive = {
    case ProcessInput =>
      if (input.ready()) {
        val in = input.readLine().trim
        if (in.nonEmpty) {
          processCommand(in)
        }
      }
    case PrintMessage(msg) => output.println(msg)
    case AddToInventory(item) =>
      item match {
        case Some(item) =>
          addToInventory(item)
          location ! Room.SayMessage("picked up " + item.name + ".", name)
        case None =>
          output.println("Item not found")
      }
    case TakeExit(dir) =>
      dir match {
        case Some(dest) =>
          if (_location != null) location ! Room.LeaveRoom(self, name)
          _location = dest
          location ! Room.EnterRoom(self, name)

          location ! Room.PrintDescription
        case None =>
          output.println("You can't go that way")
      }
    case KillCmnd(c) =>
      victim = Some(c)
      output.println("You are hitting " + c.path.name)
      println("step 3 started")
      Main.activityManager ! ActivityManager.Enqueue(10, AttackNow)
      println("step 3 sent")
    case AttackNow =>
      victim.foreach(c => c ! SendDamage(location, 5, c))
    case SendDamage(loc, dmg, c) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, isAlive)
        output.println(c.path.name + " dealt " + dmg + "damage!")
        if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(10, AttackNow)
        }
        if (!isAlive) {
          output.println("You have died.")
          this.sock.close
        }
      } else {
        sender ! PrintMessage("You are having a hard time finding them.")
      }
    case DamageTaken(dmg, alive) =>
      if (alive) {
        output.println("You dealt " + dmg + " damage to " + victim.get.path.name + "!")
        kill(victim.get.path.name)
      } else {
        output.println("you killed " + victim.get.path.name + ".")
        victim = None
      }

  }

  //Inventory Management
  def inventory = _inventory

  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.name == itemName) match {
      case Some(item) => {
        _inventory = _inventory.filter(_ != item)
        Some(item)
      }
      case None =>
        None
    }
  }

  def inspectItem(item: String): Unit = {
    for (i <- 0 until inventory.length; if (inventory(i).name == item)) yield output.println(inventory(i).description)
  }

  def addToInventory(item: Item): Unit = {
    _inventory += item
  }

  private def emptyInventory: Boolean = {
    inventory.isEmpty
  }

  def printInventory(): Unit = {
    if (emptyInventory == true) output.println("You have nothing in your bag.")
    else {
      output.println("You have: ")
      for (i <- 0 until inventory.length) yield output.println(inventory(i).name)
    }
  }

  //Equipment management
  private var _equipment: MutableDLList[Option[Item]] = new MutableDLList[Option[Item]].fill(5)(None)
  val equipment = _equipment
  //  println(equipment.length)
  //
  //  def equip(item: String): Unit = {
  //    val itm = getFromInventory(item)
  //    itm match {
  //      case None =>
  //        PrintMessage("You cannot equip an item you do not have in your inventory.")
  //      case Some(i) =>
  //        i.itype match {
  //          case Item.misc => PrintMessage("You cannot equip that.")
  //          case Item.weapon =>
  //            equipment(0) match {
  //              case None => _equipment.update(0, itm)
  //              case Some(itype) => equipment(1) match {
  //                case None => _equipment.update(1, itm)
  //                case Some(itype) => PrintMessage("Weapon slots are full.")
  //              }
  //            }
  //          case Item.armor =>
  //            equipment(2) match {
  //              case None => _equipment.update(2, itm)
  //              case Some(itype) => equipment(3) match {
  //                case None => _equipment.update(3, itm)
  //                case Some(itype) => equipment(4) match {
  //                  case None => _equipment.update(4, itm)
  //                  case Some(itype) => PrintMessage("Armor slots are full.")
  //                }
  //              }
  //            }
  //        }
  //    }
  //  }
  //  def unequip(item: String): Unit = {
  //    for (i <- 0 to equipment.length) {
  //      equipment(i) match {
  //        case Some(itm) =>
  //          if (itm.name == item) {
  //            _equipment.remove(i)
  //            addToInventory(itm)
  //            PrintMessage(item + "unequipped and added to Inventory.")
  //          }
  //        case None =>
  //      }
  //    }
  //  }
  //
  //  def printEquipment() = {
  //    for (i <- equipment) i match {
  //      case Some(item) => output.println(item.name)
  //      case None => output.println("Empty slot.")
  //    }
  //  }

  //Move Player
  private def move(direction: Int): Unit = {
    location ! Room.GetExit(direction)
  }

  //Combat commands
  var isAlive = true

  def kill(pl: String): Unit = {
    println("step 1 started")
    location ! Room.CheckInRoom(pl)
    println("step 1 sent")
  }
  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Int) = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    _health -= actDmg
    if (health <= 0) isAlive = false
    actDmg
  }

  //Player Tell Messaging
  def tellMessage(s: String): Unit = {
    val Array(_, to, msg) = s.split(" +", 3)
    Main.playerManager ! PlayerManager.PrintTellMessage(to, name, msg)
  }

  //Process Player Input
  def processCommand(in: String) = {
    //player quit
    if ("quit".startsWith(in)) {
      sock.close()
      location ! Room.LeaveGame(self, name)
    } //player movement
    else if ("north".startsWith(in)) move(0)
    else if ("south".startsWith(in)) move(1)
    else if ("east".startsWith(in)) move(2)
    else if ("west".startsWith(in)) move(3)
    else if ("up".startsWith(in)) move(4)
    else if ("down".startsWith(in)) move(5)
    else if ("look".startsWith(in)) location ! Room.PrintDescription
    //player inventory
    else if (in.length > 0 && "inventory".startsWith(in)) printInventory
    else if (in.startsWith("inspect")) inspectItem(in.trim.drop(8))
    else if (in.startsWith("get")) location ! Room.GetItem(name, in.trim.drop(4))
    else if (in.startsWith("drop")) getFromInventory(in.trim.drop(5)) match {
      case Some(item) =>
        location ! Room.DropItem(name, item)
        location ! Room.SayMessage("dropped " + item.name + ".", name)
      case None =>
        PrintMessage("You can't drop what you dont have.")
        None
    }
    //player equipment
    //    else if (in.startsWith("equip")) equip(in.drop(6))
    //    else if (in.startsWith("unequip")) unequip(in.drop(8))
    //    else if ("character".startsWith(in)) printEquipment
    //combat commands
    else if (in.startsWith("kill")) kill(in.drop(5))
    else if ("health".startsWith(in)) {
      output.println("Health at: " + health)
    } //player messaging
    else if (in.startsWith("shout")) {
      Main.playerManager ! PlayerManager.PrintShoutMessage(in.drop(6), name)
    } else if (in.startsWith("say")) location ! Room.SayMessage(in.drop(4), name)
    else if (in.startsWith("tell")) tellMessage(in)
    //help command
    else if ("help".startsWith(in)) {
      val source = Source.fromFile("help.txt")
      val lines = source.getLines()
      lines.foreach(output.println)
    } else output.println("What?")
  }
}
object Player {
  case object ProcessInput
  case class PrintMessage(msg: String)
  case class AddToInventory(item: Option[Item])

  val playerHealth = 100.0
}