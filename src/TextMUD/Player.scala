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
      println(name + " is hitting " + c.path.name)
      victim = Some(c)
      output.println("You are hitting " + c.path.name)
      Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
    case AttackNow =>
      if (victim.get != self) {
        victim.foreach(c => c ! SendDamage(location, damage, c))
      }
    case SendDamage(loc, dmg, c) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, isAlive)
        output.println(c.path.name + " dealt " + dmg + " damage!")
        if (!isAlive) {
          println(name + " Send Damage isAlive " + isAlive)
          Main.activityManager ! ActivityManager.Enqueue(20, Respawn)
          clearInventory
          location ! Room.HasDied(self, name)
          victim = None
          c ! ResetVictim
        } else if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
        }
      } else {
        sender ! PrintMessage("You are having a hard time finding them.")
      }
    case DamageTaken(dmg, alive) =>
      println(name + " Damage taken alive " + alive)
      if (alive) {
        output.println("You dealt " + dmg + " damage to " + victim.get.path.name + "!")
        kill(victim.get.path.name)
      } else {
        output.println("you killed " + victim.get.path.name + ".")
        victim = None
      }
    case Respawn =>
      Main.playerManager ! PlayerManager.NewPlayer(name, Player.playerHealth, "FirstRoom", new MutableDLList[Item](), input, output, sock)
      isAlive = true
    case ResetVictim =>
      victim = None

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

  def clearInventory: Unit = {
    _inventory.map(i => getFromInventory(i.name) match {
      case Some(item) =>
        location ! Room.DropItem(name, item)
        location ! Room.SayMessage("dropped " + item.name + ".", name)
      case None =>
    })
  }

  //Equipment management
  private var _equipment: List[EquippedItem] = List()
  def equipment = _equipment

  def equip(itemName: String): Unit = {
    getFromInventory(itemName) match {
      case Some(item) =>
        val eqItem = new EquippedItem(item.itype, item)
        if (eqItem.bodyPart == Item.chest || eqItem.bodyPart == Item.head || eqItem.bodyPart == Item.legs
          && equipment.count(_.bodyPart == eqItem.bodyPart) == 0) {
          _equipment = eqItem :: _equipment
          output.println(eqItem.item.name + " equipped.")
        } else if (eqItem.bodyPart == Item.hand
          && equipment.count(_.bodyPart == Item.twoHand) == 0
          && equipment.count(_.bodyPart == eqItem.bodyPart) <= 1) {
          _equipment = eqItem :: _equipment
          output.println(eqItem.item.name + " equipped.")
        } else if (eqItem.bodyPart == Item.offHand
          && equipment.count(_.bodyPart == Item.twoHand) == 0
          && equipment.count(_.bodyPart == eqItem.bodyPart) == 0) {
          _equipment = eqItem :: _equipment
          output.println(eqItem.item.name + " equipped.")
        } else if (eqItem.bodyPart == Item.twoHand
          && equipment.count(_.bodyPart == Item.hand) == 0
          && equipment.count(_.bodyPart == Item.offHand) == 0
          && equipment.count(_.bodyPart == eqItem.bodyPart) == 0) {
          _equipment = eqItem :: _equipment
          output.println(eqItem.item.name + " equipped.")
        } else {
          addToInventory(item)
          output.println("Cannot equip that.")
        }
      case None => output.println(itemName + " is not in your inventory.")
    }
  }

  def unequip(itemName: String): Unit = {
    _equipment.find(_.item.name == itemName) match {
      case Some(eq) =>
        _equipment = _equipment.filter(_ != eq)
        addToInventory(eq.item)
        output.println(eq.item.name + " unequipped and added to inventory.")
      case None =>
        output.println(itemName + " not equipped.")
    }
  }

  def printEquipment = {
    if (equipment.length == 0) {
      output.println("Nothing equipped.")
    } else {
      equipment.foreach(c => output.println(c.bodyPart + ": " + c.item.name))
    }
  }

  def armor = {
    var sum = 0
    for (i <- equipment) sum += i.item.armor
    sum
  }

  def damage = {
    if (equipment.isEmpty) {
      punchDamage
    } else {
      var sum = 0
      for (i <- equipment) sum += i.item.damage
      sum
    }
  }

  def speed = {
    if (equipment.isEmpty) {
      punchSpeed
    } else {
      var sum = 0
      for (i <- equipment) sum += i.item.speed
      sum
    }
  }

  //Move Player
  private def move(direction: Int): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction)
    }
  }

  //Combat commands
  var isAlive = true

  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom(pl)
  }

  private var sum = 0

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double) = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    _health -= actDmg
    if (_health <= 0) {
      isAlive = false
      println("is Alive for " + name + " changed to false")
    }
    actDmg - (armor * armorReduc)
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
    }
    //player equipment
    else if (in.startsWith("equip")) equip(in.drop(6))
    else if (in.startsWith("unequip")) unequip(in.drop(8))
    else if ("character".startsWith(in)) printEquipment
    //combat commands
    else if (in.startsWith("kill")) kill(in.drop(5))
    else if ("health".startsWith(in)) output.println("Health at: " + health)
    else if ("flee".startsWith(in) && victim.nonEmpty) location ! Room.GetExit(util.Random.nextInt(5))
    //player messaging
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

  val punchDamage = 3
  val punchSpeed = 10
  val playerHealth = 100.0
}