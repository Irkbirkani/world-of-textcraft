package entities

import adts.MutableDLList
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import mud._
import room._
import scala.io.Source
import scala.Console._

abstract class Player(
    val name: String,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    val input: BufferedReader,
    val output: PrintStream,
    val sock: Socket) {

  import Player._
  import Character._
  import ActivityManager._

  def receive: PartialFunction[Any, Unit]

  //location access
  private var _location: ActorRef = null
  def location = _location
  def newLoc(dest: ActorRef) = _location = dest

  //Actor Management

  //Inventory Management
  def inventory = _inventory

  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.name == itemName) match {
      case Some(item) => {
        _inventory = _inventory.filter(i => !(i eq item))
        Some(item)
      }
      case None =>
        None
    }
  }

  def inspectItem(item: String): Unit = {
    for (i <- 0 until inventory.length; if (inventory(i).name == item)) output.println(inventory(i).description)
  }

  def addToInventory(item: Item): Unit = {
    _inventory += item
  }

  private def emptyInventory: Boolean = {
    inventory.isEmpty
  }

  def printInventory(): Unit = {
    val invSet = inventory.toSet
    if (inventory.length == 0) {
      output.println("You have nothing in your bag.")
    } else {
      for (i <- invSet) {
        if (inventory.count(_ == i) == 1) {
          output.println(i.name)
        } else if (inventory.count(_ == i) > 1) {
          val numItem = inventory.count(_ == i)
          output.println(i.name + "(x" + numItem + ")")
        }
      }
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
      output.println("Armor: " + (armor + dmgReduc) +
        "\r\nDamage: " + damage +
        "\r\nSpeed: " + speed)
    } else {
      equipment.foreach(c => output.println(c.bodyPart + ": " + c.item.name))
      output.println("Armor: " + (armor + dmgReduc) +
        "\r\nDamage: " + damage +
        "\r\nSpeed: " + speed)
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

  //Class Management
  val abilityPower: Int
  val abilitySpeed: Int
  val abilities: Map[String, Int]

  val className: String

  val stamina: Int

  val classPower: Int

  val dmgReduc: Int

  val hlthInc: Int

  def printAbilities = {
    output.println("Abilities:")
    abilities.foreach(a => output.println(a._1 + ": available at level " + a._2))
  }

  var teleCD = false
  var sneakCD = false

  //Health Management
  val baseHlth = playerHealth + hlthInc
  def health = _health

  def resetHlth = {
    _health = playerHealth
  }

  def addHlth(h: Int): Unit = {
    val newHlth = health + h
    if (newHlth > baseHlth) _health = baseHlth else _health = newHlth
  }

  def rmvHlth(dmg: Int, self: ActorRef) = {
    val newHlth = health - dmg
    if (newHlth <= 0) {
      Main.activityManager ! ActivityManager.Enqueue(50, ResetChar, self)
    } else _health -= dmg
  }

  def eat(item: String): Unit = {
    val food = getFromInventory(item)
    food match {
      case Some(fd) =>
        fd.itype match {
          case Item.food =>
            if (health == baseHlth) {
              output.println("Health at max")
              addToInventory(food.get)
            } else if ((_health + fd.food) > baseHlth) {
              _health = baseHlth
              output.println("You ate " + fd.name + ". Health is " + health.toInt)

            } else if ((_health + fd.food) < baseHlth) {
              _health += fd.food
              output.println("You ate " + fd.name + ". Health is " + health.toInt)
            }
          case _ =>
            output.println("You can't eat that.")
            addToInventory(food.get)
        }
      case None =>
        output.println("You cant eat what you don't have!")
    }
  }

  //Move Player
  private def move(direction: Int, pla: ActorRef): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction, pla)
    }
  }

  //Level Management
  def level = _level

  private var _exp = 0
  def exp = _exp

  private var modifier = 10
  def mod = math.pow(modifier, 2).toInt

  private var _newLvlAt: Int = level * mod
  def newLvlAt = _newLvlAt

  def addExp(xp: Int) = {
    _exp = (exp + xp)
    output.println("You gained " + xp + " experience!")
    if (exp >= newLvlAt) {
      _exp = exp % newLvlAt
      _level += 1
      _newLvlAt = level * mod
      output.println("You are now level " + level + "!")
      if (level % 2 != 0) {
        modifier += 1

      }
    }
  }

  def pvpXP = level * modifier * 2

  //Party Management
  //party keeps a map of Player->Location
  import scala.collection.mutable
  private var _party: mutable.Map[ActorRef, ActorRef] = mutable.Map()

  def party = _party

  def setLoc(pl: ActorRef, newLoc: ActorRef) = _party(pl) = newLoc

  def changeLoc(pl: ActorRef, newLoc: ActorRef) = {
    party.foreach(a => a._1 ! ChangeLoc(pl, newLoc))
  }

  def addParty(newParty: mutable.Map[ActorRef, ActorRef]) = {
    _party = party ++ newParty
  }

  def addMem(pl: ActorRef, loc: ActorRef) = {
    _party += (pl -> loc)
  }

  def leaveParty(self: ActorRef) = {
    party.filter(_ != (self -> location)).foreach(p => p._1 ! RemoveMember(self))
    output.println("You left the group.")
    _party = scala.collection.mutable.Map(self -> location)
  }

  def rmvMember(pl: ActorRef) = {
    _party = _party.filter(p => p._1 != pl)
  }

  def printParty = {
    for (p <- party) {
      output.print(makeFstCap(p._1.path.name) + " is at " + p._2.path.name + ".\r\n")
    }
  }

  def partyChat(msg: String, self: ActorRef) = {
    party.filter(p => p._1 != self).foreach(p => p._1 ! PrintMessage({ RESET } + { GREEN } + name + ": " + msg + { RESET }))
  }

  //Combat Management
  private var _victim: Option[ActorRef] = None
  def victim = _victim
  def setVictim(c: Option[ActorRef]) = _victim = c

  private var _isAlive = true
  def isAlive = _isAlive
  def setAlive = _isAlive = !isAlive

  private var _stunned = false
  def stunned = _stunned
  def setStun = _stunned = !stunned

  private var _poisoned = false
  def poisoned = _poisoned
  def setPoisoned = _poisoned = !poisoned

  private var _sneaking = false
  def sneaking = _sneaking
  def setSneak = _sneaking = !sneaking

  def poison(dmg: Int, self: ActorRef) = {
    var count = 3
    while (poisoned) {
      if (count == 0) {
        Main.activityManager ! ActivityManager.Enqueue(20, Poisoned(dmg), self)
        count = 3
      } else count -= 1
    }
  }

  def kill(pl: String, self: ActorRef): Unit = {
    location ! Room.CheckInRoom("kill", pl.toUpperCase(), self)
  }

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double) = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    val totalDmg = if (actDmg - ((armor + dmgReduc) * armorReduc) < 0) 0 else actDmg - (armor + dmgReduc) * armorReduc
    _health -= totalDmg
    if (_health <= 0) {
      setAlive
    }
    totalDmg
  }

  def view(name: String, self: ActorRef) = {
    location ! Room.CheckInRoom("view", name.toUpperCase(), self)
  }

  def shortPath(room: String) = {
    Main.roomManager ! RoomManager.ShortPath(location.path.name, room)
  }

  //Player Tell Messaging
  def tellMessage(s: String): Unit = {
    val Array(_, to, msg) = s.split(" +", 3)
    Main.playerManager ! PlayerManager.PrintTellMessage(to, name, msg)
  }

  def makeFstCap(name: String): String = {
    name.substring(0, 1).toUpperCase + name.substring(1).toLowerCase()
  }

  //Process Player Input
  def processCommand(in: String, self: ActorRef): Unit = {
    //player quit
    if ("quit".startsWith(in)) {
      if (party.size > 1) leaveParty(self)
      location ! Room.LeaveGame(self, name)
      sock.close()
    } //player movement
    else if ("north".startsWith(in)) move(0, self)
    else if ("south".startsWith(in)) move(1, self)
    else if ("east".startsWith(in)) move(2, self)
    else if ("west".startsWith(in)) move(3, self)
    else if ("up".startsWith(in)) move(4, self)
    else if ("down".startsWith(in)) move(5, self)
    else if ("look".startsWith(in)) location ! Room.PrintDescription
    else if (in.startsWith("shortPath")) Main.roomManager ! RoomManager.ShortPath(location.path.name, in.drop(10))
    //player inventory
    else if (in.length > 0 && "inventory".startsWith(in)) printInventory
    else if (in.startsWith("inspect")) inspectItem(in.trim.drop(8))
    else if (in.startsWith("get")) location ! Room.GetItem(name, in.trim.drop(4), self)
    else if (in.startsWith("drop")) getFromInventory(in.trim.drop(5)) match {
      case Some(item) =>
        location ! Room.DropItem(name, item)
        if (!sneaking) location ! Room.SayMessage("dropped " + item.name + ".", name)
      case None =>
        output.println("You can't drop what you dont have.")
    }
    else if (in.startsWith("eat")) eat(in.drop(4))
    //player equipment
    else if (in.startsWith("equip")) equip(in.drop(6))
    else if (in.startsWith("unequip")) unequip(in.drop(8))
    else if ("gear".startsWith(in)) printEquipment
    else if ("character".startsWith(in)) {
      output.println(makeFstCap(name) +
        "\r\nClass: " + className +
        "\r\nLocation: " + location.path.name +
        "\r\nHealth: " + health +
        "\r\nLevel: " + level +
        "\r\nEXP till next level: " + (newLvlAt - exp))
    } //combat commands
    else if ("abilities".startsWith(in)) printAbilities
    else if (in.startsWith("view")) view(in.drop(5), self)
    else if (in.startsWith("kill")) kill(in.drop(5), self)
    else if ("health".startsWith(in)) output.println("Health at: " + health)
    else if ("flee".startsWith(in) && victim.nonEmpty) {
      setVictim(None)
      location ! Room.GetExit(util.Random.nextInt(5), self)
    } //player messaging
    else if (in.startsWith("shout")) {
      Main.playerManager ! PlayerManager.PrintShoutMessage(in.drop(6), name)
    } else if (in.startsWith("say")) location ! Room.SayMessage(in.drop(4), name)
    else if (in.startsWith("tell")) tellMessage(in)
    //party commands
    else if (in.startsWith("invite")) Main.playerManager ! PlayerManager.CheckPlayerExist(in.drop(7), party)
    else if ("party".startsWith(in)) printParty
    else if (in.startsWith("leave")) leaveParty(self)
    else if (in.startsWith("/p")) partyChat(in.drop(3), self)
    //help command
    else if ("help".startsWith(in)) {
      val source = Source.fromFile("help.txt")
      val lines = source.getLines()
      for (i <- lines) {
        if (i.startsWith("~")) {
          val h = i.drop(1)
          output.println(s"${RESET}${GREEN}$h${RESET}")
        } else output.println(i)
      }
    }
  }
}

object Player {

  case object Sneak
  case object Unsneak

  case object SneakCD

  val startLvl = 1

  val punchDamage = 3
  val punchSpeed = 10
  val playerHealth = 100.0
}