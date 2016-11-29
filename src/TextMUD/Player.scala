package TextMUD

import scala.io.Source
import akka.actor.Actor
import java.io.InputStream
import java.io.PrintStream
import akka.actor.ActorRef
import java.io.BufferedReader
import java.net.Socket
import scala.Console._

class Player(
    val name: String,
    val clas: Class,
    private var _level: Int,
    private var _health: Double,
    private var _inventory: MutableDLList[Item],
    val input: BufferedReader,
    val output: PrintStream,
    val sock: Socket) extends Actor {

  _health += clas.hlthInc

  import Player._
  import Character._

  //location access
  private var _location: ActorRef = null
  def location = _location

  //Actor Management
  def receive = {
    case ProcessInput =>
      if (input.ready() && !stunned) {
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
          if (!sneaking) location ! Room.SayMessage("picked up " + item.name + ".", name)
          else output.println("You grabbed " + item.name + "!")
        case None =>
          output.println("Item not found")
      }
    case TakeExit(dir) =>
      dir match {
        case Some(dest) =>
          if (_location != null) location ! Room.LeaveRoom(self, name, sneaking)
          _location = dest
          location ! Room.EnterRoom(self, name, sneaking)
          location ! Room.PrintDescription
        case None =>
          output.println("You can't go that way")
      }
    case KillCmnd(c) =>
      victim = Some(c)
      if (sneaking) {
        sneaking = false
        location ! Room.EnterRoom(self, name, sneaking)
      }
      if (victim.get == self) {
        output.println("You cannot kill yourself.")
        victim = None
      } else {
        output.println("You are hitting " + c.path.name)
        Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
      }
    case AttackNow =>
      if (isAlive && !stunned) {
        victim.foreach(c => c ! SendDamage(location, damage))
      }
    case SendDamage(loc, dmg) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, isAlive, health.toInt)
        output.println(sender.path.name + " dealt " + realDamage + " damage! Health is at " + health)
        if (!isAlive) {
          clearInventory
          location ! Room.HasDied(self, name)
          sender ! ResetVictim
          sender ! SendExp(pvpXP)
          victim = None
          Main.activityManager ! ActivityManager.Enqueue(50, ResetChar)
        } else if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
        }
      } else {
        sender ! PrintMessage("You are having a hard time finding them.")
      }
    case DamageTaken(dmg, alive, hp) =>
      if (alive && victim.nonEmpty) {
        output.println("You dealt " + dmg + " damage to " + victim.get.path.name + "! " +
          victim.get.path.name + " has " + hp + " health left!")
        kill(victim.get.path.name)
      } else if (victim.nonEmpty) {
        output.println("you killed " + victim.get.path.name + ".")
        victim = None
      }
    case ResetChar =>
      _health = playerHealth
      isAlive = true
      victim = None
      Main.roomManager ! RoomManager.EnterRoom("FirstRoom", self)
    case ResetVictim =>
      victim = None
    case View(name) =>
      name ! Stats
    case Stats =>
      sender ! PrintMessage("Level: " + level + "\nClass: " + clasName)
    case SendExp(xp) =>
      addExp(xp)
    case HealCmnd(pl) =>
      output.println("Healing " + pl.path.name)
      Main.activityManager ! ActivityManager.Enqueue(clas.abilitySpeed, SendHeal(pl))
      println("enqueued heal")
    case SendHeal(c) =>
      val healAmnt = level * clas.abilityPower
      c ! ReceiveHeal(healAmnt)
    case ReceiveHeal(hl) =>
      addHlth(hl)
      output.println("Healed for " + hl + "!")
      sender ! Player.PrintMessage("Healed " + name + " for " + hl + "!")
    case StunCmnd(c) =>
      victim = Some(c)
      if (victim.get == self) {
        output.println("You cannot stun yourself.")
        victim = None
      } else {
        output.println("You stunned " + c.path.name)
        Main.activityManager ! ActivityManager.Enqueue(speed, SendStun(victim.get))
        kill(victim.get.path.name)
      }
    case SendStun(c) =>
      c ! Stun(self)
    case Stun(c) =>
      stunned = true
      Main.activityManager ! ActivityManager.Enqueue(30, Unstun(c))
      output.println("You've been stunned!")
    case Unstun(c) =>
      stunned = false
      output.println("You're no longer stunned!")
      kill(c.path.name)
    case StartTeleport(rm) =>
      output.println("Teleporting to " + rm + "!")
      Main.activityManager ! ActivityManager.Enqueue(100, Player.Teleport(rm))
    case Teleport(rm) =>
      Main.roomManager ! RoomManager.EnterRoom(rm, self)
    case PoisonCmnd(c) =>
      victim = Some(c)
      if (victim.get == self) {
        output.println("You cannot poison yourself.")
        victim = None
      } else {
        output.println("You poisoned " + c.path.name)
        Main.activityManager ! ActivityManager.Enqueue(speed, SendPoison(c, clas.abilityPower * level))
        kill(victim.get.path.name)
      }
    case SendPoison(c, dmg) =>
      c ! Poisoned(dmg)
    case Poisoned(dmg) =>
      poisoned = true
      output.println("You've been poisoned!")
      if (poisoned) {
        rmvHlth(dmg)
        output.println("Poison did " + dmg + " damage!")
        poison(dmg)
      }
      Main.activityManager ! ActivityManager.Enqueue(100, Unpoison)
    case Unpoison =>
      poisoned = false
    case Sneak =>
      sneaking = true
      output.println("You are sneaking!")
      Main.activityManager ! ActivityManager.Enqueue(600, Unsneak)
    case Unsneak =>
      if (sneaking) output.println("You are no longer sneaking!")
      sneaking = false
  }

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
      output.println("Armor: " + (armor + clas.dmgReduc) +
        "\nDamage: " + damage +
        "\nSpeed: " + speed)
    } else {
      equipment.foreach(c => output.println(c.bodyPart + ": " + c.item.name))
      output.println("Armor: " + (armor + clas.dmgReduc) +
        "\nDamage: " + damage +
        "\nSpeed: " + speed)
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
  def clasName = clas.name
  def printAbilities = {
    output.println("Abilities:")
    clas.abilities.foreach(a => output.println(a._1 + " at level " + a._2))
  }

  //Health Management
  val baseHlth = playerHealth + clas.hlthInc
  def health = _health

  def addHlth(h: Int): Unit = {
    val newHlth = health + h
    if (newHlth > baseHlth) _health = baseHlth else _health = newHlth
  }

  def rmvHlth(dmg: Int) = {
    val newHlth = health - dmg
    if (newHlth <= 0) {
      Main.activityManager ! ActivityManager.Enqueue(50, ResetChar)
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
  private def move(direction: Int): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction)
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
        println(mod)
        println(_newLvlAt)
      }
    }
  }

  def pvpXP = level * modifier * 2

  //Combat Management
  private var victim: Option[ActorRef] = None

  var isAlive = true
  var stunned = false
  var poisoned = false
  var sneaking = false

  def poison(dmg: Int) = {
    var count = 3
    while (poisoned) {
      if (count == 0) {
        Main.activityManager ! ActivityManager.Enqueue(20, Poisoned(dmg))
        count = 3
      } else count -= 1
    }
  }

  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom("kill", pl, self)
  }

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double) = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    val totalDmg = if (actDmg - ((armor + clas.dmgReduc) * armorReduc) < 0) 0 else actDmg - (armor + clas.dmgReduc) * armorReduc
    _health -= totalDmg
    if (_health <= 0) {
      isAlive = false
    }
    totalDmg
  }

  def view(name: String) = {
    location ! Room.CheckInRoom("view", name, self)
  }

  def shortPath(room: String) = {
    Main.roomManager ! RoomManager.ShortPath(location.path.name, room)
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
    else if (in.startsWith("shortPath")) Main.roomManager ! RoomManager.ShortPath(location.path.name, in.drop(10))
    //player inventory
    else if (in.length > 0 && "inventory".startsWith(in)) printInventory
    else if (in.startsWith("inspect")) inspectItem(in.trim.drop(8))
    else if (in.startsWith("get")) location ! Room.GetItem(name, in.trim.drop(4))
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
      output.println(name +
        "\nClass: " + clas.name +
        "\nLocation: " + location.path.name +
        "\nHealth: " + health +
        "\nLevel: " + level +
        "\nEXP till next level: " + (newLvlAt - exp))
    } //combat commands
    else if ("abilities".startsWith(in)) printAbilities
    else if (in.startsWith("view")) view(in.drop(5))
    else if (in.startsWith("kill")) kill(in.drop(5))
    else if ("health".startsWith(in)) output.println("Health at: " + health)
    else if ("flee".startsWith(in) && victim.nonEmpty) {
      victim = None
      location ! Room.GetExit(util.Random.nextInt(5))
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
    } else clas.classCommands(in, this, self)
  }
}
object Player {
  case object ProcessInput
  case class PrintMessage(msg: String)
  case class AddToInventory(item: Option[Item])
  case class StartTeleport(rm: String)
  case class Teleport(rm: String)
  case object Sneak
  case object Unsneak

  val startLvl = 1

  val punchDamage = 3
  val punchSpeed = 10
  val playerHealth = 100.0
}