package entities

import akka.actor.Actor
import akka.actor.ActorRef
import java.util.function.ToDoubleBiFunction
import room._
import mud._

class NPC(val name: String,
    private var _level: Int,
    private var _health: Double,
    val startLoc: String,
    val attack: Int,
    val armor: Int,
    val speed: Int,
    private var items: List[Item],
    val desc: String) extends Actor {
  def health = _health
  import NPC._
  import Character._
  private var _location: ActorRef = null
  def location = _location

  Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove, self)
  //set start values
  val startHlth = _health
  val startItems = items
  val exp = (startHlth / 2).toInt
  def level = _level

  private var victim: Option[ActorRef] = None

  def receive = {
    case TakeExit(dir) =>
      dir match {
        case Some(dest) =>
          if (_location != null) location ! Room.LeaveRoom(self, name, false)
          _location = dest
          location ! Room.EnterRoom(self, name, false)
        case None =>
      }
    case RequestMove =>
      if (location != null) {
        this.move(util.Random.nextInt(5))
        Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove, self)
      }
    case KillCmnd(c) =>
      var victim = c
      Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow(self), self)
    case AttackNow(send) =>
      if (!stunned) victim.foreach(c => c ! SendDamage(location, attack, send))
    case SendDamage(loc, dmg, send) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        send ! DamageTaken(realDamage, isAlive, health.toInt)
        if (!isAlive) {
          location ! Room.HasDied(self, name)
          Main.activityManager ! ActivityManager.Enqueue(450, ResetChar, self)
          send ! ResetVictim
          send ! SendExp(exp)
          victim = None
          dropItems
          _location = null
        } else if (victim.isEmpty) {
          victim = Some(send)
          Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow(send), self)
        }
      }
    case DamageTaken(dmg, alive, hp) =>
      if (alive && victim.nonEmpty) {
        kill(victim.get.path.name)
      } else {
        victim = None
      }
    case ResetVictim =>
      victim = None
      _health = startHlth
    case ResetChar =>
      _health = startHlth
      isAlive = true
      items = startItems
      victim = None
      Main.roomManager ! RoomManager.EnterRoom(startLoc, self)
    case View(name) =>
      name ! Stats
    case Stats =>
      sender ! PrintMessage(name + ": " + desc +
        "\r\nLevel: " + level +
        "\r\nHealth: " + health)
    case ReceiveHeal(hl) =>
      addHlth(hl)
      sender ! PrintMessage("Healed " + name + " for " + hl + "!")
    case Stun(c) =>
      stunned = true
      Main.activityManager ! ActivityManager.Enqueue(30, Unstun(c), self)
    case Unstun(c) =>
      stunned = false
      kill(c.path.name)
    case SendDOT(dmg, dotType, send) =>
      _health -= dmg
      send ! DOTTaken(dmg, isAlive, health.toInt, dotType, self)
      send ! CheckDOT
      if (!isAlive) {
        location ! Room.HasDied(self, name)
        Main.activityManager ! ActivityManager.Enqueue(450, ResetChar, self)
        send ! ResetVictim
        send ! SendExp(exp)
        victim = None
        dropItems
        _location = null
        self ! ResetDOT(dotType)
        Main.activityManager ! ActivityManager.Enqueue(50, ResetChar, self)
      }

  }
  //Combat Management
  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom("kill", pl, self)
  }

  var isAlive = true
  var stunned = false

  val armorReduc = 0.1
  def dmgReduction = armor * armorReduc

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double): Double = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    val totalDmg = if (actDmg - (armor * armorReduc) < 0) 0 else actDmg - armor * armorReduc
    _health -= totalDmg
    if (health <= 0) isAlive = false
    totalDmg
  }

  //Health Management
  def addHlth(h: Int): Unit = {
    val newHlth = health + h
    if (newHlth > startHlth) _health = startHlth else _health = newHlth
  }

  def rmvHlth(dmg: Int) = {
    val newHlth = health - dmg
    if (newHlth <= 0) {
      Main.activityManager ! ActivityManager.Enqueue(450, ResetChar, self)
    } else _health -= dmg
  }

  //Movement
  def move(direction: Int): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction, self)
    }
  }

  //Items Management
  def dropItems = {
    def getItems(itemName: String): Option[Item] = {
      this.items.find(_.name == itemName) match {
        case Some(item) => {
          items = items.filter(i => !(i eq item))
          Some(item)
        }
        case None =>
          None
      }
    }
    items.map { itm =>
      getItems(itm.name) match {
        case Some(itm) =>
          location ! Room.DropItem(name, itm)
          location ! Room.SayMessage("dropped " + itm.name + ".", name)
        case None =>
      }
    }
  }
}

object NPC {
  def apply(n: xml.Node): Unit = {
    Main.npcManager ! NPCManager.NewNPC((n \ "@name").text,
      (n \ "@level").text.toInt,
      (n \ "@health").text.toDouble,
      (n \ "@location").text,
      (n \ "@attack").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@speed").text.toInt,
      (n \ "item").map(iNode => Item(iNode)).toList,
      (n \ "@desc").text)
  }
  case object RequestMove
  val moveTime = 150

}