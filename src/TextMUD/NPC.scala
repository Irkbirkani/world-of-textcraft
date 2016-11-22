package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef
import java.util.function.ToDoubleBiFunction

class NPC(val name: String, var _health: Double, val attack: Int, val armor: Int, val speed: Int, private var items: List[Item]) extends Actor {
  def health = _health
  import NPC._
  import Character._
  private var _location: ActorRef = null
  def location = _location
  
  Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove)
  
  private var startLoc = ""
  val startHlth = _health
  val startItems = items

  private var victim: Option[ActorRef] = None
  def receive = {
    case TakeExit(dir) =>
      dir match {
        case Some(dest) =>
          if (_location != null) location ! Room.LeaveRoom(self, name)
          _location = dest
          location ! Room.EnterRoom(self, name)
        case None =>
      }
    case RequestMove =>
      println(name + " " + startLoc)
      println(name + " " + startHlth)
      println(name + " " + startItems)
      if (location != null) {
        this.move(util.Random.nextInt(5))
        Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove)
      }
    case KillCmnd(c) =>
      var victim = c
      Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
    case AttackNow =>
      victim.foreach(c => c ! SendDamage(location, attack, c))
    case SendDamage(loc, dmg, c) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, isAlive, health.toInt)
        if (!isAlive) {
          location ! Room.HasDied(self, name)
          Main.activityManager ! ActivityManager.Enqueue(450, ResetChar)
          sender ! ResetVictim
          victim = None
          dropItems
          _location = null
        } else if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
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
      sender ! Player.PrintMessage("Health: " + health +
        "\nArmor: " + armor +
        "\nDamage: " + attack)
    case SetLoc(loc) =>
      startLoc = loc
  }

  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom(pl)
  }

  var isAlive = true

  def dmgReduction = armor * armorReduc

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double): Double = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    val totalDmg = actDmg - dmgReduction
    _health -= totalDmg
    if (health <= 0) isAlive = false
    totalDmg
  }

  def move(direction: Int): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction)
    }
  }
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
      (n \ "@health").text.toDouble,
      (n \ "@location").text,
      (n \ "@attack").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@speed").text.toInt,
      (n \ "item").map(iNode => Item(iNode)).toList)
  }
  case object RequestMove
  case class SetLoc(loc:String)
  val moveTime = 150

}