package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef
import java.util.function.ToDoubleBiFunction

class NPC(val name: String, var _health: Double, val attack: Int, val armor: Int, val speed: Int) extends Actor {
  def health = _health
  import NPC._
  import Character._
  Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove)

  private var _location: ActorRef = null
  def location = _location

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
      this.move(util.Random.nextInt(6))
      Main.activityManager ! ActivityManager.Enqueue(NPC.moveTime, NPC.RequestMove)
    case KillCmnd(c) =>
      var victim = c
      Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
    case AttackNow =>
      victim.foreach(c => c ! SendDamage(location, attack, c))
      println("Sent Send Damage")
    case SendDamage(loc, dmg, c) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, Alive)
        if (!Alive) {
          location ! Room.HasDied(self, name)
          Main.activityManager ! ActivityManager.Enqueue(450, ResetChar)
          println("Sent Respawn")
          victim = None
          c ! ResetVictim
        } else if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
        }
      }
    case DamageTaken(dmg, alive) =>
      if (alive) {
        kill(victim.get.path.name)
      } else {
        victim = None
      }
    case ResetVictim =>
      victim = None
    case ResetChar =>
      _health = startHlth
      victim = None
      Main.roomManager ! RoomManager.EnterRoom(startLoc, self)
  }

  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom(pl)
  }

  var Alive = true

  def dmgReduction = armor * armorReduc

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double):Double = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    val totalDmg = actDmg - dmgReduction
    _health -= totalDmg
    if (health <= 0) Alive = false
    totalDmg
  }

  def move(direction: Int): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction)
    }
  }

}

object NPC {
  private var startLoc: String = ""
  private var startHlth = 0.0
  def apply(n: xml.Node): Unit = {
    Main.npcManager ! NPCManager.NewNPC((n \ "@name").text,
      (n \ "@health").text.toDouble,
      (n \ "@location").text,
      (n \ "@attack").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@speed").text.toInt)
    startLoc = (n \ "@location").text
    startHlth = (n \ "@health").text.toDouble
  }
  case object RequestMove
  val moveTime = 150

}