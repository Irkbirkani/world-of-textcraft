package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef

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
      println("received kill cmnd for "+c.path.name)
      var victim = c
      Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
    case AttackNow =>
      println("Received Attack Now")
      victim.foreach(c => c ! SendDamage(location, attack, c))
      println("Sent Send Damage")
    case SendDamage(loc, dmg, c) =>
      println("Received Send Damage "+ dmg)
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, Alive)
        println("Sent Damage Taken "+ realDamage)
        if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(speed, AttackNow)
        }
        if (!Alive) {
          location ! Room.HasDied(self, name)
          Main.activityManager ! ActivityManager.Enqueue(300, Respawn)
        }
      }
    case DamageTaken(dmg, alive) =>
      println("Received Damage Taken")
      if (alive) {
        kill(victim.get.path.name)
      } else {
        victim = None
      }
    case Respawn =>
      println("received Respawn")
      location ! Room.EnterRoom(self, name)
  }

  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom(pl)
  }

  var Alive = true

  def dmgReduction = armor * armorReduc

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double) = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    _health = -actDmg
    if (health <= 0) Alive = false
    actDmg - dmgReduction
  }

  def move(direction: Int): Unit = {
    if (victim.isEmpty) {
      location ! Room.GetExit(direction)
    }
  }

}

object NPC {
  def apply(n: xml.Node): Unit = {
    Main.npcManager ! NPCManager.NewNPC((n \ "@name").text,
      n.text.toDouble,
      (n \ "@location").text,
      (n \ "@attack").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@speed").text.toInt)
  }
  case object RequestMove
  val moveTime = 100

}