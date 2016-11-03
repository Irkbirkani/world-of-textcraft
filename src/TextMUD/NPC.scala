package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef

class NPC(val name: String, var _health: Double, val attack:Int, val armor:Int) extends Actor {
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
      Main.activityManager ! ActivityManager.Enqueue(10, AttackNow)
    case AttackNow =>
      victim.foreach(c => c ! SendDamage(location, attack, c))
    case SendDamage(loc, dmg, c) =>
      if (loc == location) {
        val realDamage = takeDamage(dmg)
        sender ! DamageTaken(realDamage, isAlive)
        if (victim.isEmpty) {
          victim = Some(sender)
          Main.activityManager ! ActivityManager.Enqueue(10, AttackNow)
        }
        if (!isAlive) {
          location ! Room.LeaveRoom
        }
      }
    case DamageTaken(dmg, alive) =>
      if (alive) {
        kill(victim.get.path.name)
      } else {
        victim = None
      }
  }
  
  def kill(pl: String): Unit = {
    location ! Room.CheckInRoom(pl)
  }

  var isAlive = true
  
  def dmgReduction = armor*armorReduc

  def d6 = util.Random.nextInt(6) + 1

  def takeDamage(dmg: Double) = {
    val damage = d6
    val actDmg = if (damage == 0) 0
    else if (damage >= 1 && damage <= 5) dmg
    else dmg * 2
    _health = -actDmg
    if (health <= 0) isAlive = false
    actDmg-dmgReduction
  }

  def move(direction: Int): Unit = {
    location ! Room.GetExit(direction)
  }

}

object NPC {
  def apply(n: xml.Node): Unit = {
    Main.npcManager ! NPCManager.NewNPC((n \ "@name").text, 
        n.text.toDouble, 
        (n \ "@location").text,
        (n \ "@attack").text.toInt,
        (n \ "@armor").text.toInt )
  }
  case object RequestMove
  val moveTime = 50

}