package TextMUD

import akka.actor.ActorRef

object Character {

  val armorReduc = 0.1
  case class TakeExit(dir: Option[ActorRef])
  case class KillCmnd(victim: ActorRef)
  case object AttackNow
  case class SendDamage(loc: ActorRef, dmg: Double, c: ActorRef)
  case class DamageTaken(dmg: Double, alive: Boolean)
  case object ResetChar
  case object ResetVictim

}