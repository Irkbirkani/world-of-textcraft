package entities

import akka.actor.ActorRef

object Character {

  val armorReduc = 0.1
  //Exit CCs
  case class TakeExit(dir: Option[ActorRef])
  //view CCs
  case class View(name: ActorRef)
  case object Stats
  //cooldown
  case class Cooldown(cd: Int)
  //kill CCs
  case class KillCmnd(victim: ActorRef)
  case object AttackNow
  case class SendDamage(loc: ActorRef, dmg: Double)
  case class SendExp(xp: Int)
  case class DamageTaken(dmg: Double, alive: Boolean, Health: Int)
  case object ResetChar
  case object ResetVictim
  //heal CCs
  case class HealCmnd(player: ActorRef)
  case class SendHeal(c: ActorRef)
  case class ReceiveHeal(hl: Int)
  //stun/feeze CCs
  case class StunCmnd(victim: ActorRef)
  case class Stun(victim: ActorRef)
  case class SendStun(ar: ActorRef)
  case class Unstun(victim: ActorRef)
  //poison CCs
  case class PoisonCmnd(victim: ActorRef)
  case class SendPoison(ar: ActorRef, dmg: Int)
  case class Poisoned(dmg: Int)
  case object Unpoison

}