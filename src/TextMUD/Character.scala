package TextMUD

import akka.actor.ActorRef


object Character {

  case class TakeExit(dir: Option[ActorRef])
  case class KillCmnd(victim:ActorRef)
  case object GiveDamage
  case class SendDamage(loc:ActorRef, dmg:Int)
  case class DamageTaken(dmg:Int)

}