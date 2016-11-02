package TextMUD

import akka.actor.ActorRef


object Character {

  case class TakeExit(dir: Option[ActorRef])
  case class KillCmnd(victim:ActorRef)
  case object AttackNow
  case class SendDamage(loc:ActorRef, dmg:Int, c:ActorRef)
  case class DamageTaken(dmg:Int, alive:Boolean)

}