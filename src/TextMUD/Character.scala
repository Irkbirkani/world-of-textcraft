package TextMUD

import akka.actor.ActorRef


object Character {

  case class TakeExit(dir: Option[ActorRef])
  case class KillCmnd(victim:ActorRef)

}