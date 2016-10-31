package src.TextMUD

import akka.actor.Actor

class ActivityManager extends Actor {
  def receive = {
    case _ =>
  }
  
}

object ActivityManager {
  case object CheckQueue
}