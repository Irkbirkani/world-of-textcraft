package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef

class ActivityManager extends Actor {
  import ActivityManager._
  private var ticks = 0
  def receive = {
    case Enqueue(delay, message) =>
      val a = new Activity(delay + ticks, sender, message)
      queue.enqueue(a)
    case CheckQueue =>
        ticks += 1
        while (!queue.isEmpty && queue.peek.time == ticks) {
          val act = queue.dequeue()
          act.sender ! act.message
        }
  }

  private val queue: SortedListPriorityQueue[Activity] = new SortedListPriorityQueue[Activity](_.time < _.time)
  case class Activity(time: Int, sender: ActorRef, message: Any)
}

object ActivityManager {
  case class Enqueue(delay: Int, meassage: Any)
  case object CheckQueue
}