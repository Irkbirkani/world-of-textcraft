package mud

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
  private def BHComp(a: Activity, b: Activity): Int = {
    if (a.time == b.time) 0
    else if (a.time > b.time) 1
    else (-1)
  }
//  private val queue = new BinaryHeapPriorityQueue[Activity](BHComp)
  private val queue = new SortedListPriorityQueue[Activity](_.time < _.time)
  case class Activity(time: Int, sender: ActorRef, message: Any)
}

object ActivityManager {
  case class Enqueue(delay: Int, message: Any)
  case object CheckQueue
}