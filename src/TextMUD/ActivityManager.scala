package TextMUD

import akka.actor.Actor
import akka.actor.ActorRef

class ActivityManager extends Actor {
  import ActivityManager._
  private var ticks = 0
  def receive = {
    case Enqueue(delay, message) =>
      val a = new Activity(delay + ticks, sender, message)
      SLqueue.enqueue(a)
    //      BHqueue.enqueue(a)
    case CheckQueue =>
      ticks += 1
      //      while (!BHqueue.isEmpty && BHqueue.peek.time == ticks) {
      //        val act = BHqueue.dequeue()
      //        act.sender ! act.message
      //      }
      while (!SLqueue.isEmpty && SLqueue.peek.time == ticks) {
        val act = SLqueue.dequeue()
        act.sender ! act.message
      }
  }
  private def BHComp(a: Activity, b: Activity): Int = {
    if (a.time == b.time) 0
    else if (a.time > b.time) 1
    else (-1)
  }
  private val BHqueue = new BinaryHeapPriorityQueue[Activity](BHComp)
  private val SLqueue = new SortedListPriorityQueue[Activity](_.time < _.time)
  case class Activity(time: Int, sender: ActorRef, message: Any)
}

object ActivityManager {
  case class Enqueue(delay: Int, meassage: Any)
  case object CheckQueue
}