package mud

import scala.reflect.ClassTag

class BinaryHeapPriorityQueue[A: ClassTag](comp: (A, A) => Int) extends PriorityQueue[A] {
  private var heap = new Array[A](10)
  private var end = 1

  def enqueue(obj: A): Unit = {
    if (end >= heap.length) {
      val tmp = new Array[A](heap.length * 2)
      Array.copy(heap, 0, tmp, 0, heap.length)
      heap = tmp
    }
    var bubble = end
    while (bubble > 1 && comp(obj, heap(bubble / 2)) > 0) {
      heap(bubble) = heap(bubble / 2)
      bubble /= 2
    }
    heap(bubble) = obj
    end += 1
  }

  def dequeue(): A = {
    val ret = heap(1)
    end -= 1
    val temp = heap(end)
    heap(end) = heap(0)
    var stone = 1
    var flag = true
    while (flag && stone * 2 < end) {
      var greaterChild = if (stone * 2 + 1 < end && comp(heap(stone * 2 + 1),
        heap(stone * 2)) > 0)
        stone * 2 + 1 else stone * 2
      if (comp(heap(greaterChild), temp) > 0) {
        heap(stone) = heap(greaterChild)
        stone = greaterChild
      } else {
        flag = false
      }
    }
    heap(stone) = temp
    ret
  }

  def peek: A = heap(1)

  def isEmpty: Boolean = end == 1
}

object BinaryHeapPriorityQueue extends App {

  private def BHComp(a: Int, b: Int): Int = {
    if (a == b) 0
    else if (a < b) 1
    else (-1)
  }
  
  val queue = new BinaryHeapPriorityQueue[Int](BHComp)
  
  queue.enqueue(1)
  queue.enqueue(5)
  println(queue.dequeue())
}