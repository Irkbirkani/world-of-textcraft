package TextMUD
import scala.annotation.tailrec

class BSTMap[A, B](eq: (A, A) => Int) extends scala.collection.mutable.Map[A, B] {
  private class Node(var key: A, var data: B) {
    var left: Node = null
    var right: Node = null
  }

  private var root: Node = null

  def get(key: A): Option[B] = {
    var rover = root
    var c = eq(key, rover.key)
    while (rover != null && c != 0) {
      rover = if (c < 0) rover.left else rover.right
      if (rover != null) c = eq(key, rover.key)
    }
    if (rover == null) None else Some(rover.data)
  }

  def +=(kv: (A, B)) = {
    if (root == null) {
      root = new Node(kv._1, kv._2)
    } else {
      var rover = root
      var parent: Node = null
      var c = eq(kv._1, rover.key)
      while (c != 0 && rover != null) {
        parent = rover
        rover = if (c < 0) rover.left else rover.right
        if (rover != null) c = eq(kv._1, rover.key)
      }
      if (c == 0) {
        rover.key = kv._1
        rover.data = kv._2
      } else if (c < 0) {
        parent.left = new Node(kv._1, kv._2)
      } else {
        parent.right = new Node(kv._1, kv._2)
      }
    }
    this
  }

  def -=(key: A) = {
    def findVictim(n: Node): Node = {
      if (n == null) null
      else {
        val c = eq(key, n.key)
        if (c == 0) {
          if (n.left == null) n.right
          else if (n.right == null) n.left
          else {
            val (key, data, node) = deleteMaxChild(n.left)
            n.left = node
            n.key = key
            n.data = data
            n
          }
        } else if (c < 0) {
          n.left = findVictim(n.left)
          n
        } else
          n.right = findVictim(n.right)
        n
      }
    }
    def deleteMaxChild(n: Node): (A, B, Node) = {
      if (n.right == null) {
        (n.key, n.data, n.left)
      } else {
        val (key, data, node) = deleteMaxChild(n.right)
        n.right = node
        (key, data, n)
      }
    }
    root = findVictim(root)
    this
  }

  def iterator = new Iterator[(A, B)] {
    val stack = new ArrayStack[Node]
    pushRunLeft(root)
    def hasNext: Boolean = !stack.isEmpty
    def next: (A, B) = {
      val n = stack.pop()
      pushRunLeft(n.right)
      n.key -> n.data
    }
    @tailrec def pushRunLeft(n: Node) {
      if (n != null) {
        if (n != null) {
          stack.push(n)
          pushRunLeft(n.left)
        }
      }
    }
  }
}


object BSTMap {
  def apply[A,B](data: (A,B)*)(eq:(A,A)=> Int): BSTMap[A,B] = {
    val bm = new BSTMap[A,B](eq)
    val d = data.sortWith((a,b)=> eq(a._1,b._1)<0).toIndexedSeq
    def binaryAdd(start: Int, end: Int) {
      if (start< end) {
        val mid = (start + end) / 2
        bm += d(mid)
        binaryAdd(start,mid)
        binaryAdd(mid+1,end)
      }
    }
    binaryAdd(0,data.length)
    bm
  }
}