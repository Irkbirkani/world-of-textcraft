package TextMUD

import scala.io.Source

class Room(
    val name: String,
    val description: String,
    private var _items: List[Item],
    private val exits: Array[Int]) {

  def items = _items

  def printDescription(): Unit = {
    println(description)
    println("You see: ")
    if (items.length == 0) print("nothing") else for (i <- 0 until items.length) yield println(items(i).name)
  }
  def getItem(itemName: String): Option[Item] = {
    items.find(_.name == itemName) match {
      case Some(item) => {
        _items = _items.filter(_ != item)
        Some(item)
      }
      case None =>
        println("Theres nothing called that in this room")
        None
    }
  }
  def getExit(dir: Int): Option[Int] = {
    if (exits(dir) < 0) None else Some(exits(dir))
  }

  def dropItem(item: Item): Unit = {
    _items = item :: items
  }
}

object Room {
  val rooms = {
    val source = Source.fromFile("map.txt")
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.toInt) {
      apply(lines)
    }
    source.close()
    rooms
  }

  def apply(itr: Iterator[String]): Room = {
    val name = itr.next()
    val description = itr.next()
    var _items = List.fill(itr.next().toInt) {
      new Item(itr.next(), itr.next())
    }
    val exits = (itr.next.split(" +").map(_.toInt))
    new Room(name, description, _items, exits)
  }
}

