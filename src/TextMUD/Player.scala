package TextMUD

import scala.io.Source

class Player(
    private var _location: Room,
    private var _inventory: List[Item]) {
  def inventory = _inventory
  def location = _location

  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.name == itemName) match {
      case Some(item) => {
        _inventory = _inventory.filter(_ != item)
        Some(item)
      }
      case None => println("You can't drop something you don't have")
      None
    }
  }
  def inspectItem(item: String):Unit ={
    for (i <- 0 until inventory.length; if (inventory(i).name == item)) yield println(inventory(i).description) 
  }

  private def move(direction: Int): Boolean = {
    location.getExit(direction) match{
      case Some(direction) => _location = Room.rooms(direction)
      location.printDescription()
      true
      case None => println("You can't go that way")
      false
    }
  }
  def processCommand(in: String) = {
    if ("north".startsWith(in)) move(0)
    else if ("south".startsWith(in)) move(1)
    else if ("east".startsWith(in)) move(2)
    else if ("west".startsWith(in)) move(3)
    else if ("up".startsWith(in)) move(4)
    else if ("down".startsWith(in)) move(5)
    else if ("inventory".startsWith(in)) printInventory
    else if (in.startsWith("inspect")) inspectItem(in.trim.drop(8))
    else if ("look".startsWith(in)) location.printDescription()
    else if (in.startsWith("get")) location.getItem(in.trim.drop(4)) match {
      case Some(item) => addToInventory(item)
      case None => None
    }
    else if (in.startsWith("drop")) getFromInventory(in.trim.drop(5)) match {
      case Some(item) => location.dropItem(item)
      case None => None
    }
    else if ("help".startsWith(in)) {
      val source = Source.fromFile("help.txt")
      val lines = source.getLines()
      lines.foreach(println)
    }
  }
  def addToInventory(item: Item): Unit = {
    _inventory = item :: inventory
  }
  private def emptyInventory: Boolean = {
    inventory.isEmpty
  }
  def printInventory(): Unit = {
    if (emptyInventory == true) println("You have nothing in your bag.")
    else {
      println("You have: ")
      for (i <- 0 until inventory.length) yield println(inventory(i).name)
    }
  }
}