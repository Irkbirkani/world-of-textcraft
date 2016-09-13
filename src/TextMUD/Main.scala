package TextMUD

import io.StdIn._

object Main extends App {
  val player = new Player(Room.rooms(0), List())

  Room.rooms(0).printDescription
  var input = ""
  while (input != "quit") {
    input = readLine()
    player.processCommand(input)
  }
}