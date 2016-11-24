package TextMUD

import akka.actor.ActorRef

class Priest extends Class {
  def classCommands(in: String, pl: Player, pla: ActorRef) = {
    if (in.startsWith("heal") && pl.level >= 1) heal(pl, in)
    else pla ! Player.PrintMessage("What?")

  }

  def heal(pl: Player, nm: String) = {
    val healAmnt = pl.level * 3
    pl.location ! Room.HealCheck(nm, healAmnt)
  }

  val name = "Priest"

  val stamina = 100

  val classPower = 150

  val dmgReduc = 10

  val hlthInc = 10

}