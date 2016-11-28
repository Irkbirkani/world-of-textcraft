package TextMUD

import akka.actor.ActorRef

trait Class {
  def classCommands(in: String, pl: Player, pla: ActorRef): Unit
  val name: String
  val stamina: Int
  val classPower: Int
  val dmgReduc: Int
  val hlthInc: Int
  val abilitySpeed:Int
  val abilityPower:Int
  val abilities:Map[String,Int]
}