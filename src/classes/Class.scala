package classes

import akka.actor.ActorRef
import entities._

trait Class {
  def classCommands(in: String, pl: Player, pla: ActorRef): Unit
  val name: String
  
  val stamina: Int
  
  val classPower: Int
  
  val dmgReduc: Int
  
  val hlthInc: Int
  
//  val gear:Map[Item,
  
  val abilitySpeed:Int
  val abilityPower:Int
  val abilities:Map[String,Int]
}