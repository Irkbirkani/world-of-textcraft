package TextMUD

class Warrior extends Class {
  
  def classCommands(in: String, lvl: Int): Unit = {
      
  }
  val name = "Warrior"
  
  var _stamina = 100
  def stamina = _stamina
  
  var _classPower = 100
  def classPower = _classPower
  
  var _dmgReduc = 25
  def dmgRedu = _dmgReduc
  
  var _hlthInc = 25
  def hlthInc = _hlthInc
  
}