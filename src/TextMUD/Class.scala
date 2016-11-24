package TextMUD

trait Class {
  def classCommands(in: String, lvl: Int): Unit
  val name:String
  var _stamina: Int
  var _classPower: Int
  var _dmgReduc: Int
  var _hlthInc: Int
}