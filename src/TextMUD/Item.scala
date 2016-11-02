

package TextMUD

case class Item(name: String, description: String, damage: Int, speed: Int, armor: Int, itype:Int) {

}

object Item {
  def apply(n: xml.Node): Item = {
    new Item((n \ "@name").text,
      n.text, (n \ "@damage").text.toInt,
      (n \ "@speed").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@itype").text.toInt)
  }
  val itemTypeMap = Map("main hand"-> 0, "off hand" -> 1, "two hand" -> 2, "head" -> 3, "chest" -> 4, "legs"-> 5, "misc" -> 6)
}