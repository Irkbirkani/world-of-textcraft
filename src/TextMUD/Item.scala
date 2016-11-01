

package TextMUD

case class Item(name: String, description: String, damage: Int, speed: Int, armor: Int, itype: String) {

}

object Item {
  def apply(n: xml.Node): Item = {
    new Item((n \ "@name").text,
      n.text, (n \ "@damage").text.toInt,
      (n \ "@speed").text.toInt,
      (n \ "@armor").text.toInt,
      (n \ "@itype").text)
  }
  val weapon = "weapon"
  val armor = "armor"
  val misc = "misc"
}