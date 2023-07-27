package engine.graphics

case class Color(red: Float, green: Float, blue: Float, alpha: Float) {
  def this(red: Float, green: Float, blue: Float) = this(red, green, blue, 255)
}

object Color {
  def apply(red: Float, green: Float, blue: Float): Color = new Color(red, green, blue)
  val Black: Color = Color(  0,   0,   0)
}
