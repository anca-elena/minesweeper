package engine.graphics

case class Rectangle(leftUp: Point, width: Float, height: Float) {
  def top: Float = leftUp.y
  def left: Float = leftUp.x
}