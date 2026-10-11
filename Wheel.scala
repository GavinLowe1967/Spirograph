package spirograph

import Piece.Vector

/** A wheel with circumference `c`. */
class Wheel(val c: Int) extends Piece{
  /** The position of the centre of this wheel. */
  var position: Vector = (0.0, 0.0)

  /** The angle of peak 0 from the x-axis in a clockwise direction. */
  private var orientation: Double = 0.0

  /** The radius of this. */
  val radius = c / (2*Math.PI)

  /** Does this piece contain the point (x,y)? */
  def contains(x: Double, y: Double): Boolean = {
    // println(s"$position $x $y")
    val dx = x-position._1; val dy = y-position._2
    dx*dx+dy*dy <= radius*radius
  }

  /** Set the position of the centre of this to (x,y). */
  def setPosition(x: Double, y: Double) = 
    position = (x,y)

  def report = s"Wheel, circumference $c, centre at $position"
    
}



object Wheel{

}
