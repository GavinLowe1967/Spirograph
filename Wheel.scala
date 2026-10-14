package spirograph

//import Piece.Vector

/** A wheel with circumference `c`. */
class Wheel(val c: Int, var position: Vector) extends Piece{
  /** The position of the centre of this wheel. */
//  var position = Vector(0.0, 0.0)

  def getPosition = position

  /** The orientation of a reference radius from the x-axis in a clockwise
    * direction. */
  var orientation: Double = 0.0

  /** The radius of this. */
  val radius = c / (2*Math.PI)

  /** Does this piece contain the point v? */
  def contains(v: Vector): Boolean = (v-position).length <= radius

  /** Set the position of the centre of this to `v`. */
  def setPosition(v: Vector) = position = v

  /** Move this by `v`, and couple to `other`. */
  def moveByAndCouple(v: Vector, other: Piece) = {
    position += v
    // The position where the two touch.
    val join = position + v.unit*radius; println(s"Join point: $join")
    coupleTo(other)
  }

  def report = s"Wheel, circumference $c, centre at ${position.asPair}"

  /** A vector between the closest points of this and other, and its length.
    * Moving this by the resulting vector will mean that the two pieces just
    * touch. */
  def vectorTo(other: Piece): (Vector, Double) = other match{
    case w: Wheel => 
      val v1 = w.position-position; val sep = v1.length-radius-w.radius
      (v1.unit*sep, math.abs(sep))
  }    
}



object Wheel{ }
