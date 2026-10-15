package spirograph

/** A wheel with circumference `c` and centre `position`. */
class Wheel(val c: Int, protected var position: Vector) extends Piece{
  /** The radius of this. */
  val radius = c / (2*Math.PI)

  /** The orientation of a reference radius from the x-axis in a clockwise
    * direction. */
  var orientation: Double = 0.0

  private def offset(v: Vector) = v-position

  def report = s"Wheel, circumference $c, centre at ${position.asPair}"

  /** Is `v` (approximately) on the perimeter of this? */
  def onPerimeter(v: Vector) = 
    Math.abs(offset(v).length-radius) < 0.001

  /** Does this piece contain the point v? */
  def contains(v: Vector): Boolean = offset(v).length <= radius

  /** Move this by `v`, and couple to `other`.  Pre: `v` is the vector between
    * the closest points of `this` and `other`. */
  def moveByAndCouple(v: Vector, other: Piece) = {
    position += v
    val join = position + v.unit*radius // Join point, where the two touch.
    coupleTo(other,join)
  }

  /** A vector between the closest points of this and other, and its length.
    * Moving this by the resulting vector will mean that the two pieces just
    * touch. */
  def vectorTo(other: Piece): (Vector, Double) = other match{
    case w: Wheel => 
      val v1 = w.position-position; val sep = v1.length-radius-w.radius
      (v1.unit*sep, math.abs(sep))
  }    

  /** The vector corresponding to moving distance `d` along the perimeter of
    * this from `v` in an anticlockwise direction, and a unit vector normal to
    * the perimeter at that point.  Pre: `onPerimeter(v)`.  */
  def advanceFrom(v: Vector, d: Double): (Vector,Vector) = {
    val offset = v-position // v relative to the centre
    require(onPerimeter(v), s"v = $v, offset = $offset")
    //val theta = offset.theta // angle wrt centre
    val theta1 = offset.theta+d/radius // new angle wrt centre
    val norm = Vector.fromPolar(1.0, theta1)
    val v1 = position+norm*radius-v
    (v1,norm)
  }

  /** Set the position of this so that the perimeter at `v` is normal to `norm`
    * (or unit length), with `norm` pointing inwards. */
  def setPositionRelativeTo(v: Vector, norm: Vector) = {
    require(Math.abs(norm.length-1) < 0.001) // check unit vector
    position = v+norm*radius
  }
}



object Wheel{ }
