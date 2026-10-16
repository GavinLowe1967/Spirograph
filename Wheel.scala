package spirograph

import java.awt.Color

/** A wheel with circumference `c` and centre `position`. */
class Wheel(val c: Int, protected var position: Vector) extends Piece{
  /** The radius of this. */
  val radius = c / (2*Math.PI)

  /** The orientation of a reference radius from the x-axis in a clockwise
    * direction. */
  var orientation: Double = 0.0

  private def offset(v: Vector) = v-position

  def report = s"Wheel, circumference $c, centre at ${position.asPair}"

  /** Is `v` on the perimeter of this, to within Piece.Tolerance? */
  def onPerimeter(v: Vector) = 
    Math.abs(offset(v).length-radius) < Piece.Tolerance

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

  /** The point reached by moving distance `d` along the perimeter of this from
    * `v` in an anticlockwise direction, and a unit vector normal to the
    * perimeter at that point.  Pre: `onPerimeter(v)`.  */
  def advanceFrom(v: Vector, d: Double): (Vector,Vector) = {
    val offset = v-position // `v` relative to the centre.
    require(onPerimeter(v), s"v = $v, distance = ${offset.length}")
    val theta = offset.theta + d/radius // New angle wrt centre.
    val norm = Vector.fromPolar(1.0, theta) // Unit normal vector.
    val v1 = position+norm*radius // Point reached.
    (v1,norm)
  }

  /** Move and rotate this so that the point `d` units from `v` in a clockwise
    * direction is normal to `norm` (with `norm` pointing inwards) at
    * `newJoin`.  Pre: `norm` is of unit length. */
  def move(v: Vector, d: Double, norm: Vector, newJoin: Vector) = {
    require(Math.abs(norm.length-1) < Piece.Tolerance) // Check unit vector
    // The point `d` onwards from `v` (clockwise), and the unit vector to this
    // at that point.
    val (v1, norm1) = advanceFrom(v, -d)
    // Rotate to make norm and norm1 antiparallel.
    val dTheta = Math.PI+norm.theta-norm1.theta // Amount to rotate.
    orientation = Vector.normAngle(orientation+dTheta)
    position = newJoin+norm*radius // Move.
  }

  // ======================================================= Pens

  /* Details about each pen is stored using polar coordinates.  A value
   * (r,theta,c) represents a pen at distance r from the centre, at an angle
   * theta relative to the reference radius. and colour c. */  

  private var pens = List[(Double,Double,Color)]()

  def addPen(v: Vector, c: Color) = {
    val offset = v-position; val r = offset.length
    val theta = Vector.normAngle(offset.theta-orientation)
    pens ::= (r,theta,c)
  }

  // Test
  addPen(position+Vector(radius*0.9, 0.0), Color.red)

  def getPens: List[(Vector,Color)] = 
    pens.map{ case (r,theta,c) => 
      (position + Vector.fromPolar(r, theta+orientation), c)
    }

}



object Wheel{ }
