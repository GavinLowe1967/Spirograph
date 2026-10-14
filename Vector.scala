package spirograph

/** Representation of a vector in two-dimensional space. */
case class Vector(x: Double, y: Double){
  /** This plus `other`. */
  def + (other: Vector) = { val Vector(x1,y1) = other; Vector(x+x1, y+y1) }

  /** This minus `other`. */
  def - (other: Vector) = { val Vector(x1,y1) = other; Vector(x-x1, y-y1) }

  /** This scaled by s. */
  def * (s: Double) = Vector(x*s, y*s)

  /** The length of this. */
  def length = Math.sqrt(x*x+y*y)

  /** A unit vector in the direction of this. */
  def unit = this*(1.0/length)

  /** Convert this to a pair. */
  def asPair = (x,y)
}
