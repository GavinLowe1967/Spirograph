package spirograph

trait Piece{ 
  /** Does this piece contain the point (x,y)? */
  def contains(x: Double, y: Double): Boolean

  /** Set the position of this to (x,y). */
  def setPosition(x: Double, y: Double): Unit
}

// ============================================================

object Piece{
  /** A vector in two-dimensional space.  We choose the scale such that each
    * tooth has side length 1.0. */
  type Vector = (Double,Double)
}
