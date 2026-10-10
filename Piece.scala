package spirograph

trait Piece{ }

object Piece{
  /** A vector in two-dimensional space.  We choose the scale such that each
    * tooth has side length 1.0. */
  type Vector = (Double,Double)
}
