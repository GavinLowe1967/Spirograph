package spirograph

import Piece.Vector

/** A wheel with `n` teeth. */
class Wheel(n: Int) extends Piece{
  /** The position of the centre of this wheel. */
  var position: Vector = (0.0, 0.0)

  /** The angle of peak 0 from the x-axis in a clockwise direction. */
  private var orientation: Double = 0.0

  val innerRadius = 0.5 / Math.sin(Math.PI/n)

}



object Wheel{

}
