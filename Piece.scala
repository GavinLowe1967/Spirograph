package spirograph

import Piece._
import java.awt.Color

trait Piece{ 
  /** The reference point of this piece, typically the centre. */
  protected var position: Vector 

  /** Get the position of the reference point of this. */
  def getPosition: Vector = position

  /** Set the reference position of this to v. */
  def setPosition(v: Vector) = position = v

  // ========= Operations in subclasses. =========

  /** Add a pen at position `v` and colour `c`. */
  def addPen(v: Vector, c: Color): Unit

  /** Get details about pens.  Successive calls with no intervening `addPen`
    * will return the same order for pens. */
  def getPens: List[(Vector,Color)]

  /** A description of the state of this. */
  def report: String 

  /** Does this piece contain the point v? */
  def contains(v: Vector): Boolean

  /** Is `v` on the perimeter of this, to within Piece.Tolerance? */
  def onPerimeter(v: Vector) : Boolean


  /** Move this by `v`, and couple to `other`. */
  def moveByAndCouple(v: Vector, other: Piece): Unit

  /** A vector between the closest points of this and other, and its length.
    * Moving this by the resulting vector will mean that the two pieces just
    * touch.  */
  def vectorTo(other: Piece): (Vector, Double)

  /** The vector corresponding to moving distance `d` along the perimeter of
    * this from `v` in an anticlockwise direction, and a unit vector normal to
    * the perimeter at that point.  Pre: `onPerimeter(v)`. */
  def advanceFrom(v: Vector, d: Double): (Vector, Vector)

  /** Move and rotate this so that the point `d` units from `v` in a clockwise
    * direction is normal to `norm` (with `norm` pointing inwards) at
    * `newJoin`.  Pre: `norm` is of unit length. */
  def move(v: Vector, d: Double, norm: Vector, newJoin: Vector): Unit

  // ========= Coupling =========

  /** The pieces this is coupled to, and the point of contact.  Invariant: the
    * relation is symmetric: if (p2,v) in p1.coupledTo, then (p1,v) in
    * p2.coupledTo. */
  private var coupledTo = List[(Piece,Vector)]()

  /** Record that other is coupled to this (if not already recorded). */
  private def maybeCoupleTo(other: Piece, v: Vector) = {
    assert(onPerimeter(v), v.toString)
    coupledTo = (other,v) :: coupledTo.filter(_._1 != other)
  }

  /** Couple this to other, touching at `v`, and vice versa. */
  def coupleTo(other: Piece, v: Vector) = { 
    maybeCoupleTo(other,v); other.maybeCoupleTo(this,v)
  }

  /** Record that other is no longer coupled to this. */
  private def uncoupleFrom(other: Piece) = {
    assert(coupledTo.map(_._1).contains(other), coupledTo.toString+"\n"+other)
    coupledTo = coupledTo.filter(_._1 != other)
  }

  /** Uncouple this from another piece. */
  def uncouple() = {
    for((other,_) <- coupledTo) other.uncoupleFrom(this)
    coupledTo = List()
  }

  /** Is this coupled to another piece? */
  def isCoupled = coupledTo.nonEmpty

  /** The piece and touching point that this is coupled to. */
  def coupling: (Piece,Vector) = {
    assert(coupledTo.length == 1) // FIXME!
    coupledTo.head
  }

}

// ============================================================

object Piece{
  /** Tolerance, to allow for rounding errors. */
  val Tolerance = 0.0001
 }
