package spirograph

import Piece._

trait Piece{ 
  /** Does this piece contain the point v? */
  def contains(v: Vector): Boolean

  def getPosition: Vector

  /** Set the position of this to v. */
  def setPosition(v: Vector): Unit

  /** Move this by `v`, and couple to `other`. */
  def moveByAndCouple(v: Vector, other: Piece): Unit

  /** A description of the state of this. */
  def report: String 

  /** A vector between the closest points of this and other, and its length.
    * Moving this by the resulting vector will mean that the two pieces just
    * touch.  */
  def vectorTo(other: Piece): (Vector, Double)

  /** The pieces this is coupled to.  Invariant: the relation is symmetric: if
    * p2 in p1.coupledTo, then p1 in p2.coupledTo. */
  private var coupledTo = List[Piece]()

  /** Record that other is coupled to this (if not already recorded). */
  private def maybeCoupleTo(other: Piece) = 
    if(!coupledTo.contains(other)) coupledTo ::= other
    // else println("Already coupled")

  /** Couple this to other, and vice versa. */
  protected def coupleTo(other: Piece) = { 
    maybeCoupleTo(other); other.maybeCoupleTo(this)
  }

  /** Record that other is no longer coupled to this. */
  private def uncoupleFrom(other: Piece) = {
    assert(coupledTo.contains(other))
    coupledTo = coupledTo.filter(_ != other)
  }

  /** Uncouple this from another piece. */
  def uncouple() = {
    for(other <- coupledTo) other.uncoupleFrom(this)
    coupledTo = List()
  }

  /** Is this coupled to another piece? */
  def isCoupled = coupledTo.nonEmpty

}

// ============================================================

object Piece{ }
