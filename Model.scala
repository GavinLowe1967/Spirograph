package spirograph

import java.awt.Color
import Model._

/** The model, as seen from the view. */
trait ModelFacadeForView{
  /* The height and width of the model's grid. */
  val height: Int; val width: Int

  /* Get the current list of pieces. */
  def getPieces: List[Piece]

  /** Get the current list of drawn line segments. */ 
  def getLines: List[LineSegment]

  def getCurrentPiece: Piece
}

// =======================================================

class Model extends ModelFacadeForView{
  /** The view. */
  private var app: FrameT = null

  /** Set the view to be `f`. */
  def setFrame(f: FrameT) = app = f

  val height = 100; val width = 100

  // ========= The pieces =========

  /** The current pieces. */
  @volatile private var pieces = List[Piece]()

  def getPieces: List[Piece] = pieces
  // Note: the DrawingPanel needs to obtain pieces while rolling is happening,
  // so another thread holds the synchronized lock.  Hence the above doesn't
  // use "synchronized", but instead a volatile variable.

  private def addPiece(p: Piece) = synchronized{ pieces ::= p }

  /** Get the piece at `(x,y)`, or `null` if there is no such piece. */
  private def pieceAt(v: Vector): Piece = {
    var ps = pieces // ; val v = Vector(x,y)
    while(ps.nonEmpty && !ps.head.contains(v)) ps = ps.tail
    if(ps.nonEmpty) ps.head else null
  }

  /** Make a new wheel of circumference c. */
  def makeNewWheel(c: Int) = synchronized{
    val r = c/(2*Math.PI); val v = Vector(r+0.5,r+1) // initial position
    val p = new Wheel(c, v); addPiece(p)
  }

  // ========= The current piece.

  /** The currently selected piece, if any.  A piece becomes the current piece
    * when it is selected with the mouse.  It stops being the current piece
    * when the "Done" button is pressed, or another piece is selected. */
  @volatile private var currentPiece: Piece = null

  def getCurrentPiece = currentPiece

  /** Move the current piece to (x,y). */
  private def setCurrentPos(v: Vector) = currentPiece.setPosition(v)

  /** Set the current piece to be the one at (x,y), if any.  Return the piece,
    * or `null` if unsuccessful. */
  def selectPieceAt(v: Vector): Piece = synchronized{
    var ps = pieces
    while(ps.nonEmpty && !ps.head.contains(v)) ps = ps.tail
    currentPiece = if(ps.nonEmpty) ps.head else null
    currentPiece 
  }

  /** The distance within which the current piece should be "snapped" to touch
    * the nearest other piece. */
  val SnapDistance = 1.5
  // IMPROVE: this should really be defined in screen units, so depends upon
  // DrawingPanel.scaleF. */

  /** If the current piece is within SnapDistance of another Piece, "snap" it so
    * that it touches. */
  private def maybeSnap() = if(pieces.length > 1){
    // Find closest other in closestP; minD is the distance; minV is the
    // vector to move; minJ will be the join point.
    var minD = Double.MaxValue; var minV: Vector = null
    var closestP: Piece = null; var minJ: Vector = null
    for(p <- pieces; if p != currentPiece){
      val (v, d, j) = currentPiece.vectorTo(p) 
      if(d < minD){ minD = d; closestP = p; minV = v; minJ = j }
    }
    if(minD < SnapDistance) currentPiece.moveByAndCouple(minV, closestP, minJ)
    else currentPiece.uncouple()
  }

  /** Move the current piece, if any, to `v`.  Return the piece if
    * successful. */
  def moveCurrentPiece(v: Vector): Piece = synchronized{
    if(currentPiece != null){ setCurrentPos(v); maybeSnap(); currentPiece }
    else null
  }

  /** Drop the current piece, if any, at (x,y).  Return true if successful. */
  def dropCurrentPiece(v: Vector) = synchronized{ currentPiece = null }

  /** Add a pen to the current piece at position v. */
  def addPen(v: Vector, c: Color) = currentPiece.addPen(v, c)

  def deleteCurrentPiece() = {
    pieces = pieces.filter(_ != currentPiece)
    currentPiece = null
  }

  // ========= Rolling ========

  // Amount to advance in one step.
  private val delta = 0.5

  /** Has the current call of rollCurrentPiece been cancelled? */
  @volatile private var cancelled = false

  /** Roll the current piece.  Return that piece at the end. */
  def rollCurrentPiece(): Piece = synchronized{
    require(currentPiece.isCoupled)
    val (other,v0) = currentPiece.coupling // Coupling to move from
    var v = v0 // Current position
    // Distance so far; and total required.
    var d = 0.0; val total = lcm(currentPiece.perimeter, other.perimeter)
    while(d < total && !cancelled){ 
      v = rollStep(other,v); d += delta; 
      app.repaintDrawingPanel(); Thread.sleep(30)
    }
    cancelled = false // Reset for next time
    currentPiece
  }

  /** Cancel the current rolling. */
  def cancelRoll() = cancelled = true

  /** Roll the current piece for one step. */
  def rollCurrentPieceOneStep() = synchronized{
    require(currentPiece.isCoupled)
    val (other,v) = currentPiece.coupling // Coupling to move from
    rollStep(other,v)
  }

  /** Roll the current piece one step around `other`, starting from the join
    * point at `v`. */
  private def rollStep(other: Piece, v: Vector): Vector = {
    val pens1 = currentPiece.getPens
    // In `other`, the point `delta` advanced from `v`, and a normal vector at
    // that point.
    val (newJoin, norm) = other.advanceFrom(v, delta)
    currentPiece.move(v, delta, norm, newJoin)
    currentPiece.coupleTo(other, newJoin)
    val pens2 = currentPiece.getPens
    // Add the lines
    for(((start,c1), (end,c2)) <- pens1.zip(pens2)){
      assert(c1 == c2); addLine((start, c1, end))
    }
    newJoin
  }


  // ========= Drawn lines =========

  @volatile private var lines = List[LineSegment]()

  private def addLine(ls: LineSegment) = lines ::= ls

  def getLines =  lines
  // Note: the DrawingPanel needs to obtain lines while rolling is happening,
  // so another thread holds the synchronized lock.  Hence the above doesn't
  // use "synchronized", but instead a volatile variable.
}

// =======================================================

object Model{
  /** A single line segment (start-point, colour, end-point). */
  type LineSegment = (Vector, Color, Vector)

  /** The greatest common divisor of `a` and `b`. */
  private def gcd(a: Int, b: Int) = {
    require(a > 0 && b > 0)
    var x = a; var y = b // Inv: gcd(x,y) = gcd(a,b)
    while(y > 0){
      // x, y = y, x%y
      val t = y; y = x%y; x = t
    }
    x
  }

  /** The last common multiplier of `a` and `b`. */
  private def lcm(a: Int, b: Int) = a*b/gcd(a,b)
}
