package spirograph

import java.awt.Color

// trait ControllerT{
//   def redisplay(): Unit
// }

class Model{
  private var app: FrameT = null

  def setFrame(f: FrameT) = app = f

  val height = 100; val width = 100

  /** The current pieces. */
  @volatile private var pieces = List[Piece]()

  def getPieces = pieces
  // Note: the DrawingPanel needs to obtain pieces while rolling is happening,
  // so another thread holds the synchronized lock.  Hence the above doesn't
  // use "synchronized", but instead a volatile variable.

  def addPiece(p: Piece) = synchronized{ pieces ::= p }

  /** The currently selected piece, if any.  A piece becomes the current piece
    * when it is selected with the mouse.  It stops being the current piece
    * when the "Done" button is pressed, or another piece is selected. */
  private var currentPiece: Piece = null

  /** Move the current piece to (x,y). */
  private def setCurrentPos(v: Vector) = 
    currentPiece.setPosition(v)

  /** Get the piece at `(x,y)`, or `null` if there is no such piece. */
  private def pieceAt(v: Vector): Piece = {
    var ps = pieces // ; val v = Vector(x,y)
    while(ps.nonEmpty && !ps.head.contains(v)) ps = ps.tail
    if(ps.nonEmpty) ps.head else null
  }

  /** Set the current piece to be the one at (x,y), if any.  Return the piece,
    * or `null` if unsuccessful. */
  def selectPieceAt(v: Vector): Piece = synchronized{
    // require(currentPiece == null); 
    currentPiece = pieceAt(v)
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
    // Find closest other
    var minD = Double.MaxValue; var minV: Vector = null
    var closestP: Piece = null
    for(p <- pieces; if p != currentPiece){
      val (v, d) = currentPiece.vectorTo(p) 
      if(d < minD){ minD = d; closestP = p; minV = v }
    }
    if(minD < SnapDistance) currentPiece.moveByAndCouple(minV, closestP)
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

  // Amount to advance in one step.
  private val delta = 0.5

  /** Roll the current piece. */
  def rollCurrentPiece() = synchronized{
    require(currentPiece.isCoupled)
    val (other,v0) = currentPiece.coupling // coupling to move from
    var v = v0
    while(true){ // FIXME
      v = rollStep(other,v); app.redisplay(); Thread.sleep(30)
    }
  }
// TODO: stop above loop when back to start point, or interrupted.

  def rollCurrentPieceOneStep() = synchronized{
    require(currentPiece.isCoupled)
    val (other,v) = currentPiece.coupling // coupling to move from
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


  def makeNewWheel(s: Int) = synchronized{
    val x = s/Math.PI/2; val v = Vector(x+0.5,x+1) // initial position
    val p = new Wheel(s, v); addPiece(p)
  }

  // ========= Drawn lines =========

  /** A single line segment (start-point, colour, end-point). */
  type LineSegment = (Vector, Color, Vector)

  @volatile private var lines = List[LineSegment]()

  private def addLine(ls: LineSegment) = lines ::= ls

  def getLines =  lines
  // Note: the DrawingPanel needs to obtain lines while rolling is happening,
  // so another thread holds the synchronized lock.  Hence the above doesn't
  // use "synchronized", but instead a volatile variable.


}
