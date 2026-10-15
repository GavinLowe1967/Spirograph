package spirograph

class Model{
  val height = 100; val width = 100

  /** The current pieces. */
  private var pieces = List[Piece]()

  def getPieces = synchronized{ pieces }

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

/*
    if(currentPiece != null){ 
      // The following is normally, but not always, a no-op. 
      setCurrentPos(v); maybeSnap()
      currentPiece = null; true
// FIXME: it's still the current piece
    }
    else false
  }
 */

  // Amount to advance in one step
  private val delta = 0.5

  def rollCurrentPiece() = synchronized{
    require(currentPiece.isCoupled)
    val (other,v) = currentPiece.coupling // coupling to move from
    //println(s"currentPiece = ${currentPiece.report}")
    //println(s"other = ${other.report}; $v")
    // In other, position advancing delta from v and normal at that point
    val (deltaV,norm) = other.advanceFrom(v, delta)
    // println(s"deltaV = $deltaV")
    val newJoin = v+deltaV // new join point
// TODO: rotate
    currentPiece.setPositionRelativeTo(newJoin,norm)
    currentPiece.coupleTo(other, newJoin)
  }

  def makeNewWheel(s: Int) = synchronized{
    val x = s/Math.PI/2; val v = Vector(x+0.5,x+1) // initial position
    val p = new Wheel(s, v); addPiece(p)
  }
}
