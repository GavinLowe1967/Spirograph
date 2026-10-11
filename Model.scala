package spirograph

class Model{
  val height = 100; val width = 100

  /** The current pieces. */
  private var pieces = List[Piece]()

  def getPieces = pieces

  def addPiece(p: Piece) = pieces ::= p

  /** Get the piece at `(x,y)`, or `null` if there is no such piece. */
  private def pieceAt(x: Double, y: Double): Piece = {
    var ps = pieces
    while(ps.nonEmpty && !ps.head.contains(x,y)) ps = ps.tail
    if(ps.nonEmpty) ps.head else null
  }

  /** The currently selected piece, if any. */
  private var currentPiece: Piece = null


  /** Set the current piece to be the one at (x,y), if any.  Return true if
    * successful. */
  def selectPieceAt(x: Double, y: Double): Boolean = synchronized{
    require(currentPiece == null); currentPiece = pieceAt(x,y)
    currentPiece != null
  }


  /** Move the current piece, if any, to (x,y).  Return true if successful. */
  def moveCurrentPiece(x: Double, y: Double): Boolean  = synchronized{
    if(currentPiece != null){ currentPiece.setPosition(x,y); true } 
    else false
  }

  /** Drop the current piece, if any, at (x,y).  Return true if successful. */
  def dropCurrentPiece(x: Double, y: Double): Boolean = synchronized{
    if(currentPiece != null){ 
      currentPiece.setPosition(x,y); currentPiece = null; true 
    }
    else false
  }

  def makeNewWheel(s: Int) = synchronized{
    //require(currentPiece == null)
    val x = s/Math.PI/2
    val p = new Wheel(s); p.setPosition(x+0.5,x+1); addPiece(p)
  }
}
