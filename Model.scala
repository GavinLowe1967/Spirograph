package spirograph

class Model{
  val height = 100; val width = 100

  /** The current pieces. */
  private var pieces = List[Piece]()

  def getPieces = pieces

  def addPiece(p: Piece) = pieces ::= p

  /** Get the piece at `(x,y)`, or `null` if there is no such piece. */
  def pieceAt(x: Double, y: Double): Piece = {
    var ps = pieces
    while(ps.nonEmpty && !ps.head.contains(x,y)) ps = ps.tail
    if(ps.nonEmpty) ps.head else null
  }

}
