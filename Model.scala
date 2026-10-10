package spirograph

class Model{

  private var pieces = List[Piece]()

  def getPieces = pieces

  def addPiece(p: Piece) = pieces ::= p

}
