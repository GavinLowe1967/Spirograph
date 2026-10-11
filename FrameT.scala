package spirograph

trait FrameT{
  /** Redisplay the frame. */
  def redisplay(): Unit

  /** Show the selection panel. */
  def showSelectionPanel(): Unit

  def movePiece(p: Piece): Unit
}
