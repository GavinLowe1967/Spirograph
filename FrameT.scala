package spirograph

trait FrameT{
  /** Redisplay the frame. */
  def redisplay(): Unit

  /** Show the selection panel. */
  //def showSelectionPanel(): Unit

  /** Move piece `p`. */
  def movePiece(p: Piece): Unit

  /** Repaint the DrawingPanel. */
  def repaintDrawingPanel(): Unit
}
