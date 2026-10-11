package spirograph

import scala.swing._
import swing.event._
import java.awt.{Color,Font,Graphics2D, Point}
import DrawingPanel._

/** The main panel for drawing. */
class DrawingPanel(model: Model) extends Panel{

  // ========= Coordinate conversion

  /* Note: model uses cartesian coordinates (Doubles); here we use screen
   * coordinates (Ints, with y increasing down the page. */ 

  /** Scala factor: the number of screen units per logical grid unit. */
  private var scaleF = 10.0

  /* Convert logical coordinates to screen coordinates. */
  def gridToScreenX(x: Double) = (scaleF*x).toInt
  def gridToScreenY(y: Double) = (scaleF*(model.height-y)).toInt
  def lengthToScreen(l: Double) = (scaleF*l).toInt

  /* Size of this panel. */
  val width = lengthToScreen(100.0); val height = width
  preferredSize = new Dimension(width, height)

  /** The model coordinates corresponding to screen coordinates `(x,y)`. */
  def getCoords(x: Int, y: Int): (Double, Double) = 
    (x/scaleF, model.height-y/scaleF)

  /** Model coordinates for `e`. */
  def getCoords(e: MouseEvent): (Double, Double) = {
    val p = e.point; getCoords(p.x, p.y)
  }

  // ========== Painting

  border = Swing.EtchedBorder

  /** Paint this panel. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g) 

    for(piece <- model.getPieces) piece match{
      case w: Wheel => 
        val (x,y) = w.position; val r = w.radius; val size = lengthToScreen(2*r)
        g.setColor(WheelColour)
        g.drawOval(gridToScreenX(x-r), gridToScreenY(y+r), size, size)
    }

  }

  // ===== Reactions

  listenTo(mouse.clicks, mouse.moves)

  reactions += {
    case e: MousePressed => // Select piece
      val (x,y) = getCoords(e); model.selectPieceAt(x,y)

    case e: MouseDragged => // Move piece
      val (x,y) = getCoords(e)
      if(model.moveCurrentPiece(x,y)) repaint()

    case e: MouseReleased => // Drop piece
      val (x,y) = getCoords(e)
      if(model.dropCurrentPiece(x,y)) repaint()
  }
}

// =======================================================

object DrawingPanel{
  val WheelColour = Color.blue
}
