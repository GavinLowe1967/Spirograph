package spirograph

import scala.swing._
import swing.event._
import java.awt.{Color,Font,Graphics2D, Point}
import DrawingPanel._

/** The main panel for drawing. */
class DrawingPanel(model: Model) extends Panel{

  // ========= Coordinate conversion

  /* Note: model uses cartesian coordinates (reals); here we use screen
   * coordinates (Ints, with y increasing down the page. */ 

  /* The origin of this panel in grid units. */
  // private var xOrigin = 0.0; private var yOrigin = 0.0

  /** Scala factor: the number of screen units per logical grid unit. */
  private var scaleF = 5.0

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
    val p = e.point; /* println((p.x, p.y));*/ getCoords(p.x, p.y)
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

  // Current Piece can be set by clicking on piece, or from selector.  Current
  // piece can be moved by dragging, and placed by dropping.

  listenTo(mouse.clicks, mouse.moves)

  /** The current selected piece. */
  private var currentPiece: Piece = null

  reactions += {
    case e: MousePressed => 
      val (x,y) = getCoords(e)
      currentPiece = model.pieceAt(x,y)
      println(currentPiece)

    case e: MouseDragged =>
      if(currentPiece != null){
        val (x,y) = getCoords(e); currentPiece.setPosition(x,y); repaint()
      }

    case e: MouseReleased => 
      if(currentPiece != null){
        val (x,y) = getCoords(e); currentPiece.setPosition(x,y);
        currentPiece = null; repaint()
      }
  }
}

object DrawingPanel{
  val WheelColour = Color.blue
}
