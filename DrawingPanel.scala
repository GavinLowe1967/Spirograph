package spirograph

import scala.swing._
import swing.event._
import java.awt.{Color,Font,Graphics2D, Point, geom}
import DrawingPanel._

/** The main panel for drawing. */
class DrawingPanel(model: Model, controller: Controller) extends Panel{

  // ========= Coordinate conversion

  /* Note: model uses cartesian coordinates (Doubles); here we use screen
   * coordinates (Ints, with y increasing down the page. */ 

  /** Round `x` to the nearest `Int`. */
  private def round(x: Double): Int = (x+0.5).toInt

  /* Convert logical coordinates to screen coordinates. */
  private def gridToScreenX(x: Double) = round(scaleF*x)
  private def gridToScreenY(y: Double) = round(scaleF*(model.height-y))
  private def lengthToScreen(l: Double) = round(scaleF*l)
  private def toScreen(v: Vector) = (gridToScreenX(v.x), gridToScreenY(v.y))

  /* Size of this panel. */
  val width = lengthToScreen(100.0); val height = width
  preferredSize = new Dimension(width, height)

  /** The model coordinates corresponding to screen coordinates `(x,y)`. */
  private def getCoords(x: Int, y: Int) = Vector(x/scaleF, model.height-y/scaleF)

  /** Model coordinates for `e`. */
  private 
  def getCoords(e: MouseEvent): Vector = { val p = e.point; getCoords(p.x, p.y) }

  // ========== Painting

  border = Swing.EtchedBorder

  // val transformArray = Array(
  //   scaleF, 0.0, 0.0,   0.0, -scaleF, -scaleF*model.height,   0.0, 0.0, 1.0
  // )
  // private val transform = new java.awt.geom.AffineTransform(transformArray)

  /** Paint this panel. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g) 

    // Paint pieces
    for(piece <- model.getPieces) piece match{
      case w: Wheel => 
        val Vector(x,y) = w.getPosition; val r = w.radius; 
        val size = lengthToScreen(2*r); g.setColor(WheelColour)
        // Circumference
        g.drawOval(gridToScreenX(x-r), gridToScreenY(y+r), size, size)
        // Reference radius
        val theta = w.orientation
        g.drawLine(gridToScreenX(x), gridToScreenY(y), 
          gridToScreenX(x+r*Math.cos(theta)), gridToScreenY(y+r*Math.sin(theta)))
    }

    // Paint lines
    for((start,colour,end) <- model.getLines){
      g.setColor(colour)
      val (x1,y1) = toScreen(start); val (x2,y2) = toScreen(end)
      g.drawLine(x1, y1, x2, y2)
    }
  }

  // ===== Reactions

  listenTo(mouse.clicks, mouse.moves)

  reactions += {
    case e: MousePressed => // Select piece
      val v = getCoords(e); controller.selectPieceAt(v)

    case e: MouseDragged => // Move piece
      val v = getCoords(e); controller.moveCurrentPiece(v)

    case e: MouseReleased => // Drop piece
      val v = getCoords(e); controller.moveCurrentPiece(v) //.dropPieceAt(v)
  }
}

// =======================================================

object DrawingPanel{
  /** Scale factor: the number of screen units per logical grid unit. */
  private var scaleF = 10.0

  val WheelColour = Color.blue
}
