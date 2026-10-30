package spirograph

import scala.swing._
import swing.event._
import java.awt.{Color,Font,Graphics2D, Point, geom}
import DrawingPanel._

/** The main panel for drawing. */
class DrawingPanel(model: ModelFacadeForView, controller: Controller) 
    extends Panel{

  // ========= Coordinate conversion

  /* Note: model uses cartesian coordinates (Doubles); here we use screen
   * coordinates (Ints, with y increasing down the page. */ 

  /** Round `x` to the nearest `Int`. */
  private def round(x: Double): Int = (x+0.5).toInt

  /** The size of grid for the snap-to-grid feature. */
  private val SnapGridSize = 1.0

  /* Convert logical coordinates to screen coordinates. */
  private def gridToScreenX(x: Double) = round(scaleF*x)
  private def gridToScreenY(y: Double) = round(scaleF*(model.height-y))
  private def lengthToScreen(l: Double) = round(scaleF*l)
  private def toScreen(v: Vector) = (gridToScreenX(v.x), gridToScreenY(v.y))

  /** Round to the nearest multiple of GridSize. */
  private def toGrid(x: Double): Double = 
    ((x+SnapGridSize/2)/SnapGridSize).toInt*SnapGridSize
  private def toGrid(v: Vector): Vector = Vector(toGrid(v.x), toGrid(v.y))

  /* Size of this panel. */
  val width = lengthToScreen(100.0); val height = width
  preferredSize = new Dimension(width, height)

  /** The model coordinates corresponding to screen coordinates `(x,y)`. */
  private def getCoords(x: Int, y: Int) = Vector(x/scaleF, model.height-y/scaleF)

  /** Model coordinates for `e`. */
  private 
  def getCoords(e: MouseEvent): Vector = { val p = e.point; getCoords(p.x, p.y) }
  /** Model coordinates for `e`, rounded to multiple of GridSize. */
  //private def toGrid(e: MouseEvent): Vector = toGrid(getCoords(e))

  // ========== Painting

  border = Swing.EtchedBorder

  /** Should a grid be drawn? */
  private var gridMode = true

  /** Toggle drawing of grid to `b`. */
  def setGridMode(b: Boolean) = { gridMode = b; repaint() }

  /** Gap between grid lines in model units. */
  val GridSize = 4

  /** The drawn radius of each pen, in screen units. */
  val PenRadius = 3

  /** Paint this panel. */
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)     
    // ========= Clear
    g.setColor(BackgroundColour); g.fillRect(0,0, size.width, size.height)
    // ========= Grid lines
    g.setColor(GridColour)
    if(gridMode){
      for(x <- 0 until model.width.ceil.toInt by GridSize){
        val x1 = gridToScreenX(x); g.drawLine(x1,0,x1,height)
      }
      for(y <- 0 until model.height.ceil.toInt by GridSize){
        val y1 = gridToScreenY(y); g.drawLine(0,y1,width,y1)
      }
    }
    // ========= Paint pieces
    for(piece <- model.getPieces) piece match{
      case w: Wheel => 
        val (v @ Vector(x,y), theta) = w.getState; val r = w.radius
        val size = lengthToScreen(2*r); g.setColor(WheelColour)
        // Circumference
        g.drawOval(gridToScreenX(x-r), gridToScreenY(y+r), size, size)
        // Reference radius
        g.drawLine(gridToScreenX(x), gridToScreenY(y), 
          gridToScreenX(x+r*Math.cos(theta)), gridToScreenY(y+r*Math.sin(theta)))
        // Pens
        for((vp,c) <- w.getPens){
          g.setColor(c); val (xp,yp) = toScreen(vp)
          g.fillOval(xp-PenRadius, yp-PenRadius, 2*PenRadius, 2*PenRadius)
        }
    }
    // ========= Paint drawn lines
    for((start,colour,end) <- model.getLines){
      g.setColor(colour)
      val (x1,y1) = toScreen(start); val (x2,y2) = toScreen(end)
      g.drawLine(x1, y1, x2, y2)
    }
  }

  // ===== Reactions

  /** Is this currently rolling? */
  private var rolling = false

  def startRolling() = rolling = true

  def stopRolling() = rolling = false

  /** Is a pen being added?  Set by SelectionPanel. */
  var addingPen = false

  /** Current pen colour.  Set by SelectionPanel. */
  var penColour = Color.red

  /** Is the "snap to grid" mode set? */
  var snapToGrid = false

  /** Model coordinates for `e`, rounded to the grid size if appropriate. */
  private def maybeToGrid(e: MouseEvent): Vector = 
    if(snapToGrid) toGrid(getCoords(e)) else getCoords(e)

  listenTo(mouse.clicks, mouse.moves)

  reactions += {
    case e: MousePressed if !rolling && !addingPen => // Select piece
      val v = getCoords(e); controller.selectPieceAt(v)

    case e: MouseDragged if !rolling  && !addingPen => // Move piece
      val v = maybeToGrid(e); controller.moveCurrentPiece(v)

    case e: MouseReleased if !rolling  => // Drop piece
      if(addingPen){ val v = getCoords(e); controller.addPen(v, penColour) }
      else{ val v = maybeToGrid(e); controller.moveCurrentPiece(v) }
  }
}

// =======================================================

object DrawingPanel{
  /** Scale factor: the number of screen units per logical grid unit. */
  private var scaleF = 10.0
  val BackgroundColour = Color.white
  val WheelColour = Color.blue
  val GridColour = new Color(230,230,230)

}
