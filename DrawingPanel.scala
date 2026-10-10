package spirograph

import scala.swing._
import swing.event._
import java.awt.{Color,Font,Graphics2D, Point}
import DrawingPanel._

class DrawingPanel(model: Model) extends Panel{
  /* The origin of this panel in grid units. */
  private var xOrigin = 0.0; private var yOrigin = 0.0

  /** Scala factor: the number of screen units per logical grid unit. */
  private var scaleF = 5.0

  /* Convert logical coordinates to screen coordinates. */
  def gridToScreenX(x: Double) = (scaleF*(x-xOrigin)).toInt
  def gridToScreenY(y: Double) = (scaleF*(y-yOrigin)).toInt
  def lengthToScreen(l: Double) = (scaleF*l).toInt

  val width = lengthToScreen(100.0); val height = width

  preferredSize = new Dimension(width, height)

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g) 

    for(piece <- model.getPieces) piece match{
      case w: Wheel => 
        val (x,y) = w.position; val r = w.innerRadius
        val size = lengthToScreen(2*r)
        println(s"$x $y $r $size")
        g.setColor(WheelColour)
        g.drawOval(gridToScreenX(x-r), gridToScreenY(y-r), size, size)

    }

  }

}

object DrawingPanel{
  val WheelColour = Color.blue
}
