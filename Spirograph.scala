package spirograph

import scala.swing._

/** The main Spirograph program. */
object Spirograph{
  def main(args: Array[String]) = {
    val model = new Model; val frame = new SpirographMainFrame(model)
    frame.location = new Point(500,50); frame.pack(); frame.visible = true
  }
}
