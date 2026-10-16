package spirograph

import scala.swing._

/** The main Spirograph program. */
object Spirograph{
  def main(args: Array[String]) = {
    val model = new Model; val controller = new Controller(model)
    val frame = new SpirographMainFrame(model, controller)
    model.setFrame(frame); controller.start(frame)
    frame.location = new Point(500,50); frame.pack(); frame.visible = true
  }
}
