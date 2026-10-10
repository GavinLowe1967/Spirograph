package spirograph

import scala.swing._

object Spirograph{

  def main(args: Array[String]) = {
    val wheel = new Wheel(30); wheel.position = (20.0,20.0)
    val model = new Model; model.addPiece(wheel)
    val frame = new SpirographMainFrame(model)
    frame.location = new Point(500,50); frame.pack(); frame.visible = true

  }
}
