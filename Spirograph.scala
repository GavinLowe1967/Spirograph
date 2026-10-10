package spirograph

import scala.swing._

object Spirograph{

  def main(args: Array[String]) = {
    val wheel = new Wheel(40); wheel.position = (20.0,20.0)
    println(wheel.innerRadius)
    val model = new Model; model.addPiece(wheel)
//    val controller = new Controller(model)
    val frame = new SpirographMainFrame(model)
//    controller.init(frame)
    frame.location = new Point(500,50); frame.pack(); frame.visible = true

  }
}
