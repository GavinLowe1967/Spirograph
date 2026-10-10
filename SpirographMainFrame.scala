package spirograph


import scala.swing._

class SpirographMainFrame(model: Model) extends MainFrame{
  title = "Spirograph"

  private val drawingPanel = new DrawingPanel(model)

  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10)
    contents += drawingPanel
  }
}
