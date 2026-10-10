package spirograph

import scala.swing._
import event._

class SpirographMainFrame(model: Model) extends MainFrame{
  title = "Spirograph"

  private val drawingPanel = new DrawingPanel(model)

  private val ButtonPanelSize = new Dimension(2*model.width, 5*model.height)
  // FIXME: cf DrawingPanel

  private val buttonPanel = new BoxPanel(Orientation.Horizontal){
    border = Swing.EtchedBorder
    contents += Swing.HGlue
    contents += Button("New wheel"){ println("New wheel"); showNewWheelPanel() }
    contents += Swing.Glue
  }

  /** Panel for creating new wheel. */
  private val newWheelPanel = new BoxPanel(Orientation.Vertical){
    private var wheelSize = -1
    object sizeField extends TextField{ columns = 5 }
    contents += Swing.VGlue
    contents += new FlowPanel{
      contents += new Label("Size:"); contents += sizeField
    }
    // contents += Swing.VStrut(10)
    contents += Button("Done"){ doDone() }
    contents += Swing.VGlue

    listenTo(sizeField)
    reactions += {
      case EditDone(`sizeField`) =>
        wheelSize = sizeField.text.toInt // FIXME: catch
        println(s"size = $wheelSize")
    }
    visible = false
// TODO: if wheelSize > 0, create new wheel
    def doDone() = { println("doDone"); visible = false }
  }

  def showNewWheelPanel() = newWheelPanel.visible = true


  contents = new BoxPanel(Orientation.Horizontal) {
    border = Swing.EmptyBorder(10)
    contents += drawingPanel
    contents += new BoxPanel(Orientation.Vertical){
      preferredSize = ButtonPanelSize
      contents += buttonPanel
      contents += newWheelPanel
    }
  }




}
