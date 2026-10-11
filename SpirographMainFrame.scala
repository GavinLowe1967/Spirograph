package spirograph

import scala.swing._
import event._

/** The main frame for the Spirograph program. */
class SpirographMainFrame(model: Model) extends MainFrame{
  title = "Spirograph"

  private val drawingPanel = new DrawingPanel(model)

  //private val ButtonPanelSize = new Dimension(2*model.width, 2*model.height)
  // FIXME: cf DrawingPanel

  /** Panel containing buttons. */
  private val buttonPanel = new BoxPanel(Orientation.Horizontal){
    border = Swing.EtchedBorder
    contents += Swing.HGlue
    contents += Button("New wheel"){ showNewWheelPanel() }
    contents += Swing.Glue
  }

  /** Panel for creating new wheel. */
  private val newWheelPanel = new FlowPanel{ 
    object sizeField extends TextField{ columns = 5 }
    contents += new Label("Size:"); contents += sizeField
    contents += Button("Done"){ doDone() }
    visible = false

    // Note: following triggers twice when "enter" is pressed! This is a known 
    // bug.
    // listenTo(sizeField)
    // reactions += {
    //   case EditDone(`sizeField`) => println("editDone"); doDone()
    // }

    /* React to the "Done" button being pressed. */
    def doDone() = { 
      val sizeText = sizeField.text
      if(sizeText.nonEmpty && sizeText.forall(_.isDigit)){
        val wheelSize = sizeText.toInt
        if(wheelSize > 0){ 
          model.makeNewWheel(wheelSize); drawingPanel.repaint() 
        }
        visible = false; buttonPanel.visible = true
      }
      // IMPROVE: give warning in "else" cases. 
    }
  } // end of newWheelPanel

  /** Display the newWheelPanel, but not the buttons. */
  def showNewWheelPanel(): Unit = { 
    newWheelPanel.visible = true; buttonPanel.visible = false 
  }

// TODO: have a panel giving information about the current piece, and a button
// to place it (and return to the button menu.  Needs updating each time the
// piece is moved.

  /* Create contents. */
  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10)
    contents += drawingPanel
    contents += buttonPanel
    contents += newWheelPanel
  }

}
