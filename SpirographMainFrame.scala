package spirograph

import scala.swing._
import event._
import Swing.{HStrut,VStrut,HGlue}

/** The main frame for the Spirograph program. */
class SpirographMainFrame(model: Model, controller: Controller)
    extends MainFrame with FrameT{
  title = "Spirograph"

  /* ========= The components ========= */

  /** The drawing panel. */
  private val drawingPanel = new DrawingPanel(model, controller)

  /** Panel containing buttons. */
  private val buttonPanel = new BoxPanel(Orientation.Horizontal){
    border = Swing.EtchedBorder
    contents ++= List(HGlue, Button("New wheel"){ showNewWheelPanel() }, HGlue)
  }

  /** Panel for creating new wheel. */
  private val newWheelPanel = new FlowPanel{ 
    object sizeField extends TextField{ columns = 5 }
    contents ++= List(
      new Label("Size:"), sizeField, Swing.HStrut(10), Button("Done"){doDone()}
    )
    // Following triggers twice when "enter" is pressed! This is a known bug.
    listenTo(sizeField)
    reactions += { case EditDone(`sizeField`) => doDone() }

    // Time of last call of doDone.
    private var lastTime = System.currentTimeMillis()

    /* React to the "Done" button being pressed. */
    private def doDone() = { 
      val currentTime = System.currentTimeMillis()
      // Catch the double-trigger bug
      if(currentTime-lastTime > 200){
        val sizeText = sizeField.text
        if(sizeText.nonEmpty && sizeText.forall(_.isDigit)){
          lastTime = currentTime; val wheelSize = sizeText.toInt
          if(wheelSize > 0){
            model.makeNewWheel(wheelSize); drawingPanel.repaint()
// TODO: move this to controller
          }
          showMainMenu() // visible = false; buttonPanel.visible = true
        }
      }
      // IMPROVE: give warning in "else" cases. 
    }
  } // end of newWheelPanel

  /** Panel displaying information about the currently selected piece. */
  private class SelectionPanel extends FlowPanel{
    private val label = new Label(""); 
    private val rollButton = Button("Roll"){ controller.roll() }
    private val rollOneButton = Button("Roll one step"){ controller.rollOne() }
    rollButton.visible = false; rollOneButton.visible = false
    private val doneButton =  Button("Done"){ showMainMenu() }
    contents ++= 
      List(label, Swing.HStrut(15), rollButton, rollOneButton, doneButton)
    /** Update this panel based on `p`. */
    def update(p: Piece) = { 
      label.text = p.report; val allowRolls = p.isCoupled
      rollButton.visible = allowRolls; rollOneButton.visible = allowRolls
    }
  }

  private val selectionPanel = new SelectionPanel

  /* ========= Controlling which panels are displayed. ========= */

  /** Display the newWheelPanel, but not the buttons. */
  private def showNewWheelPanel(): Unit = { 
    newWheelPanel.visible = true; buttonPanel.visible = false
    selectionPanel.visible = false
  }

  /** Show the main menu of buttons. */
  private def showMainMenu(): Unit =  { 
    newWheelPanel.visible = false; buttonPanel.visible = true
    selectionPanel.visible = false
  }

  /** Show the selection panel. */
  private def showSelectionPanel() = { 
    newWheelPanel.visible = false; buttonPanel.visible = false
    selectionPanel.visible = true
  }

  showMainMenu()

  /* Create contents. */
  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10)
    contents ++= List(drawingPanel, buttonPanel, newWheelPanel, selectionPanel)
  }

  /* ========= Public operations ========= */

  def redisplay() = { repaint() }

  /** React to piece `p` being moved.  Give information about it in the
    * selection panel, and redraw main panel. */
  def movePiece(p: Piece) = { 
    showSelectionPanel(); selectionPanel.update(p); drawingPanel.repaint() 
  }
}
