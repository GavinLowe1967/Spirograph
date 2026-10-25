package spirograph

import scala.swing._
import java.awt.Color

trait MainFrameT{
  def showMainMenu(): Unit
}

/** Panel to show the currently selected piece.  The panel also allows the
  * addition of pens, or to roll the current piece. */
class SelectionPanel(model: ModelFacadeForView, controller: Controller,
  mainFrame: MainFrameT, drawingPanel: DrawingPanel)
    extends FlowPanel{
  // Possible states
  val Normal = 0; val Rolling = 1; val AddingPen = 2
  var state = 0

  /** Label for text. */
  private val label = new Label("")

// TODO: have both "cancel" and "stop" buttons when rolling.

  /* Buttons. */
  private val addPenButton = Button("Add pen"){ addPenMode() }
  private val rollButton = Button("Roll"){ startRolling() }
  private val rollOneButton = Button("Roll one step"){ controller.rollOne() }
  private val doneButton = Button("Done"){
    if(state == AddingPen) update(model.getCurrentPiece) 
    else mainFrame.showMainMenu()
  }
  private val cancelButton = Button("Cancel"){ controller.cancelRoll()
  }

  private val buttons =
    List(addPenButton, rollButton, rollOneButton, doneButton, cancelButton)

  /** Make just the buttons in `bs` visible. */
  private def showButtons(bs: List[Button]): Unit = 
    buttons.foreach(b => b.visible = (bs.contains(b)))
  private def showButtons(bs: Button*): Unit = showButtons(bs.toList)

  /* Start with just the "Done" and "Add pen" buttons. */
  showButtons(doneButton, addPenButton)

// IMPROVE: share with drawingPanel
  private var penColour = Color.red

  /** Panel to show the current pen colour. */
  private val colourShower = new Panel{
    preferredSize = new Dimension(40,40)
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g); g.setColor(penColour); g.fillOval(10,10,20,20)
    }
  }

  /** Component to choose the current colour. */
  private val colourChooser = new ColorChooser{
    preferredSize = new Dimension(450,150)
    peer.setPreviewPanel(new javax.swing.JPanel())
    // Use only the DefaultSwatchChooserPanel
    //println(peer.getChooserPanels.mkString("\n"))
    //peer.setChooserPanels(Array(peer.getChooserPanels()(0))) // fragile
    peer.setChooserPanels(
      peer.getChooserPanels().filter(_.toString.contains("Swatch")))
    // That's a hack; but seems the least bad way. 
    reactions += { 
      case event.ColorChanged(s,colour) => 
        penColour = colour; colourShower.repaint()
        drawingPanel.penColour = colour
    }
  }

  colourShower.visible = false; colourChooser.visible = false 

  contents ++= List(label, Swing.HStrut(20)) ++ buttons ++ 
    List(Swing.HStrut(20), colourChooser, colourShower)

  /** Update this panel based on `p` being selected. */
  def update(p: Piece): Unit = {
    state = Normal; label.text = p.report 
    drawingPanel.addingPen = false
    colourShower.visible = false; colourChooser.visible = false
    if(p.isCoupled) 
      showButtons(doneButton, addPenButton, rollButton, rollOneButton)
    else showButtons(doneButton, addPenButton)
  }

  /** Start the current piece rolling. */
  private def startRolling(): Unit = {
    state = Rolling; showButtons(cancelButton)
    drawingPanel.startRolling(); controller.roll()
  }

  /** Switch to mode for adding pen. */
  private def addPenMode(): Unit = {
    state = AddingPen; label.text = "Click to add pen"; showButtons(doneButton)
    colourShower.visible = true; colourChooser.visible = true; drawingPanel.addingPen = true
  }
}
