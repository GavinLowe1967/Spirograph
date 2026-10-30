package spirograph

import scala.swing._
import java.awt.{Color,Font}

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

//  preferredSize = new Dimension(420,120)
// IMPROVE: link to colourChooser  

  /** Label for text. */
  private val label = new Label("")

// TODO: have both "cancel" and "stop" buttons when rolling.

  /* Buttons. */
  private val addPenButton = Button("Add pen"){ addPenMode() }
  private val rollButton = Button("Roll"){ startRolling() }
  private val rollOneButton = Button("Roll one step"){ controller.rollOne() }
  private val deleteButton = Button("Delete"){ controller.deletePiece() }
  private val doneButton = Button("Done"){
    if(state == AddingPen) update(model.getCurrentPiece) 
    else mainFrame.showMainMenu()
  }
  private val cancelButton = Button("Cancel"){ controller.cancelRoll() }

  private val gridModeCheckBox = new CheckBox("Grid mode"){
    selected = true
    reactions += {
      case e: event.ButtonClicked => drawingPanel.setGridMode(selected)
    }
  }
  private val snapCheckBox = new CheckBox("Snap to grid"){
    reactions += {
      case e: event.ButtonClicked => drawingPanel.snapToGrid = selected
    }
  }

  private val buttons = List(
    addPenButton, rollButton, rollOneButton, 
    deleteButton, doneButton, cancelButton, gridModeCheckBox, snapCheckBox)

  /** Make just the buttons in `bs` visible. */
  private def showButtons(bs: List[AbstractButton]): Unit = 
    buttons.foreach(b => b.visible = (bs.contains(b)))
  private def showButtons(bs: AbstractButton*): Unit = showButtons(bs.toList)

  /* Start with just the "Done", "Delete" and "Add pen" buttons, and the check
   * boxes. */
  showButtons(doneButton, deleteButton, addPenButton, 
    gridModeCheckBox, snapCheckBox)

  /** The current pen colour. */
  private def penColour = drawingPanel.penColour

  /** Panel to show the current pen colour. */
  private val colourShower = new Panel{
    val width = 60; val diameter = 18
    preferredSize = new Dimension(width,55); visible = false
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g);
      SelectionPanel.drawCenteredStrings(
        g, List("Current", "colour"), width/2, 10, Color.black, label.font) 
      g.setColor(penColour); g.fillOval(width/2-diameter/2,34,diameter,diameter)
    }
  }

  /** Component to choose the current colour. */
  private val colourChooser = new ColorChooser{
    preferredSize = new Dimension(420,100); visible = false
    peer.setPreviewPanel(new javax.swing.JPanel())
    // Use only the DefaultSwatchChooserPanel
    //println(peer.getChooserPanels.mkString("\n"))
    //peer.setChooserPanels(Array(peer.getChooserPanels()(0))) // fragile
    peer.setChooserPanels(
      peer.getChooserPanels().filter(_.toString.contains("Swatch")))
    // That's a hack; but seems the least bad way. 
    reactions += { 
      case event.ColorChanged(s,colour) => 
        colourShower.repaint(); drawingPanel.penColour = colour
    }
  }


  /* Set contents. */
  contents ++= List(label, Swing.HStrut(20)) ++ buttons ++ 
    List(Swing.HStrut(20), colourChooser, colourShower)

  /** Update this panel based on `p` being selected. */
  def update(p: Piece): Unit = {
    state = Normal; label.text = p.report 
    drawingPanel.addingPen = false
    colourShower.visible = false; colourChooser.visible = false
    if(p.isCoupled) showButtons(
      doneButton, deleteButton, addPenButton, rollButton, rollOneButton, 
      gridModeCheckBox, snapCheckBox)
    else showButtons(doneButton, deleteButton, addPenButton,
      gridModeCheckBox, snapCheckBox)
  }

  /** Start the current piece rolling. */
  private def startRolling(): Unit = {
    state = Rolling; showButtons(cancelButton)
    drawingPanel.startRolling(); controller.roll()
  }

  /** Switch to mode for adding pen. */
  private def addPenMode(): Unit = {
    state = AddingPen; label.text = "Click to add pen"
    showButtons(doneButton, gridModeCheckBox)
    colourShower.visible = true; colourChooser.visible = true; 
    drawingPanel.addingPen = true
  }
}


// =======================================================

object SelectionPanel{
  /** Draw text with centre at screen coordinates (x,y). */
  def drawCenteredString(
    g: Graphics2D, text: String, x: Int, y: Int, colour: Color, font: Font)
  = {
    val metrics = g.getFontMetrics(font)
    val xx = x - metrics.stringWidth(text) / 2
    val yy = y + metrics.getHeight() / 2 
    g.setFont(font); g.setColor(colour); g.drawString(text, xx, yy)
  }

  /** Display text, one entry per line, with centre-top at (x,y). */
  def drawCenteredStrings(
    g: Graphics2D, text: List[String], x: Int, y: Int, colour: Color, font: Font)
  = {
    val metrics = g.getFontMetrics(font)
    g.setFont(font); g.setColor(colour)
    var yy = y; val deltaY = metrics.getHeight() 
    for(st <- text){
      val xx = x - metrics.stringWidth(st) / 2
      g.drawString(st, xx, yy); yy += deltaY
    }

  }

}
