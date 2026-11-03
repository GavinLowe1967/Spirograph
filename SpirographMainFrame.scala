package spirograph

import scala.swing._
import event._
import Swing.{HStrut,VStrut,HGlue}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.{File,IOException}

/** The main frame for the Spirograph program. */
class SpirographMainFrame(model: ModelFacadeForView, controller: Controller)
    extends MainFrame with FrameT with MainFrameT{
  title = "Spirograph"

  /* ========= The components ========= */

  private val state = new ViewState

  /** The drawing panel. */
  private val drawingPanel = new DrawingPanel(model, controller, state)

  /** Main panel containing buttons. */
  private val mainPanel = new FlowPanel{ // BoxPanel(Orientation.Horizontal){
    border = Swing.EtchedBorder
    private val newWheelButton = Button("New wheel"){ showPanel(newWheelPanel) }
    private val showRollsButton = 
      Button("Show rolls"){ showPanel(rollViewPanel) }
    private val saveButton = Button("Save"){ save() }
    contents ++= List(newWheelButton, showRollsButton,  saveButton)
  }

  /** Panel for creating new wheel. */
  private val newWheelPanel: Panel = new FlowPanel{ 
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
          if(wheelSize > 0) controller.makeNewWheel(wheelSize)
          showMainMenu() 
        }
      }
      // IMPROVE: give warning in "else" cases. 
    }
  } // end of newWheelPanel

  /** Panel displaying information about the currently selected piece. */
  private val selectionPanel = 
    new SelectionPanel(model, controller, this, state, drawingPanel)

  /** Panel giving view of the different rolls. */
  private val rollViewPanel = new FlowPanel{
    private val prevButton = Button("Previous"){ println("previous") }
    private val nextButton = Button("Next"){ println("next") }
    private val doneButton = Button("Done"){ showMainMenu() }
    contents ++= List(prevButton, nextButton, doneButton)
  }

  private val buttonPanels: List[Panel] =
    List(mainPanel, newWheelPanel, selectionPanel, rollViewPanel)

  /* ========= Controlling which panels are displayed. ========= */

  /** Show panel `p`. */
  private def showPanel(p: Panel) = 
    for(p1 <- buttonPanels) p1.visible = (p1 == p)

  // /** Display the newWheelPanel, but not the buttons. */
  // private def showNewWheelPanel(): Unit = showPanel(newWheelPanel)

  /** Show the main menu of buttons. */
  def showMainMenu(): Unit =  showPanel(mainPanel)

  /** Show the selection panel. */
  private def showSelectionPanel() = showPanel(selectionPanel)

  showMainMenu()

  /* Create contents. */
  contents = new BoxPanel(Orientation.Vertical) {
    border = Swing.EmptyBorder(10)
    contents ++= drawingPanel :: buttonPanels 
  }

  /** Save the image. */
  private def save() = {
    val bImg = new BufferedImage(
      drawingPanel.width, drawingPanel.height, BufferedImage.TYPE_INT_RGB)
    val cg = bImg.createGraphics(); drawingPanel.paint(cg)
    try{ ImageIO.write(bImg, "JPEG", new File("./output_image.jpg")) } 
    catch { case e: IOException => e.printStackTrace() }
  }
// IMPROVE: prompt for name. 

  /* ========= Public operations ========= */

  def redisplay() = { repaint() }

  /** React to piece `p` being moved.  Give information about it in the
    * selection panel, and redraw main panel. */
  def movePiece(p: Piece) = { 
    showSelectionPanel(); selectionPanel.update(p); drawingPanel.repaint()
  }

  def repaintDrawingPanel() = drawingPanel.repaint()
}
