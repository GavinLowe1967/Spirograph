package spirograph

import scala.swing._

import Controller._

/** Controller. */
class Controller(model: Model){
  /** The Frame (set by start). */
  private var app: FrameT = null

  /** Redraw the display. */
  private def redisplay() = Swing.onEDT(app.redisplay())

  /** Channel for events to mainLoop thread. */
  private val channel = new Channel[Event](1)
// IMPROVE: use a one-place channel? 

  /* ----- Public operations, called by the view. ----- */

  /** Move the current piece to (x,y). */
  def moveCurrentPiece(x: Double, y: Double) = channel!MoveCurrentPiece(x,y)
  /** Select the piece at (x,y). */
  def selectPieceAt(x: Double, y: Double) = channel!SelectPieceAt(x,y)
  /** Drop the piece at (x,y). */
  def dropPieceAt(x: Double, y: Double) = channel!DropPieceAt(x,y)

  /** Controller for the main loop. */
  private def mainLoop = {
    var done = false
    while(!done){
      channel?() match{
        case MoveCurrentPiece(x,y) => 
          val p = model.moveCurrentPiece(x,y)
          if(p !=  null) app.movePiece(p)

        case SelectPieceAt(x,y) => 
          if(model.selectPieceAt(x,y)) app.showSelectionPanel()

        case DropPieceAt(x,y) => 
          if(model.dropCurrentPiece(x,y)) redisplay()
      }
    }
  }

  /** Start the controller running, updating on `app`. */
  def start(frame: FrameT) = { app = frame; Concurrency.runThread(mainLoop) }
}

// =======================================================

object Controller{
  /** Events sent to the mainLoop thread. */
  trait Event
  /** Event to move the current piece to (x,y). */ 
  case class MoveCurrentPiece(x: Double, y: Double) extends Event
  /** Event to select the piece at (x,y), if any. */
  case class SelectPieceAt(x: Double, y: Double) extends Event
  /** Event to drop the current piece at (x,y). */
  case class DropPieceAt(x: Double, y: Double) extends Event
}

// =======================================================

/** Helper object to create a new thread. */
object Concurrency{
  /** Run a thread to perform comp. */
  def runThread(comp: => Unit) = 
    new Thread(new Runnable{ def run = comp }).start
}

// =======================================================

/** A buffered channel with capacity `capacity`. */
class Channel[A: scala.reflect.ClassTag](capacity: Int){
  /** The buffered values. */
  private val data = new Array[A](capacity)

  /** Index of the next value to be received. */
  private var start = 0

  /** Number of stored values. */
  private var length = 0

  /* The current contents of the buffer is data[start..start.length), wrapping
   * round. */

  /** Send x on the channel. */
  def !(x: A) = synchronized{
    while(length >= capacity) wait()
    data((start+length)%capacity) = x; length += 1
    notifyAll()
  }

  /** Receive on the channel. */
  def ?(u: Unit) : A = synchronized{
    while(length == 0) wait()
    val result = data(start); start = (start+1)%capacity; length -= 1
    notifyAll(); result
  }

}
