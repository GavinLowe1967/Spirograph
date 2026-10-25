package spirograph

import scala.swing._
import java.awt.Color

import Controller._

/** Controller. */
class Controller(model: Model){
  /** The Frame (set by start). */
  private var app: FrameT = null

  /** Redraw the display. */
  private def redisplay() = Swing.onEDT(app.redisplay())

  /** Channel for events to mainLoop thread. */
  private val channel = new UnboundedBuffer[Event] 

  /* ----- Public operations, called by the view. ----- */

  /** Make a new wheel of circumference c. */
  def makeNewWheel(c: Int) = channel!MakeNewWheel(c)
  /** Move the current piece to `v`. */
  def moveCurrentPiece(v: Vector) = channel!MoveCurrentPiece(v)
  /** Select the piece at `v`. */
  def selectPieceAt(v: Vector) = channel!SelectPieceAt(v)
  /** Drop the piece at `v`. */
  def dropPieceAt(v: Vector) = channel!DropPieceAt(v)
  /** Roll the current piece. */
  def roll() = channel!Roll
  /** Roll the current piece. */
  def rollOne() = channel!RollOne
  /** Cancel a roll. */
  def cancelRoll() = channel!CancelRoll
  /** Add pen. */
  def addPen(v: Vector, c: Color) = channel!AddPen(v,c) 

  @volatile private var rolling = false 

  /** Controller for the main loop. */
  private def mainLoop = {
    var done = false
    while(!done){
      channel?() match{
        case MakeNewWheel(c) =>
          model.makeNewWheel(c); redisplay()

        case MoveCurrentPiece(v) if !rolling => 
          val p = model.moveCurrentPiece(v)
          if(p != null) app.movePiece(p)

        case SelectPieceAt(v) if !rolling => 
          val p = model.selectPieceAt(v)
          if(p != null) app.movePiece(p)

        case DropPieceAt(v) if !rolling => 
          model.dropCurrentPiece(v); redisplay()

        case RollOne if !rolling =>
          model.rollCurrentPieceOneStep()
          redisplay()

        case Roll if !rolling => 
          rolling = true
          Concurrency.runThread{ 
            val p = model.rollCurrentPiece()
            app.movePiece(p); rolling = false
          }

        case CancelRoll if rolling =>
          model.cancelRoll()
          // Note: this leads to the execution of the last line of the thread
          // for the `Roll` event, so resets.

        case AddPen(v,c) if !rolling => model.addPen(v, c); redisplay()

        case e => println(s"Event ignored: $e")
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
  /** Event to make new wheel of circumference c. */
  case class MakeNewWheel(c: Int) extends Event
  /** Event to move the current piece to v. */ 
  case class MoveCurrentPiece(v: Vector) extends Event
  /** Event to select the piece at v, if any. */
  case class SelectPieceAt(v: Vector) extends Event
  /** Event to drop the current piece at v. */
  case class DropPieceAt(v: Vector) extends Event
  /** Event to roll the current piece. */
  case object Roll extends Event
  /** Event to roll the current piece one step. */
  case object RollOne extends Event
  /** Even to cancel a roll. */
  case object CancelRoll extends Event
  /** Event to add a pen. */
  case class AddPen(v: Vector, c: Color) extends Event
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

/** An unbounded buffer. */
class UnboundedBuffer[A]{
  /** The buffered values. */
  private val data = new scala.collection.mutable.Queue[A]

  /** Send x on the channel. */
  def !(x: A) = synchronized{ data.enqueue(x); notify() }

  /** Receive on the channel. */
  def ?(u: Unit) : A = synchronized{
    while(data.isEmpty) wait()
    data.dequeue()
  }
}
