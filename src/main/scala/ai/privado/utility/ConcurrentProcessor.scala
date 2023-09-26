package ai.privado.utility

import ai.privado.utility.ConcurrentProcessor.{STOP, zContext}
import org.slf4j.LoggerFactory
import org.zeromq.{SocketType, ZContext, ZMQ}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ConcurrentProcessor {
  val zContext = new ZContext()
  zContext.getContext.setMaxSockets(4096)
  val STOP     = "STOP"
}

/** Concurrent processor to process the individual processing of the data in parallel threads for each item of the array
  * returned by {@link ConcurrentProcessor#generateParts()}.
  *
  * This will be done by invoking {@link ConcurrentProcessor#runOnPart()} method from independent Future by passing each
  * item of the array to it.
  *
  * The only precaution you need to take is instead of writing the results to result object directly, push the task of
  * writing result to final result object in a writer queue.
  *
  * This can be done by calling {@link ConcurrentProcessor#addInWriterQueue(command: String, item: Any)} method
  *
  * Writing result will be handled by the independent thread which listens to the queue for a command and its respective
  * item. Writing results with the respective command needs to be handled as part of this
  *
  * {@link ConcurrentProcessor#processCommand(command: String, item: Any, result: R)} method implementation.
  *
  * @param result
  *   \- Result object which needs to be populated with the results.
  * @tparam T
  *   \- Type of the element which will be the source input to {@link ConcurrentProcessor#runOnPart()}
  * @tparam R
  *   \- Type of the result object
  */
abstract class ConcurrentProcessor[T, R](result: R) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  // PUSH socket maintained for each task thread (Future)
  private val queue: ThreadLocal[ZMQ.Socket] = new ThreadLocal[ZMQ.Socket]
  // Unique ZeroMQ inproc:// topic used for each instance of ConcurrentProcessor.
  private val topic: String = UUID.randomUUID().toString

  // generate Array of parts that can be processed in parallel
  def generateParts(): Array[T]
  // setup large data structures, acquire external resources
  def init(): Unit = {}
  // release large data structures and external resources
  def finish(): Unit = {}
  // main function: add desired changes to builder
  def runOnPart(part: T): Unit

  /** This method will be called from the Writer thread to write the results to the result object. How to write the
    * results to result object will be the job of user of this class. One can make use of {@command} to differentiate
    * the ways to write result {@item} to {@result} object
    *
    * @param command
    *   \- Command to differentiate the result processing
    * @param item
    *   \- one or more result object of invocation of the method {@link ConcurrentProcessor#runOnPart()} for each parts.
    *   Which will be queued by calling {@link ConcurrentProcessor#addInWriterQueue(command: String, item: Any)}
    * @param result
    *   \- Final result object which will be helpful for chaining the method calls.
    */
  def processCommand(command: String, item: Any, result: R): Unit

  /** It will create all the parallel tasks for each parts.
    *
    * It will also take care of starting the Writer thread which will take care of writing results to final result
    * object. It will wait for writer thread to finish writing all the items from the queue to result object.
    *
    * @return
    *   \- Result object
    */
  def createAndApply(): R = {
    init()
    val genParts = generateParts()
    Option(genParts) match
      case Some(parts) if parts.size > 0 =>
        val queueWriter = new Thread(Writer)
        queueWriter.start()
        val futures = parts
          .map(part => {
            Future {
              runOnPart(part)
              cleanup()
            }
          })
          .toList
        val allResults: Future[List[Unit]] = Future.sequence(futures)
        Await.result(allResults, Duration.Inf)
        pushStopCommandInQueue()
        queueWriter.join()
      case _ =>
    finish()
    result
  }

  // Push the "STOP" command in writer queue. So that it will come out of processing loop and stop the queue
  private def pushStopCommandInQueue(): Unit = {
    val queue = getQueue()
    queue.send(STOP)
    cleanup()
  }

  /** Factory object for PUSH socket of ZeroMQ, which will maintain separate instance per worker thread.
    * @return
    *   \- ZeroMQ PUSH Socket.
    */
  private def getQueue(): ZMQ.Socket = {
    val temp = queue.get()
    Option(temp) match {
      case Some(socket) =>
        socket
      case None =>
        val socket = zContext.createSocket(SocketType.PUSH)
        socket.connect(s"inproc://${topic}")
        queue.set(socket)
        socket
    }
  }

  // For now it will just close the socket.
  private def cleanup(): Unit = {
    Option(queue.get()) match {
      case Some(socket) =>
        socket.close()
        queue.set(null)
      case _ =>
    }
  }

  /** Connect to ZeroMQ queue topic using PUSH socket and push the {@command} along with the {@item} to be processed
    *
    * @param command
    *   \- Command you want to differentiate the results processing action.
    * @param item
    *   \- result object generated from the processing of {@link ConcurrentProcessor#runOnPart()}
    */
  def addInWriterQueue(command: String, item: Any): Unit = {
    try {
      val queue = getQueue()
      queue.sendMore(command)
      queue.send(serialise(item))
      logger.trace(s"Command '${command}' pushed to queue -> inproc://${topic}")
      logger.trace(s"Item ${item} pushed to queue -> inproc://${topic}")
    } catch {
      case ex: Exception =>
        logger.error(
          s"Error while writing in queue -> inproc://${topic} from thread ${Thread.currentThread().getId()}: ",
          ex
        )
    }
  }

  /** Serialise any object to byte array to be passed through queue
    * @param value
    *   \- Any object to passed through queue as a result item.
    * @return
    *   \- Object serialised into ByteArray
    */
  private def serialise(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos                           = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  /** Deserialize the ByteArray back to Object.
    * @param bytes
    *   \- Array[Byte] to be deserialized
    * @return
    *   \- Deserialized object
    */
  private def deserialise(bytes: Array[Byte]): Any = {
    val ois   = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close()
    value
  }

  /** Independent writer thread which will listen to incoming message (command along with item to be processed) from the
    * PULL Socket queue.
    */
  private object Writer extends Runnable {
    override def run(): Unit = {
      val queueProcessor = zContext.createSocket(SocketType.PULL)
      queueProcessor.bind(s"inproc://${topic}")
      var done = false
      logger.debug(s"Writer started..... for queue -> inproc://${topic}")
      while (!done) {
        val command = queueProcessor.recvStr()
        logger.trace(s"queue -> inproc://${topic} ==> command -> ${command}")
        if (STOP == command) done = true
        else {
          val stream = queueProcessor.recv()
          val item   = deserialise(stream)
          logger.trace(s"queue -> inproc://${topic} ==> item -> ${item}")
          processCommand(command = command, item = item, result = result)
        }
      }
      queueProcessor.close()
      logger.debug(s"Writer done..... for queue -> inproc://${topic}")
    }
  }
}
