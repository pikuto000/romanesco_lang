// ==========================================
// SolveTree.scala
// 優先度を持たせない公平な探索実行のための器（並列化・成果物キャッシング対応版）
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

case class FailureReason(message: String, isFatal: Boolean = false)

/** 公平な探索（speculative parallel search）を表現するデータ構造
  */
enum SolveTree[+T]:
  case Success(value: T)
  case Failure(reason: Option[FailureReason] = None)
  case Choice(branches: List[SolveTree[T]])
  case Step(thunk: () => SolveTree[T])
  case DeepStep(thunk: () => SolveTree[T]) 
  case Memo[V](tree: SolveTree[V], onComplete: Option[V] => Unit) extends SolveTree[V]

  def map[U](f: T => U): SolveTree[U] = this match
    case Success(v) => Success(f(v))
    case Failure(r) => Failure(r)
    case Choice(bs) => Choice(bs.map(_.map(f)))
    case Step(t)    => Step(() => t().map(f))
    case DeepStep(t) => DeepStep(() => t().map(f))
    case m: Memo[v] => 
      val mappedTree = m.tree.map(f)
      Memo(mappedTree, (res: Option[U]) => ())

  def flatMap[U](f: T => SolveTree[U]): SolveTree[U] = this match
    case Success(v) => f(v)
    case Failure(r) => Failure(r)
    case Choice(bs) => Choice(bs.map(_.flatMap(f)))
    case Step(t)    => Step(() => t().flatMap(f))
    case DeepStep(t) => DeepStep(() => t().flatMap(f))
    case m: Memo[v] => 
      Memo(m.tree.flatMap(f), _ => ())

  /** 各ブランチを公平に1ステップずつ進め、成功した結果をLazyListとして返します */
  def solveFair: LazyList[T] = {
    val stateQueue = mutable.Queue[(SolveTree[Any], List[MemoTracker[Any]])]()
    stateQueue.enqueue((this, Nil))

    class MemoTracker[V](val onComplete: Option[V] => Unit) {
      var result: Option[V] = None
      var activeNodes = 1
      def finishNode(): Unit = {
        activeNodes -= 1
        if (activeNodes == 0) onComplete(result)
      }
    }

    def loop(): LazyList[T] = {
      if (stateQueue.isEmpty) LazyList.empty
      else {
        val (tree, trackers) = stateQueue.dequeue()
        tree match {
          case Success(v) => 
            trackers.foreach(_.result = Some(v))
            trackers.foreach(_.finishNode())
            v.asInstanceOf[T] #:: loop()
          case Failure(Some(FailureReason(msg, true))) =>
            // Fatal failure: prune all other branches for this node's trackers
            trackers.foreach(_.activeNodes = 1) // Force finish
            trackers.foreach(_.finishNode())
            loop()
          case Failure(_) =>
            trackers.foreach(_.finishNode())
            loop()
          case Choice(bs) =>
            if (bs.isEmpty) {
              trackers.foreach(_.finishNode())
            } else {
              trackers.foreach(_.activeNodes += bs.length - 1)
              bs.foreach(b => stateQueue.enqueue((b, trackers)))
            }
            loop()
          case Step(t) =>
            stateQueue.enqueue((t(), trackers))
            loop()
          case DeepStep(t) =>
            stateQueue.enqueue((t(), trackers))
            loop()
          case Memo(t, c) =>
            val tracker = new MemoTracker(c)
            stateQueue.enqueue((t, tracker.asInstanceOf[MemoTracker[Any]] :: trackers))
            loop()
        }
      }
    }
    loop()
  }

  /** 各ブランチを並行して探索し、最初に見つかった成功結果を返します。
    */
  def solveParallel(maxParallelism: Int = 8): LazyList[T] = {
    val executor = SolveTree.sharedExecutor
    // Bound the queue to prevent memory leak (Issue 4)
    val resultQueue = new LinkedBlockingQueue[Option[T]](1000)
    val activeTasks = new AtomicInteger(0)
    val isCancelled = new java.util.concurrent.atomic.AtomicBoolean(false)

    class ParallelMemoTracker[V](val onComplete: Option[V] => Unit) {
      private val count = new AtomicInteger(1)
      private val result = new java.util.concurrent.atomic.AtomicReference[Option[V]](None)
      def increment(): Unit = count.incrementAndGet()
      def setResult(v: V): Unit = result.compareAndSet(None, Some(v))
      def decrement(): Unit = {
        if (count.decrementAndGet() == 0) {
          onComplete(result.get())
        }
      }
    }

    def submitTask(tree: SolveTree[Any], trackers: List[ParallelMemoTracker[Any]]): Unit = {
      if (isCancelled.get()) return
      activeTasks.incrementAndGet()
      executor.submit(new Runnable {
        def run(): Unit = {
          try {
            process(tree, trackers)
          } finally {
            trackers.foreach(_.decrement())
            if (activeTasks.decrementAndGet() == 0) {
              resultQueue.put(None)
            }
          }
        }
      })
    }

    def process(node: SolveTree[Any], trackers: List[ParallelMemoTracker[Any]]): Unit = {
      if (isCancelled.get()) return
      node match {
        case Success(v) => 
          trackers.foreach(_.setResult(v))
          resultQueue.put(Some(v.asInstanceOf[T]))
        case Failure(Some(FailureReason(_, true))) =>
          isCancelled.set(true)
          resultQueue.put(None)
        case Failure(_) => // End
        case Choice(bs) =>
          if (bs.nonEmpty) {
            trackers.foreach(t => for (_ <- 1 until bs.length) t.increment())
            bs.foreach(b => submitTask(b, trackers))
          }
        case DeepStep(t) =>
          submitTask(t(), trackers)
        case Step(t) =>
          process(t(), trackers)
        case Memo(t, c) =>
          val tracker = new ParallelMemoTracker(c)
          process(t, tracker.asInstanceOf[ParallelMemoTracker[Any]] :: trackers)
      }
    }

    submitTask(this, Nil)

    def results(): LazyList[T] = {
      resultQueue.take() match {
        case Some(v) => v #:: results()
        case None    => LazyList.empty
      }
    }
    results()
  }

object SolveTree:
  import java.util.concurrent._
  
  // 証明器全体で共有するスレッドプール (ForkJoinPool はワークスティーリングに適している)
  private[core] val sharedExecutor = new ForkJoinPool(
    Runtime.getRuntime().availableProcessors(),
    ForkJoinPool.defaultForkJoinWorkerThreadFactory,
    null,
    true 
  )

  def fromLazyList[T](ll: LazyList[T]): SolveTree[T] =
    if (ll.isEmpty) Failure()
    else Choice(List(Success(ll.head), Step(() => fromLazyList(ll.tail))))

  /** 複数の探索ツリーを公平に結合します */
  def merge[T](trees: Iterable[SolveTree[T]]): SolveTree[T] =
    Choice(trees.toList)
