// ==========================================
// SolveTree.scala
// 優先度を持たせない公平な探索実行のための器
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable

/** 公平な探索（speculative parallel search）を表現するデータ構造
  */
enum SolveTree[+T]:
  case Success(value: T)
  case Failure
  case Choice(branches: List[SolveTree[T]])
  case Step(thunk: () => SolveTree[T])
  case Memo(tree: SolveTree[T], onComplete: Boolean => Unit)

  def map[U](f: T => U): SolveTree[U] = this match
    case Success(v) => Success(f(v))
    case Failure    => Failure
    case Choice(bs) => Choice(bs.map(_.map(f)))
    case Step(t)    => Step(() => t().map(f))
    case Memo(t, c) => Memo(t.map(f), c)

  def flatMap[U](f: T => SolveTree[U]): SolveTree[U] = this match
    case Success(v) => f(v)
    case Failure    => Failure
    case Choice(bs) => Choice(bs.map(_.flatMap(f)))
    case Step(t)    => Step(() => t().flatMap(f))
    case Memo(t, c) => Memo(t.flatMap(f), c)

  /** 各ブランチを公平に1ステップずつ進め、成功した結果をLazyListとして返します */
  def solveFair: LazyList[T] = {
    val queue = mutable.Queue[SolveTree[T]](this)
    // Memoノードの追跡用: 各Memoノードの下で成功が見つかったかどうかを管理
    // 本来はスタック構造が必要だが、公平な探索（キュー）では各ノードがどのMemoに属するかを保持する必要がある
    
    // (Node, ParentMemoContext)
    val stateQueue = mutable.Queue[(SolveTree[T], List[MemoTracker])]()
    stateQueue.enqueue((this, Nil))

    class MemoTracker(val onComplete: Boolean => Unit) {
      var successFound = false
      var activeNodes = 1
      def finishNode(): Unit = {
        activeNodes -= 1
        if (activeNodes == 0) onComplete(successFound)
      }
    }

    def loop(): LazyList[T] = {
      if (stateQueue.isEmpty) LazyList.empty
      else {
        val (tree, trackers) = stateQueue.dequeue()
        tree match {
          case Success(v) => 
            trackers.foreach(_.successFound = true)
            trackers.foreach(_.finishNode())
            v #:: loop()
          case Failure =>
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
          case Memo(t, c) =>
            val tracker = new MemoTracker(c)
            stateQueue.enqueue((t, tracker :: trackers))
            loop()
        }
      }
    }
    loop()
  }

    /** 各ブランチを並行して探索し、最初に見つかった成功結果を返します */

    def solveParallel(maxParallelism: Int = 8): LazyList[T] = {

      import java.util.concurrent._

      // solveParallel が呼ばれた際、最上位で solveFair を使って各ブランチに分解し、

      // それらを個別に実行することで Memo の整合性を保つ

      this match {

        case Choice(bs) if bs.nonEmpty =>

          val queue = new LinkedBlockingQueue[Option[T]]()

          val executor = Executors.newFixedThreadPool(math.min(maxParallelism, bs.length))

          val activeCount = new java.util.concurrent.atomic.AtomicInteger(bs.length)

          

          bs.foreach { b =>

            executor.submit(new Runnable {

              def run(): Unit = {

                try {

                  // 各ブランチに対して solveFair を実行

                  // これにより、ブランチ内の Memo は正しく処理される

                  b.solveFair.foreach { v => 

                    queue.put(Some(v))

                  }

                } catch {

                  case _: InterruptedException => // cancelled

                  case e: Exception => 

                    // エラーが発生した場合は無視するかログを出す

                } finally {

                  if (activeCount.decrementAndGet() == 0) {

                    queue.put(None) // 全てのタスクが終了した合図

                  }

                }

              }

            })

          }

          

          def results(): LazyList[T] = {

            try {

              queue.take() match {

                case Some(v) => 

                  v #:: results()

                case None => 

                  executor.shutdown()

                  LazyList.empty

              }

            } catch {

              case _: InterruptedException =>

                executor.shutdownNow()

                LazyList.empty

            }

          }

          results()

  

        case _ => this.solveFair

      }

    }

  

object SolveTree:
  def fromLazyList[T](ll: LazyList[T]): SolveTree[T] =
    if (ll.isEmpty) Failure
    else Choice(List(Success(ll.head), Step(() => fromLazyList(ll.tail))))

  /** 複数の探索ツリーを公平に結合します */
  def merge[T](trees: Iterable[SolveTree[T]]): SolveTree[T] =
    Choice(trees.toList)
