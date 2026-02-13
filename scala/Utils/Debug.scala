package romanesco.Utils

import java.util.concurrent.atomic.AtomicInteger
import java.time.LocalTime
import java.time.format.DateTimeFormatter

/** 拡張デバッグユーティリティ 投機的並列探索や複雑な帰納法に対応した高度なロガー
  */
object Debug {
  object logger {
    private var enabled = false
    private var maxDepth = 10
    private val currentDepth = ThreadLocal.withInitial(() => 0)

    // スレッドごとの識別子（並列探索の追跡用）
    private val threadId = ThreadLocal.withInitial(() => {
      val id = Thread.currentThread().getName
      if (id.startsWith("ForkJoinPool")) {
        id.split("-").lastOption.getOrElse(id)
      } else id
    })

    private val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

    enum Level:
      case TRACE, INFO, DEBUG, WARN, ERROR

    private var minLevel = Level.DEBUG

    def switch(b: Boolean) = {
      enabled = b
      println(s"[LOGGER] ${if (enabled) "ENABLED" else "DISABLED"}")
    }

    def setLevel(level: Level) = {
      minLevel = level
    }

    def setMaxDepth(depth: Int) = {
      maxDepth = depth
      println(s"[LOGGER] Max log depth set to $depth")
    }

    def increaseDepth() = {
      currentDepth.set(currentDepth.get() + 1)
    }

    def decreaseDepth() = {
      val d = currentDepth.get()
      if (d > 0) currentDepth.set(d - 1)
    }

    def resetDepth() = {
      currentDepth.set(0)
    }

    /** 基本的なログ出力
      */
    def log(s: => String): Unit = log(Level.DEBUG, s)

    def info(s: => String): Unit = log(Level.INFO, s)
    def warn(s: => String): Unit = log(Level.WARN, s)
    def error(s: => String): Unit = log(Level.ERROR, s)
    def trace(s: => String): Unit = log(Level.TRACE, s)

    private def log(level: Level, s: => String): Unit = {
      if (enabled && level.ordinal >= minLevel.ordinal) {
        val depth = currentDepth.get()
        if (depth <= maxDepth) {
          val time = LocalTime.now().format(timeFormatter)
          val tid = threadId.get()
          val indent = "  " * depth
          val levelStr = f"${level.toString}%-5s"

          // 並列実行時はスレッドIDを表示
          val threadInfo = if (tid.length > 0) s"[$tid] " else ""

          println(s"$time $levelStr $threadInfo$indent$s")
        }
      }
    }

    /** 処理時間の計測
      */
    def time[T](name: String)(block: => T): T = {
      if (enabled) {
        val start = System.nanoTime()
        try {
          block
        } finally {
          val end = System.nanoTime()
          info(s"Task '$name' took ${(end - start) / 1000000.0} ms")
        }
      } else {
        block
      }
    }
  }
}
