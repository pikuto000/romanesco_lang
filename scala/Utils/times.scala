package romanesco.Utils

import romanesco.Utils.Debug.logger

object times {
  def watch[R](blk: => R): R = {
    val start = System.nanoTime
    val result = blk
    val end = System.nanoTime
    val elapsed = (end - start) / 1000000.0
    logger.log(s"elapsed time $elapsed ms")
    result
  }
}
