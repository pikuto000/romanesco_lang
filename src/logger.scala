package romanesco

enum LogLevel {
  case Debug, Info, Warn, Error
}

object logger {
  private var enabled = false
  private var minLevel = LogLevel.Info
  
  def switch(enable: Boolean = false, level: LogLevel = LogLevel.Info): Unit = {
    enabled = enable
    minLevel = level
    if (enabled) {
      println(s"Logger enabled (level: $level)")
    }
  }
  
  def debug(msg: => String): Unit = log(LogLevel.Debug, msg)
  def info(msg: => String): Unit = log(LogLevel.Info, msg)
  def warn(msg: => String): Unit = log(LogLevel.Warn, msg)
  def error(msg: => String): Unit = log(LogLevel.Error, msg)
  
  // 後方互換性のため
  def Switch(enable: Boolean): Unit = switch(enable, LogLevel.Debug)
  def log(msg: => String): Unit = debug(msg)
  
  private def log(level: LogLevel, msg: => String): Unit = {
    if (enabled && level.ordinal >= minLevel.ordinal) {
      val prefix = level match {
        case LogLevel.Debug => "[DEBUG]"
        case LogLevel.Info => "[INFO]"
        case LogLevel.Warn => "[WARN]"
        case LogLevel.Error => "[ERROR]"
      }
      println(s"$prefix $msg".replace("\n", "\\n").replace("\r", "\\r"))
    }
  }
}
