object Debug:
  private var status: Boolean = false

  def setDebug(newStatus: Boolean): Unit =
    status = newStatus

  def debug(msg: => String): Unit = if status then println(msg)
