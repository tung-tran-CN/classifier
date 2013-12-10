package com.condenast.ml.util

object LoggingLevel extends Enumeration {
  type Level = Value
  val DEBUG = Value("Debug")
  val INFO = Value("Info")
  val ERROR = Value("Error")
}

class Logger {
  var currentLevel: LoggingLevel.Level = LoggingLevel.INFO
  def log(msg: => String, level: LoggingLevel.Level) = if (level >= currentLevel) println(msg)
  def debug(msg: => String) = log(msg, LoggingLevel.DEBUG)
  def info(msg: => String) = log(msg, LoggingLevel.INFO)
  def error(msg: => String) = log(msg, LoggingLevel.ERROR)
}

object Logger {
  val logger = new Logger()
  def apply() = logger
  def setLevel(level: LoggingLevel.Level) = logger.currentLevel = level
}