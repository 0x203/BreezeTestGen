package de.hpi.asg.breezetestgen

import de.uni_potsdam.hpi.asg.common.io.LoggerHelper
import org.apache.logging.log4j.message.{Message, ParameterizedMessageFactory}
import org.apache.logging.log4j.{Logger, LogManager}

// Inspired by:: http://stackoverflow.com/a/981942/2148890

trait Loggable {

  private val logger:Logger = Logging.getLogger(this)

  private def checkFormat(msg:String, refs:Seq[Any]):Message =
    ParameterizedMessageFactory.INSTANCE.newMessage(msg, refs)

  protected def trace(msg:String, refs:Any*) = logger trace checkFormat(msg, refs)

  protected def trace(t:Throwable, msg:String, refs:Any*) = logger trace (checkFormat(msg, refs), t)

  protected def info(msg:String, refs:Any*) = logger info checkFormat(msg, refs)

  protected def info(t:Throwable, msg:String, refs:Any*) = logger info (checkFormat(msg, refs), t)

  protected def warn(msg:String, refs:Any*) = logger warn checkFormat(msg, refs)

  protected def warn(t:Throwable, msg:String, refs:Any*) = logger warn (checkFormat(msg, refs), t)

  protected def error(msg:String, refs:Any*) = logger error checkFormat(msg, refs)

  protected def error(t:Throwable, msg:String, refs:Any*) = logger error (checkFormat(msg, refs), t)

  protected def critical(msg:String, refs:Any*) = logger error checkFormat(msg, refs)

  protected def critical(t:Throwable, msg:String, refs:Any*) = logger error (checkFormat(msg, refs), t)

}

object Logging {
  def initLogger(level: Int = 2,
                 file: java.io.File =  new java.io.File("simulator.log"),
                 debugMode: Boolean = false): Logger = LoggerHelper.initLogger(level, file, debugMode)

  def loggerNameForClass(className: String) = {
    if (className endsWith "$") className.substring(0, className.length - 1)
    else className
  }

  def getLogger(logging: AnyRef) = LogManager.getLogger(loggerNameForClass(logging.getClass.getName))
}