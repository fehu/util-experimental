package feh.util.sh

import feh.util._
import scala.util.matching.Regex

case class ProcessorCall(name: String, params: Seq[String] = Nil)

object UniversalProcessor extends ScopedState[UP](impl.UniversalProcessorImpl())

/** Combines various processors
 */
trait UniversalProcessor extends SourceProcessor{
  /** Processors available
   */
  protected def processorByName: Map[String, SourceProcessor]

  /** Applies the given processors to the source, supports processor params
   */
  def apply(processors: Seq[ProcessorCall]): StringBuilder => StringBuilder = _.$${
    sb =>
      processors.withFilter(processorByName contains _.name).foreach{
        c =>
          processorByName(c.name).process(sb, c.params: _*)
      }
  }
}

/** Reads processors names and params from source and applies them
 */
trait ProcessorConfigSourceParser extends SourceProcessorHelper{
  self: UniversalProcessor =>

  /** Processors configuration keyword
   */
  def configKey: String

  object ConfigKey{
    def r = configKey.r
    def len = configKey.length
  }

  def configSeparators: Array[Char]

  /**
   * 2 groups are expected: name, [args]
   */
  val configRegex: Regex
  def configArgsSeparators: Array[Char]

  def confReplacePolicy: SourceProcessorHelper#Replace

  assert(configSeparators.intersect(configArgsSeparators).isEmpty, "config and args separators sets intersect!")

  /**
   * @param args will be treated as processor calls using [[configRegex]]
   */
  def process(source: StringBuilder, args: String*) = {
    val confs = args ++ extractConfig(source, erase = true).flatMap(_.split(configSeparators))
    val calls = confs.map(_.trim).map{
      case `configRegex`(name, _args) =>
        val args = Option(_args).map(_.split(configArgsSeparators).toSeq).getOrElse(Nil)
        ProcessorCall(name, args)
      case x => sys.error("failed to parse config: " + x)
    }
    apply(calls)(source)
  }

  protected def extractConfig(source: StringBuilder, erase: Boolean) =
    extractAndReplace(source, allMatchesWithAggregatedLines(source.mkString, ConfigKey.r))(ConfigKey.len, 0) _ apply
      confReplacePolicy map uniteAggregated(configSeparators.head.toString)

}

/**
 * Defines `all` key
 */
trait ProcessorConfigSourceParserAllKey extends UniversalProcessor{
  self: ProcessorConfigSourceParser =>

  def allKey: String

  override def apply(processors: Seq[ProcessorCall]) = {
    val runAll = processors.exists(_.name == allKey)
    if(!runAll) super.apply(processors)
    else{ // leave existing calls intact to preserve parameters and add the missing ones
      val toRun =
        if(processors.size == 1) processorByName.keys.toSeq // 'all' is the only key defined
        else {
          val namesDefined = processors.map(_.name)
          processorByName.keys.filterNot(namesDefined.contains).toSeq
        }
      super.apply(toRun.map(ProcessorCall(_)))
    }
  }
}

trait UniversalProcessorLibs extends UniversalProcessor{

  /** should be called apart from other processors */
  def dependenciesProcessors: Seq[SourceDependencies]


}
