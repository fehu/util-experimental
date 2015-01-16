package feh.util.sh

import feh.util._
import feh.util.sh.exec.{DependencyInfo, Import, LibInfo}

/** Provides a mechanism for a [[feh.util.sh.exec.ScriptExecutor]] to access the library dependencies of source code
  */
trait SourceDependencies extends SourceProcessor{

  /** processes the source and returns it with dependencies; the source may be modified
   */
  def withDependencies(source: StringBuilder, params: String*): (StringBuilder, Seq[DependencyInfo])
}

/** Writes import statements into `source` StringBuilder.
  *  The imports to write are defined by <b>keys</b>, passed to `params` argument of `process` method.
  *  The keys are resolved by `libByKey` method
  */
trait SourceImports extends SourceDependencies{
  type ImportKey = String

  def importByKey: PartialFunction[ImportKey, Seq[Import]]
  def strings(imports: Seq[Import]): Seq[String]
  protected def insert(imports: String, in: StringBuilder): StringBuilder

  def predefinedImports: Seq[Import]

  /**
   * @param params keys to include in imports and classpath
   */
  def process(source: StringBuilder, params: String*) = withDependencies(source, params: _*)._1

  def withDependencies(source: StringBuilder, params: String*): (StringBuilder, Seq[Import]) = {
    val imp = imports(source, params).distinct
    (imp |> insertImports(source)) -> imp
  }


  def importsIndent = 0

  def toString(imports: Seq[Import], indent: Int = 0) = strings(imports).map(" "*indent +).mkString("\n")

  protected def imports(source: StringBuilder, params: Seq[String]) = predefinedImports ++ importsByKeys(params)

  protected def importsByKeys(keys: Seq[String]) =
    keys.flatMap(importByKey.applyOrElse(_, (key: String) => sys.error(s"no key $key is defined")))

  protected def insertImports(source: StringBuilder)(libs: Seq[Import]) =
    toString(libs, importsIndent) |> (insert(_, source))
}

trait SourceImportsExtractor extends SourceImports{
  /** caution, this method removes dependencies definitions from source */
  override protected def imports(source: StringBuilder, params: Seq[String]) =
    super.imports(source, params) ++ extractImports(source, params).pipe(importsByKeys)

  /** import keyword */
  def importKey: String

  /** reads and removes import definitions from source */
  protected def extractImports(source: StringBuilder, params: Seq[String]): Seq[ImportKey]
}

/** Extracts imports and dependencies definition from the source
  */
trait SourceDependenciesExtractor extends SourceDependencies {

  def predefinedDependencies: Seq[LibInfo]

  /** dependency keyword */
  def dependencyKey: String

  /**
   * @param params are ignored for now, TODO
   */
  def process(source: StringBuilder, params: String*) = withDependencies(source, params: _*)._1

  def withDependencies(source: StringBuilder, params: String*): (StringBuilder, Seq[LibInfo]) =
    source -> dependencies(source, params)

  /** caution, this method removes dependencies definitions from source */
  protected def dependencies(source: StringBuilder, params: Seq[String]) =
    predefinedDependencies ++ extractDependencies(source, params)

  /** reads and removes import definitions from source */
  protected def extractDependencies(source: StringBuilder, params: Seq[String]): Seq[LibInfo]
}
