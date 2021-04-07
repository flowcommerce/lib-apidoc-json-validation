package io.apibuilder.validation

import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.util.{FileOrder, StandardErrors}
import io.apibuilder.validation.zip.ZipFileReader
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
trait MultiService extends ResponseHelpers {

  def services(): List[ApiBuilderService]

  def findOperation(method: Method, path: String): Option[ApiBuilderOperation]

  /**
    * Resolves the type specified
    */
  def findType(typeName: TypeName): Option[AnyType]

  /**
   * Resolves the types specified
   */
  def findTypes(typeName: TypeName): Seq[AnyType]

  /**
    * Upcast the json value based on the specified type name, if it is defined. a No-op if not
    */
  def upcast(typ: AnyType, js: JsValue): Either[Seq[String], JsValue]

  final def findOperation(method: String, path: String): Option[ApiBuilderOperation] = {
    findOperation(Method(method), path)
  }

  /**
    * @param defaultNamespace e.g. io.flow.user.v0 - used unless the type name
    *                         is fully qualified already
    * @param typeName e.g. 'user' - looks up the API Builder type with this name
    *                 and if found
    */
  final def findType(defaultNamespace: String, typeName: String): Option[AnyType] = {
    findType(TypeName.parse(name = typeName, defaultNamespace = defaultNamespace))
  }

  final def findType(fullyQualifiedName: String): Option[AnyType] = {
    TypeName.parse(fullyQualifiedName).flatMap(findType) orElse {
      ScalarType.fromName(fullyQualifiedName)
    }
  }

  final def findTypes(defaultNamespace: String, typeName: String): Seq[AnyType] = {
    findTypes(TypeName.parse(name = typeName, defaultNamespace = defaultNamespace))
  }

  final def findTypes(fullyQualifiedName: String): Seq[AnyType] = {
    TypeName.parse(fullyQualifiedName).toSeq.flatMap(findTypes) match {
      case Nil => ScalarType.fromName(fullyQualifiedName).toSeq
      case some => some
    }
  }

  /**
    * If the specified method & path requires a body, returns the type of the body
    */
  final def bodyTypeFromPath(method: String, path: String): Option[AnyType] = {
    bodyTypeFromPath(Method(method), path)
  }

  final def bodyTypeFromPath(method: Method, path: String): Option[AnyType] = {
    findOperation(method, path).flatMap(findBodyType)
  }

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  final def upcast(apiBuilderOperation: ApiBuilderOperation, js: JsValue): Either[Seq[String], JsValue] = {
    findBodyType(apiBuilderOperation) match {
      case None => Right(js)
      case Some(bodyType) => upcast(bodyType, js)
    }
  }

  final def findBodyType(apiBuilderOperation: ApiBuilderOperation): Option[AnyType] = {
    apiBuilderOperation.operation.body.flatMap { body =>
      findType(
        defaultNamespace = apiBuilderOperation.service.service.namespace,
        typeName = body.`type`
      )
    }
  }

  final def upcastOperationBody(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    upcastOperationBody(Method(method), path, js)
  }

  final def upcastOperationBody(method: Method, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    validateOperation(method, path) match {
      case Left(errors) => Left(errors)
      case Right(op) => upcast(op, js)
    }
  }

  final def upcastType(defaultNamespace: String, typeName: String, js: JsValue): Either[Seq[String], JsValue] = {
    findType(defaultNamespace = defaultNamespace, typeName = typeName) match {
      case None => Right(js)
      case Some(t) => upcast(t, js)
    }
  }

  final def validateOperation(method: Method, path: String): Either[Seq[String], ApiBuilderOperation] = {
    findOperation(method, path) match {
      case Some(op) => Right(op)
      case None => Left(operationErrorMessage(method, path))
    }
  }


  /**
    * Returns a nice error message explaining that this method is unavailable
    * with hints as to what may be (e.g. alternate methods)
    */
  private[this] def operationErrorMessage(method: Method, path: String): Seq[String] = {
    method match {
      case Method.UNDEFINED(name) => {
        Seq(StandardErrors.invalidMethodError(name))
      }
      case _ => {
        val availableMethods = Method.all.filterNot(_ == method).filter { m =>
          findOperation(m, path).nonEmpty
        }
        if (availableMethods.isEmpty) {
          Seq(s"HTTP path '$path' is not defined")
        } else {
          Seq(s"HTTP method '$method' not defined for path '$path' - Available methods: ${availableMethods.map(_.toString).mkString(", ")}")
        }
      }
    }
  }

  final val allEnums: Seq[ApiBuilderType.Enum] = services().flatMap(_.enums)

  final val allInterfaces: Seq[ApiBuilderType.Interface] = services().flatMap(_.interfaces)

  final val allModels: Seq[ApiBuilderType.Model] = services().flatMap(_.models)

  final val allUnions: Seq[ApiBuilderType.Union] = services().flatMap(_.unions)

  final val allTypes: Seq[ApiBuilderType] = allEnums ++ allModels ++ allUnions
}

object MultiService {

  // If this file is found in the zip file, we read files in
  // the order in which they are listed.
  private[this] val OrderByFileName: String = "order.txt"

  def apply(services: List[ApiBuilderService]): MultiService = {
    MultiServiceImpl(services)
  }

  def fromUrl(url: String): Either[Seq[String], MultiService] = {
    fromUrls(urls = Seq(url))
  }

  /**
  * Loads the list of API Builder service specification from the specified URIs,
  * returning either a list of errors or an instance of MultiService
  */
  def fromUrls(urls: Seq[String]): Either[Seq[String], MultiService] = {
    val eithers = urls.flatMap { url =>
      if (ZipFileReader.isZipFile(url)) {
        ZipFileReader.fromUrl(url) match {
          case Left(errors) => Seq(Left(errors))
          case Right(reader) => {
            val fileSorter = FileOrder(reader.entries.find(_.name.toLowerCase() == OrderByFileName).map(_.file))
            reader.entries
              .filter { e => ZipFileReader.isJsonFile(e.name) }
              .sortBy { e => fileSorter.sortOrder(e.name) }
              .map { e => ApiBuilderService.fromFile(e.file) }
          }
        }
      } else {
        Seq(ApiBuilderService.fromUrl(url))
      }
    }

    eithers.flatMap(_.left.getOrElse(Nil)).toList match {
      case Nil => {
        val services = eithers.collect { case Right(r) => r }
        Right(MultiServiceImpl(services.toList))
      }
      case errors => Left(errors)
    }
  }

}
