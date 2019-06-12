package io.apibuilder.validation

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, FileInputStream, InputStream}

import io.apibuilder.spec.v0.models.{Method, Operation, Service}
import io.apibuilder.spec.v0.models.json._
import java.nio.charset.StandardCharsets

import io.apibuilder.validation.util.{StandardErrors, UrlDownloader}
import play.api.libs.json._

/**
  * Wraps a single API Builder service, providing helpers to validate
  * objects based on the incoming http method and path
  */
case class ApiBuilderService(
  service: Service
) {
  private[this] val validator = JsonValidator(service)
  private[this] val normalizer = PathNormalizer(service)

  /**
    * If the specified method, path require a body, returns the type of the body
    */
  def bodyTypeFromPath(method: String, path: String): Option[String] = {
    operation(method, path).flatMap(_.body.map(_.`type`))
  }

  def findType(name: String): Option[ApibuilderType] = {
    validator.findType(name, defaultNamespace = Some(service.namespace)).headOption
  }

  /**
    * Returns a map of the HTTP Methods available for the given path
    */
  def methodsFromPath(path: String): Seq[Method] = {
    Method.all.filter { m =>
      normalizer.resolve(m, path).isRight
    }
  }

  def isDefinedAt(method: String, path: String): Boolean = {
    isDefinedAt(Method(method), path)
  }

  def isDefinedAt(method: Method, path: String): Boolean = {
    validate(method, path).isRight
  }

  def isPathDefinedAt(path: String): Boolean = {
    Method.all.exists { m =>
      normalizer.resolve(m, path).isRight
    }
  }

  /**
    * If the provided method and path are known, returns the associated
    * operation. Otherwise returns an appropriate error message.
    */
  def validate(method: String, path: String): Either[Seq[String], Operation] = {
    Method.fromString(method) match {
      case None => Left(Seq(StandardErrors.invalidMethodError(method)))
      case Some(m) => validate(m, path)
    }
  }

  def validate(method: Method, path: String): Either[Seq[String], Operation] = {
    normalizer.resolve(method, path) match {
      case Right(op) => Right(op)
      case Left(_) => {
        // Provide a friendly error here
        methodsFromPath(path).toList match {
          case Nil => {
            Left(Seq(s"HTTP path $path is not defined"))
          }

          case available => {
            Left(Seq(
              s"HTTP method '$method' not supported for path $path - Available methods: " +
                available.map(_.toString).mkString(", ")
            ))
          }
        }
      }
    }
  }

  /**
    * Returns the operation associated with the specified method
    * and path, if any
    */
  def operation(method: String, path: String): Option[Operation] = {
    normalizer.resolve(method, path) match {
      case Left(_) => None
      case Right(op) => Some(op)
    }
  }

}

object ApiBuilderService {

  /**
    * Loads the API Builder service specification from the specified URI,
    * returning either a list of errors or the service itself.
    */
  def fromUrl(url: String): Either[Seq[String], ApiBuilderService] = {
    UrlDownloader.withInputStream(url) { is =>
      fromInputStream(is)
    }
  }

  def fromFile(file: File): Either[Seq[String], ApiBuilderService] = {
    val is = new BufferedInputStream(new FileInputStream(file))
    try {
      fromInputStream(is)
    } finally {
      is.close()
    }
  }

  def fromInputStream(inputStream: InputStream): Either[Seq[String], ApiBuilderService] = {
    toService(copyToString(inputStream))
  }

  private[this] def copyToString(inputStream: InputStream): String = {
    val result = new ByteArrayOutputStream()
    val buffer = new Array[Byte](1024)
    var length = inputStream.read(buffer)
    while (length != -1) {
      result.write(buffer, 0, length)
      length = inputStream.read(buffer)
    }
    result.toString(StandardCharsets.UTF_8.name())
  }

  def toService(contents: String): Either[Seq[String], ApiBuilderService] = {
    Json.parse(contents).validate[Service] match {
      case s: JsSuccess[Service] => Right(ApiBuilderService(s.get))
      case e: JsError => Left(Seq(s"Error parsing service: $e"))
    }
  }
  
}
