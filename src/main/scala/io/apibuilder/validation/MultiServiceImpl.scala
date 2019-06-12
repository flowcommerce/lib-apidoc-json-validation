package io.apibuilder.validation

import io.apibuilder.spec.v0.models._
import play.api.libs.json._

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiServiceImpl(
  services: Seq[ApiBuilderService]
) extends MultiService {

  private[this] val validator = JsonValidator(services.map(_.service))

  def findType(name: String): Seq[ApibuilderType] = validator.findType(name, defaultNamespace = None)

  def findType(namespace: String, name: String): Seq[ApibuilderType] = validator.findType(namespace, name)

  /**
    * Validates the js value across all services, upcasting types to
    * match the request method/path as needed.
    */
  def upcast(method: String, path: String, js: JsValue): Either[Seq[String], JsValue] = {
    resolveService(method, path) match {
      case Left(errors) => {
        Left(errors)
      }
      case Right(service) => {
        service.validate(method = method, path = path) match {
          case Left(errors) => {
            Left(errors)
          }
          case Right(op) => {
            op.body.map(_.`type`) match {
              case None => {
                Right(js)
              }
              case Some(typeName) => {
                service.findType(typeName) match {
                  case None => upcast(typeName, js)
                  case Some(typ) => upcast(typ, js)
                }
              }
            }
          }
        }
      }
    }
  }

  /**
    * Upcast the json value based on the specified type name
    *
    * @param typeName e.g. 'user' - looks up the apibuilder type with this name
    *                 and if found, uses that type to validate and upcase the
    *                 JSON. Note if the type is not found, the JSON returned
    *                 is unchanged.
    */
  def upcast(typeName: String, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validate(typeName, js, defaultNamespace = None)
  }

  def upcast(typ: ApibuilderType, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js)
  }

  /**
   * Validates that the path is known and the method is supported for the path.
   * If known, returns the corresponding operation. Otherwise returns a
   * list of errors.
   */
  def validate(method: String, path: String): Either[Seq[String], Operation] = {
    resolveService(method, path) match {
      case Left(errors) => Left(errors)
      case Right(service) => service.validate(method, path)
    }
  }

  def validate(
    typ: ApibuilderType,
    js: JsValue,
    prefix: Option[String] = None
  ): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js, prefix)
  }

  /**
    * resolve the API Builder service defined at the provided method, path.
    * if no service, return a nice error message. Otherwise invoke
    * the provided function on the API Builder service.
    */
  private[validation] def resolveService(method: String, path: String): Either[Seq[String], ApiBuilderService] = {
    resolveService(Method(method), path)
  }

  private[this] def resolveService(method: Method, path: String): Either[Seq[String], ApiBuilderService] = {
    services.filter { s =>
      s.isDefinedAt(method = method, path = path)
    } match {
      case Nil => {
        services.find(_.isPathDefinedAt(path)) match {
          case None => {
            Left(Seq(s"HTTP path '$path' is not defined"))
          }

          case Some(s) => s.validate(method, path) match {
            case Left(errors) => Left(errors)
            case Right(_) => Right(s)
          }
        }
      }
      case one :: Nil => Right(one)

      case multiple => {
        // If we find a non dynamic path in any service, return that one.
        // Otherwise return the first matching service. This handles ambiguity:
        //   - service 1 defines POST /:organization/tokens
        //   - service 2 defines POST /users/tokens
        // We want to return service 2 when the path is /users/tokens
        Right(
          multiple.find { s =>
            s.validate(method, path) match {
              case Right(op) if Route.isStatic(op.path) => true
              case _ => false
            }
          }.getOrElse {
            multiple.head
          }
        )
      }
    }
  }

}

