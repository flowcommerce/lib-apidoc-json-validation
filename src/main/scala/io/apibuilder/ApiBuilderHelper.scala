package apibuilder

import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.{AnyType, ApiBuilderField, ApiBuilderService, ApiBuilderType, ApiBuilderUnionType, MultiService, ScalarType, TypeName}

import scala.annotation.tailrec
import scala.util.matching.Regex

case class ApiBuilderHelperImpl(override val multiService: MultiService) extends ApiBuilderHelper

object ApiBuilderHelper {
  val Array: Regex = """^\[(.+)\]$""".r
  val Map: Regex = """^map\[(.+)\]$""".r

  def is2xx(r: Response): Boolean = {
    r.code match {
      case ResponseCodeInt(v) => v >= 200 && v < 300
      case ResponseCodeOption.Default => true
      case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
    }
  }

  def changeName(apiBuilderType: ApiBuilderType, name: String): ApiBuilderType = {
    apiBuilderType match {
      case t: ApiBuilderType.Enum => ApiBuilderType.Enum(t.service, t.`enum`.copy(name = name))
      case t: ApiBuilderType.Interface => ApiBuilderType.Interface(t.service, t.interface.copy(name = name))
      case t: ApiBuilderType.Model => ApiBuilderType.Model(t.service, t.model.copy(name = name))
      case t: ApiBuilderType.Union => ApiBuilderType.Union(t.service, t.union.copy(name = name))
    }
  }
}

/**
 * Collection of utilities to simplify working with ApiBuilder services including lots
 * of helpers to resolve types from strings to Any Time.
 */
trait ApiBuilderHelper {

  def multiService: MultiService

  def resolveType(service: ApiBuilderService, typeName: String): Option[AnyType] = {
    resolveType(service.service, typeName)
  }

  def resolveType(field: ApiBuilderField): Option[AnyType] = resolveType(field.parent.service, field.field.`type`)
  def resolveType(unionType: ApiBuilderUnionType): Option[AnyType] = resolveType(unionType.union.service, unionType.`type`.`type`)
  def resolveType(service: Service, resource: Resource): Option[AnyType] = resolveType(service, resource.`type`)
  def resolveType(service: Service, body: Body): Option[AnyType] = resolveType(service, body.`type`)
  def resolveType(service: Service, parameter: Parameter): Option[AnyType] = resolveType(service, parameter.`type`)

  def resolveType(typeName: TypeName): Option[AnyType] = {
    multiService.services().find(_.namespace == typeName.namespace).flatMap { s =>
      resolveType(s, typeName.name)
    }
  }

  def resolveType(service: Service, typeName: String): Option[AnyType] = {
    multiService.findType(
      defaultNamespace = service.namespace,
      typeName = baseType(service, typeName),
    )
  }

  def isDeprecated(resource: Resource): Boolean = resource.deprecation.isDefined

  def isDeprecated(operation: Operation): Boolean = operation.deprecation.isDefined

  def isDeprecated(response: Response): Boolean = response.deprecation.isDefined

  def isMap(typeName: String): Boolean = {
    typeName match {
      case ApiBuilderHelper.Map(_) => true
      case _ => false
    }
  }

  def isArray(typeName: String): Boolean = {
    typeName match {
      case ApiBuilderHelper.Array(_) => true
      case _ => false
    }
  }

  def responseIsMulti(operation: Operation): Boolean = {
    operation.responses.filter(ApiBuilderHelper.is2xx).exists(responseIsMulti)
  }

  def responseIsMulti(response: Response): Boolean = {
    isMap(response.`type`) || isArray(response.`type`)
  }

  def responseIsMulti(responseType: String): Boolean = {
    isMap(responseType) || isArray(responseType)
  }

  final def isScalar(service: ApiBuilderService, typeName: String): Boolean = {
    ScalarType.fromName(baseType(service.service, typeName)).isDefined
  }

  def isModel(apiBuilderService: ApiBuilderService, typeName: String): Boolean = {
    isModel(apiBuilderService.service, typeName)
  }

  def isModel(service: Service, typeName: String): Boolean = {
    resolveType(service, typeName) match {
      case Some(_: ApiBuilderType.Model) => true
      case _ => false
    }
  }

  /**
   * Resolves the base type of the typeName. For example, all of the
   * following would resolve to the ApiBuilderType.Model of "user"
   * (assuming the model is defined):
   *   - "user"
   *   - "[user]"
   *   - "map[user]"
   */
  @tailrec
  final def baseType(service: Service, typeName: String): String = {
    typeName match {
      case ApiBuilderHelper.Array(inner) => baseType(service, inner)
      case ApiBuilderHelper.Map(inner) => baseType(service, inner)
      case _ => typeName
    }
  }

}
