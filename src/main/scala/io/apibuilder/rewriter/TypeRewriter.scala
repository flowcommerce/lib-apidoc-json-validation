package io.apibuilder.rewriter

import apibuilder.{ApiBuilderHelper, ApiBuilderHelperImpl}
import io.apibuilder.spec.v0.models._
import io.apibuilder.validation.{AnyType, ApiBuilderService, ApiBuilderType, MultiService, ScalarType}

/**
 * Provides the ability to rename the types of unions / models type (eg. replacing a union with object)
 *
 * @param rewriteType A function that is called for each type we find. You return the new type if you want to change it
 */
case class TypeRewriter(rewriteType: AnyType => AnyType) extends DefaultRewriter {

  override def rewrite(multiService: MultiService): MultiService = {
    val helper = ApiBuilderHelperImpl(multiService)
    MultiService(multiService.services().map { s => rewrite(helper, s) })
  }

  private[this] def rewrite(helper: ApiBuilderHelper, service: ApiBuilderService): ApiBuilderService = {
    ApiBuilderService(
      service = service.service.copy(
        enums = service.enums.map { t => rewriteEnum(helper, t) }.map(_.`enum`),
        interfaces = service.interfaces.map { t => rewriteInterface(helper, t) }.map(_.interface),
        models = service.models.map { t => rewriteModel(helper, t) }.map(_.model),
        unions = service.unions.map { t => rewriteUnion(helper, t) }.map(_.union),
        resources = service.service.resources.map { r => rewrite(helper, service, r) }
      )
    )
  }

  private[this] def rewrite(helper: ApiBuilderHelper, service: ApiBuilderService, resource: Resource): Resource = {
    resource.copy(
      `type` = doRewriteType(helper, service, resource.`type`),
      operations = resource.operations.map { op =>
        rewrite(helper, service, op)
      }
    )
  }

  private[this] def rewrite(helper: ApiBuilderHelper, service: ApiBuilderService, operation: Operation): Operation = {
    operation.copy(
      body = operation.body.map { b => rewrite(helper, service, b) },
      parameters = operation.parameters.map { p => rewrite(helper, service, p) },
      responses = operation.responses.map { r => rewrite(helper, service, r) },
    )
  }

  private[this] def rewrite(helper: ApiBuilderHelper, service: ApiBuilderService, body: Body): Body = {
    body.copy(
      `type` = doRewriteType(helper, service, body.`type`),
    )
  }

  private[this] def rewrite(helper: ApiBuilderHelper, service: ApiBuilderService, parameter: Parameter): Parameter = {
    parameter.copy(
      `type` = doRewriteType(helper, service, parameter.`type`),
    )
  }

  private[this] def rewrite(helper: ApiBuilderHelper, service: ApiBuilderService, response: Response): Response = {
    response.copy(
      `type` = doRewriteType(helper, service, response.`type`),
    )
  }

  private[this] def rewriteEnum(helper: ApiBuilderHelper, typ: ApiBuilderType.Enum): ApiBuilderType.Enum = {
    ApiBuilderType.Enum(
      typ.service,
      typ.`enum`.copy(
        name = doRewriteType(helper, typ.service, typ.`enum`.name),
      )
    )
  }

  private[this] def rewriteUnion(helper: ApiBuilderHelper, typ: ApiBuilderType.Union): ApiBuilderType.Union = {
    ApiBuilderType.Union(
      typ.service,
      typ.union.copy(
        name = doRewriteType(helper, typ.service, typ.union.name),
        types = typ.union.types.map { t =>
          t.copy(
            `type` = doRewriteType(helper, typ.service, t.`type`),
          )
        }
      )
    )
  }


  private[this] def rewriteInterface(helper: ApiBuilderHelper, typ: ApiBuilderType.Interface): ApiBuilderType.Interface = {
    ApiBuilderType.Interface(
      typ.service,
      typ.interface.copy(
        name = doRewriteType(helper, typ.service, typ.interface.name),
        fields = typ.fields.map { f =>
          f.field.copy(
            `type` = doRewriteType(helper, f.parent.service, f.field.`type`)
          )
        }
      )
    )
  }
  private[this] def rewriteModel(helper: ApiBuilderHelper, typ: ApiBuilderType.Model): ApiBuilderType.Model = {
    ApiBuilderType.Model(
      typ.service,
      typ.model.copy(
        name = doRewriteType(helper, typ.service, typ.model.name),
        fields = typ.fields.map { f =>
          f.field.copy(
            `type` = doRewriteType(helper, f.parent.service, f.field.`type`)
          )
        }
      )
    )
  }

  private[this] def doRewriteType(helper: ApiBuilderHelper, service: ApiBuilderService, typeName: String): String = {
    val baseType = helper.resolveType(service, typeName).getOrElse {
      sys.error(s"Unknown type: ${typeName}")
    }

    addCollections(
      typeName,
      rewriteType(baseType) match {
        case t: ScalarType => t.name
        case t: ApiBuilderType if t.namespace == service.namespace => t.name
        case t: ApiBuilderType => t.qualified
      }
    )
  }

  private[this] def addCollections(originalType: String, newType: String): String = {
    originalType match {
      case ApiBuilderHelper.Array(inner) => "[" + addCollections(inner, newType) + "]"
      case ApiBuilderHelper.Map(inner) => "map[" + addCollections(inner, newType) + "]"
      case _ => newType
    }
  }
}
