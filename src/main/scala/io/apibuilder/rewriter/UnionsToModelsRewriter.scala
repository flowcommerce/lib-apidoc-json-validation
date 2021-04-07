package io.apibuilder.rewriter

import cats.data.Validated.{Invalid, Valid}
import io.apibuilder.util.UnionToModelBuilder
import io.apibuilder.validation.{ApiBuilderService, ApiBuilderType, MultiService}

/**
 * Replaces all union types with models (and their associated discriminator enums).
 * If the union type cannot be converted to a model - usually because we find types
 * with fields that themselves have types that are different - we leave that union
 * type untouched.
 */
object UnionsToModelsRewriter extends DefaultRewriter {

  override def rewrite(multiService: MultiService): MultiService = {
    val unionToModelBuilder = UnionToModelBuilder(multiService)
    MultiService(
      multiService.services().map { s => rewrite(unionToModelBuilder, s) }
    )
  }

  private[this] def rewrite(unionToModelBuilder: UnionToModelBuilder, service: ApiBuilderService): ApiBuilderService = {
    val newTypes = service.unions.flatMap { u => rewrite(unionToModelBuilder, u) }
    service.copy(
      service = service.service.copy(
        enums = service.service.enums ++ newTypes.collect { case e: ApiBuilderType.Enum => e.enum },
        interfaces = service.service.interfaces ++ newTypes.collect { case m: ApiBuilderType.Interface => m.interface },
        models = service.service.models ++ newTypes.collect { case m: ApiBuilderType.Model => m.model },
        unions = newTypes.collect { case e: ApiBuilderType.Union => e.union },
      )
    )
  }

  private[this] def rewrite(unionToModelBuilder: UnionToModelBuilder, union: ApiBuilderType.Union): Seq[ApiBuilderType] = {
    unionToModelBuilder.toModel(union) match {
      case Valid(r) => r.apiBuilderTypes
      case Invalid(errors) => {
        // TODO: Maybe thread through validation end to end
        sys.error(s"rewrite errors: " + errors.toNonEmptyList.toList.mkString(", "))
      }
    }
  }

}
