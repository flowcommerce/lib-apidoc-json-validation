package io.apibuilder.rewriter

import apibuilder.{ApiBuilderHelper, ApiBuilderHelperImpl}
import io.apibuilder.validation.{ApiBuilderType, MultiService}

import scala.annotation.tailrec

/**
 * Rewrites the multi service such that the specified types resolve correctly. eg.
 * recurses through the provides types to collect ALL referenced types (through models, unions, etc)
 * and return a multi service containing only the types necessary such that the provided
 * types fully resolve.
 */
case class MinimalTypesRewriter(types: Iterable[ApiBuilderType]) extends DefaultRewriter {

  override def rewrite(multiService: MultiService): MultiService = {
    val helper = ApiBuilderHelperImpl(multiService)
    val all = expand(helper, types.toSeq, Set.empty).groupBy(_.namespace)

    MultiService(
      multiService.services().map { s =>
        val svcTypes = all.getOrElse(s.namespace, Nil).toSeq
        s.copy(
          service = s.service.copy(
            enums = svcTypes.collect { case t: ApiBuilderType.Enum => t }.map(_.`enum`),
            interfaces = svcTypes.collect { case t: ApiBuilderType.Interface => t }.map(_.interface),
            models = svcTypes.collect { case t: ApiBuilderType.Model => t }.map(_.model),
            unions = svcTypes.collect { case t: ApiBuilderType.Union => t }.map(_.union),
          )
        )
      }
    )
  }

  /**
   * Expand the types to include any types defined on the fields of models
   * or the types of a union
   */
  @tailrec
  private[this] def expand(helper: ApiBuilderHelper, incoming: Seq[ApiBuilderType], resolved: Set[ApiBuilderType]): Set[ApiBuilderType] = {
    incoming.toList match {
      case Nil => resolved
      case one :: rest => {
        val newTypes = one match {
          case _: ApiBuilderType.Enum => Nil
          case t: ApiBuilderType.Interface => t.fields.flatMap(helper.resolveType)
          case t: ApiBuilderType.Model => t.fields.flatMap(helper.resolveType)
          case t: ApiBuilderType.Union => t.types.flatMap(helper.resolveType)
        }
        val newResolved = resolved ++ Set(one)
        expand(
          helper,
          rest ++ newTypes
          .collect { case t: ApiBuilderType => t }
          .filterNot(newResolved.contains),
          newResolved
        )
      }
    }
  }

}
