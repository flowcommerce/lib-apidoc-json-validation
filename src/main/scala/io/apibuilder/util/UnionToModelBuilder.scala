package io.apibuilder.util

import apibuilder.{ApiBuilderHelper, ApiBuilderHelperImpl}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.{Enum, EnumValue, Field, Model, UnionType}
import io.apibuilder.validation.{ApiBuilderService, ApiBuilderType, ApiBuilderUnionType, MultiService, ScalarType}

case class UnionModel(
  service: ApiBuilderService,
  discriminatorEnum: Enum,
  model: Model,
) {
  private[this] val discriminatorEnumApiBuilderType = ApiBuilderType.Enum(service, discriminatorEnum)
  private[this] val modelApiBuilderType = ApiBuilderType.Model(service, model)
  val apiBuilderTypes = Seq(discriminatorEnumApiBuilderType, modelApiBuilderType)
}

/**
 * Given a union class, returns a single model type
 * to represent ALL of the field of ALL of the types.
 *
 * If the union type models contain fields of the same name
 * but differing types:
 *   - if they are all scalars, we use string
 *   - otherwise we return a validation error
 *
 * This class requires the union to have a discriminator
 * and generates an enum to represent the values
 * of that discriminator.
 **/
case class UnionToModelBuilder(multiService: MultiService) {

  private[this] val helper: ApiBuilderHelper = ApiBuilderHelperImpl(multiService)

  def toModel(union: ApiBuilderType.Union): ValidatedNec[String, UnionModel] = {
    (
      validateDiscriminator(union),
      validateTypes(union.types).andThen { types =>
        validateFields(union, types)
      }
    ).mapN { case (discriminator, fields) =>
      buildModel(union, discriminator, fields)
    }
  }

  private[this] def buildModel(union: ApiBuilderType.Union, discriminator: String, fields: Seq[Field]): UnionModel = {
    val enum = buildDiscriminatorEnum(union, discriminator)
    val allFields = Seq(buildDiscriminatorField(union, enum, discriminator)) ++ fields
    UnionModel(
      service = union.service,
      discriminatorEnum = enum,
      model = Model(
        name = union.union.name,
        plural = union.union.plural,
        fields = allFields,
      )
    )
  }

  private[this] def buildDiscriminatorEnum(union: ApiBuilderType.Union, discriminator: String): Enum = {
    Enum(
      name = union.name + "_" + discriminator,
      plural = union.name + "_" + pluralize(discriminator),
      values = union.union.types.map { t => buildEnumValue(t) },
    )
  }

  private[this] def pluralize(value: String): String = value + "s"

  private[this] def buildEnumValue(typ: UnionType): EnumValue = {
    EnumValue(name = typ.`type`, value = typ.discriminatorValue)
  }

  private[this] def validateFields(
    union: ApiBuilderType.Union,
    models: Seq[ApiBuilderType.Model],
  ): ValidatedNec[String, Seq[Field]] = {
    // sort field names by the first occurrence across all models
    val sortOrder = models.flatMap(_.model.fields).map(_.name).zipWithIndex.groupBy(_._1).map { case (n, data) =>
      n -> data.head._2
    }

    val byName = models.flatMap(_.model.fields).groupBy(_.name)
    byName.keys.toList.sortBy(sortOrder).map { fieldName =>
      val fields = byName(fieldName)

      toCommonTypes(fields.map(_.`type`)).toList match {
        case fieldType :: Nil => {
          val default = fields.map(_.default).toList.distinct match {
            case one :: Nil if fields.size == models.size => one
            case _ => None
          }
          val required = fields.size == models.size && fields.forall(_.required)
          Seq(buildField(fieldName, fieldType, required, default)).validNec
        }
        case multiple => {
          s"Union type '${union.qualified}' Field '${fieldName}' has incompatible types: ${multiple.mkString(", ")} - all union types must have a common type".invalidNec
        }
      }
    }.traverse(identity).map(_.flatten)
  }

  private[this] def buildField(name: String, typ: String, required: Boolean, default: Option[String]): Field = {
    Field(
      name = name,
      `type` = typ,
      default = default,
      required = required,
    )
  }

  private[this] def hasDefaultType(union: ApiBuilderType.Union): Boolean = {
    union.union.types.exists(_.default.getOrElse(false))
  }

  private[this] def buildDiscriminatorField(union: ApiBuilderType.Union, discriminatorEnum: Enum, name: String): Field = {
    Field(
      name = name,
      `type` = discriminatorEnum.name,
       required = !hasDefaultType(union),
    )
  }

  /**
   * sometimes we see fields where the type is 'string' and 'long' => in these cases convert to 'string'
   * as a universal type
   */
  private[this] def toCommonTypes(types: Seq[String]): Seq[String] = {
    types.distinct.toList match {
      case one :: Nil => Seq(one)
      case _ if types.forall { t => ScalarType.fromName(t).isDefined } => Seq(ScalarType.StringType.name)
      case other => other.sorted
    }
  }

  private[this] def validateTypes(types: Seq[ApiBuilderUnionType]): ValidatedNec[String, Seq[ApiBuilderType.Model]] = {
    types.map(validate).toList.traverse(identity)
  }

  private[this] def validate(unionType: ApiBuilderUnionType): ValidatedNec[String, ApiBuilderType.Model] = {
    helper.resolveType(unionType) match {
      case None => s"Could not resolve type '${unionType.`type`}'".invalidNec
      case Some(t) => {
        t match {
          case _: ScalarType => s"Type '${t.name}' is a scalar. A model is required".invalidNec
          case _: ApiBuilderType.Interface  => s"Type '${t.name}' is an interface. A model is required".invalidNec
          case m: ApiBuilderType.Model => m.validNec
          case _: ApiBuilderType.Enum  => s"Type '${t.name}' is an enum. A model is required".invalidNec
          case _: ApiBuilderType.Union => s"Type '${t.name}' is a union. A model is required".invalidNec
        }
      }
    }
  }

  private[this] def validateDiscriminator(union: ApiBuilderType.Union): ValidatedNec[String, String] = {
    union.union.discriminator match {
      case None => s"Union '${union.qualified}' must have a 'discriminator' defined'".invalidNec
      case Some(d) => d.validNec
    }
  }

}
