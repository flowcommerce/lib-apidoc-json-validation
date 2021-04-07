package io.apibuilder.validation

import io.apibuilder.spec.v0.models
import io.apibuilder.spec.v0.models.{Field, UnionType}

sealed trait AnyType {
  def name: String
}

sealed trait ScalarType extends AnyType

/**
 * Represents a scalar in api builder
 */
object ScalarType {
  case object BooleanType extends ScalarType { override val name = "boolean" }
  case object DoubleType extends ScalarType { override val name = "double" }
  case object IntegerType extends ScalarType { override val name = "integer" }
  case object StringType extends ScalarType { override val name = "string" }
  case object DecimalType extends ScalarType { override val name = "decimal" }
  case object FloatType extends ScalarType { override val name = "float" }
  case object LongType extends ScalarType { override val name = "long" }
  case object JsonType extends ScalarType { override val name = "json" }
  case object ObjectType extends ScalarType { override val name = "object" }
  case object DateIso8601Type extends ScalarType { override val name = "date-iso8601" }
  case object DateTimeIso8601Type extends ScalarType { override val name = "date-time-iso8601" }
  case object UnitType extends ScalarType { override val name = "unit" }
  case object UuidType extends ScalarType { override val name = "uuid" }

  val all: scala.List[ScalarType] = scala.List(BooleanType, DoubleType, IntegerType, StringType, DecimalType, FloatType, LongType, JsonType, ObjectType, DateIso8601Type, DateTimeIso8601Type, UnitType, UuidType)

  private[this] val byName: Map[String, ScalarType] = all.map(x => x.name.toLowerCase -> x).toMap

  def fromName(typeName: String): Option[ScalarType] = byName.get(typeName.toLowerCase)
}

sealed trait TypeDiscriminator
object TypeDiscriminator {
  case object Enums extends TypeDiscriminator { override def toString = "enums" }
  case object Interfaces extends TypeDiscriminator { override def toString = "interfaces" }
  case object Models extends TypeDiscriminator { override def toString = "models" }
  case object Unions extends TypeDiscriminator { override def toString = "unions" }
}

/**
 * Represents an interface, union, model or enum
 */
sealed trait ApiBuilderType extends AnyType {

  def service: ApiBuilderService
  final def namespace: String = service.namespace
  def typeDiscriminator: TypeDiscriminator

  /**
   * returns the fully qualified name of this type (ie including the namespace)
   */
  def qualified: String = s"$namespace.$typeDiscriminator.$name"
}

case class ApiBuilderField(parent: ApiBuilderType, field: Field)
case class ApiBuilderUnionType(union: ApiBuilderType.Union, `type`: UnionType)

object ApiBuilderType {
  case class Enum(override val service: ApiBuilderService, enum: models.Enum) extends ApiBuilderType {
    override val name: String = enum.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Enums
  }
  case class Interface(override val service: ApiBuilderService, interface: models.Interface) extends ApiBuilderType {
    override val name: String = interface.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Interfaces
    val fields: Seq[ApiBuilderField] = interface.fields.map { f =>
      ApiBuilderField(this, f)
    }
  }
  case class Model(override val service: ApiBuilderService, model: models.Model) extends ApiBuilderType {
    override val name: String = model.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Models
    val fields: Seq[ApiBuilderField] = model.fields.map { f =>
      ApiBuilderField(this, f)
    }
  }
  case class Union(override val service: ApiBuilderService, union: models.Union) extends ApiBuilderType {
    override val name: String = union.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Unions
    val types: Seq[ApiBuilderUnionType] = union.types.map { t =>
      ApiBuilderUnionType(this, t)
    }
  }
}
