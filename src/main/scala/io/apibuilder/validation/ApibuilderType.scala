package io.apibuilder.validation

import io.apibuilder.spec.v0.models

sealed trait ApibuilderType {

  def service: models.Service

  def namespace: String = typeName.namespace

  def name: String = typeName.name

  /**
   * returns the fully qualified name of this type (ie including the namespace)
   */
  def qualified: String = s"$namespace.$typeDiscriminator.$name"

  protected def typeName: TypeName
  protected def typeDiscriminator: String
}

object ApibuilderType {
  case class Enum(service: models.Service, enum: models.Enum) extends ApibuilderType {
    override val typeName: TypeName = TypeName.parse(name = enum.name, defaultNamespace = service.namespace)
    override val typeDiscriminator = "enums"
  }
  case class Model(service: models.Service, model: models.Model) extends ApibuilderType {
    override val typeName: TypeName = TypeName.parse(name = model.name, defaultNamespace = service.namespace)
    override val typeDiscriminator = "models"
  }
  case class Union(service: models.Service, union: models.Union) extends ApibuilderType {
    override val typeName: TypeName = TypeName.parse(name = union.name, defaultNamespace = service.namespace)
    override val typeDiscriminator = "unions"
  }
}
