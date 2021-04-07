package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import play.api.libs.json.JsValue

/**
  * Wrapper to work with multiple API Builder services.
  * Takes an ordered list of services. If multiple
  * services define an http path, first one is selected.
  */
case class MultiServiceImpl(
  override val services: List[ApiBuilderService]
) extends MultiService {

  private[this] val validator = JsonValidator(services)
  private[this] val serviceResolver = ServiceOperationResolver(services)

  override def findOperation(method: Method, path: String): Option[ApiBuilderOperation] = {
    serviceResolver.findOperation(method, path)
  }

  override def findType(typ: TypeName): Option[AnyType] = {
    validator.findType(
      defaultNamespace = typ.namespace,
      name = typ.name,
    ).headOption
  }

  override def findTypes(typ: TypeName): Seq[AnyType] = {
    validator.findType(
      defaultNamespace = typ.namespace,
      name = typ.name,
    )
  }

  override def upcast(typ: AnyType, js: JsValue): Either[Seq[String], JsValue] = {
    validator.validateType(typ, js)
  }

}
