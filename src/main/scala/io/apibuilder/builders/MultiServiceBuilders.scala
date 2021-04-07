package io.apibuilder.builders

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.validation.{ApiBuilderService, ApiBuilderType, MultiService, TypeName}

trait MultiServiceBuilders {

  def makeMultiService(service: ApiBuilderService): MultiService = {
    MultiService(Seq(service).toList)
  }

  def makeMultiService(service: Service): MultiService = {
    makeMultiService(Seq(service))
  }

  def makeMultiService(
    services: Seq[Service] = Nil,
  ): MultiService = {
    MultiService(services.map(ApiBuilderService.apply).toList)
  }

  def findApiBuilderType(multiService: MultiService, name: String): Option[ApiBuilderType] = {
    val types = TypeName.parse(name) match {
      case None => multiService.allTypes.filter(_.name == name)
      case Some(tn) => multiService.findType(tn).toSeq
    }

    types.toList match {
      case Nil => None
      case one :: Nil => Some(one).collect { case t: ApiBuilderType => t }
      case _ => sys.error(s"Multiple types found with name '$name'")
    }
  }

  def mustFindApiBuilderType(multiService: MultiService, name: String): ApiBuilderType = {
    findApiBuilderType(multiService, name).getOrElse {
      sys.error(s"Cannot find type: $name")
    }
  }

  def findEnum(multiService: MultiService, name: String): Option[ApiBuilderType.Enum] = {
    findApiBuilderType(multiService, name).collect { case t: ApiBuilderType.Enum => t }
  }

  def findEnum(apiBuilderService: ApiBuilderService, name: String): Option[ApiBuilderType.Enum] = {
    findEnum(makeMultiService(apiBuilderService), name)
  }

  def mustFindEnum(service: ApiBuilderService, name: String): ApiBuilderType.Enum = {
    mustFindEnum(makeMultiService(service), name)
  }

  def mustFindEnum(multiService: MultiService, name: String): ApiBuilderType.Enum = {
    mustFindApiBuilderType(multiService, name) match {
      case t: ApiBuilderType.Enum => t
      case _ => sys.error(s"Type '${name}' is not an enum")
    }
  }

  def findInterface(apiBuilderService: ApiBuilderService, name: String): Option[ApiBuilderType.Interface] = {
    findInterface(makeMultiService(apiBuilderService), name)
  }

  def findInterface(multiService: MultiService, name: String): Option[ApiBuilderType.Interface] = {
    findApiBuilderType(multiService, name).collect { case t: ApiBuilderType.Interface => t }
  }

  def mustFindInterface(service: ApiBuilderService, name: String): ApiBuilderType.Interface = {
    mustFindInterface(makeMultiService(service), name)
  }

  def mustFindInterface(multiService: MultiService, name: String): ApiBuilderType.Interface = {
    mustFindApiBuilderType(multiService, name) match {
      case t: ApiBuilderType.Interface => t
      case _ => sys.error(s"Type '${name}' is not an interface")
    }
  }

  def findModel(apiBuilderService: ApiBuilderService, name: String): Option[ApiBuilderType.Model] = {
    findModel(makeMultiService(apiBuilderService), name)
  }

  def findModel(multiService: MultiService, name: String): Option[ApiBuilderType.Model] = {
    findApiBuilderType(multiService, name).collect { case t: ApiBuilderType.Model => t }
  }

  def mustFindModel(service: ApiBuilderService, name: String): ApiBuilderType.Model = {
    mustFindModel(makeMultiService(service), name)
  }

  def mustFindModel(multiService: MultiService, name: String): ApiBuilderType.Model = {
    mustFindApiBuilderType(multiService, name) match {
      case t: ApiBuilderType.Model => t
      case _ => sys.error(s"Type '${name}' is not a model")
    }
  }

  def findUnion(apiBuilderService: ApiBuilderService, name: String): Option[ApiBuilderType.Union] = {
    findUnion(makeMultiService(apiBuilderService), name)
  }

  def findUnion(multiService: MultiService, name: String): Option[ApiBuilderType.Union] = {
    findApiBuilderType(multiService, name).collect { case t: ApiBuilderType.Union => t }
  }

  def mustFindUnion(service: ApiBuilderService, name: String): ApiBuilderType.Union = {
    mustFindUnion(makeMultiService(service), name)
  }

  def mustFindUnion(multiService: MultiService, name: String): ApiBuilderType.Union = {
    mustFindApiBuilderType(multiService, name) match {
      case t: ApiBuilderType.Union => t
      case _ => sys.error(s"Type '${name}' is not a union")
    }
  }

}
