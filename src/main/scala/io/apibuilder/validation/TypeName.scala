package io.apibuilder.validation

case class TypeName(
  name: String,
  namespace: Option[String]
)

object TypeName {

  def apply(name: String): TypeName = {
    val parts = name.split("\\.").toList

    if (parts.lengthCompare(3) >= 0) {
      TypeName(
        name = parts.last,
        namespace = Some(parts.slice(0, parts.length-3).mkString("."))
      )
    } else {
      TypeName(
        name = parts.last,
        namespace = None
      )
    }
  }

}
