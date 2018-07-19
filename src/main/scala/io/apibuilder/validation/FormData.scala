package io.apibuilder.validation

import java.net.{URLDecoder, URLEncoder}

import play.api.libs.json._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Convert any url form encoded params (query or body) to a Json
  * object. Makes best guesses on types.
  */
object FormData {

  private[this] val Encoding: String = "UTF-8"

  /**
    * Given a url encoded string, parses it and then reformats as url
    * encoded. Main use case is to turn things like:
    *
    * number[]=100379876543&number[]=WT65xSPLX-SPT-5
    *
    * into
    *
    * number=100379876543&number=WT65xSPLX-SPT-5
    */
  def rewriteEncoded(value: String): String = {
    toEncoded(
      toJson(
        parseEncoded(value)
      )
    )
  }

  /**
    * Converts the specified js value into a url form encoded string,
    * recursively through all types.
    *
    * @param keys Keeps track of the top level keys we are parsing to
    *             build up nested keys (e.g. user[first] for maps)
    * @param numberFormat optional user supplied number formatting function
    */
  def toEncoded(
    js: JsValue,
    keys: Seq[String] = Nil,
    numberFormat: (BigDecimal) => String = defaultNumberFormat
  ): String = {
    def urlEncode(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")
    def encodeIt(value: String, keys: Seq[String]) = encode(urlEncode(value), keys)
    def encodeNumber(value: BigDecimal, keys: Seq[String]) = encode(urlEncode(numberFormat(value)), keys)

    js match {
      case o: JsObject => {
        o.value.map { case (key, value) =>
          toEncoded(value, keys ++ Seq(key), numberFormat)
        }.mkString("&")
      }
      case o: JsArray => {
        o.value.zipWithIndex.map { case (v, i) =>
          toEncoded(v, keys ++ Seq(i.toString), numberFormat)
        }.mkString("&")
      }
      case o: JsString => encodeIt(o.value, keys)
      case o: JsBoolean => encodeIt(o.value.toString, keys)
      case o: JsNumber => encodeNumber(o.value, keys)
      case JsNull => encodeIt("", keys)
      case other => encodeIt(other.toString, keys)
    }
  }

  private[this] def defaultNumberFormat(value: BigDecimal): String = value.toString

  private[this] def encode(value: String, keys: Seq[String] = Nil): String = {
    keys.toList match {
      case Nil => value
      case one :: rest => {
        s"%s=%s".format(buildKey(one, rest), value)
      }
    }
  }

  @scala.annotation.tailrec
  private[this] def buildKey(result: String, values: Seq[String]): String = {
    values.toList match {
      case Nil => result
      case one :: rest => buildKey(s"$result[$one]", rest)
    }
  }

  /**
    * Parses a url encoded string into a Json Object
    */
  def parseEncodedToJsObject(value: String): JsObject = {
    FormData.toJson(
      FormData.parseEncoded(value)
    )
  }

  /**
    * Parses a url encoded string into a map
    */
  def parseEncoded(value: String): Map[String, Seq[String]] = {
    val data = scala.collection.mutable.Map[String, Seq[String]]()
    value.split("&").foreach { x =>
      x.split("=", 2).toList match {
        case key :: rest if key.nonEmpty => {
          val decodedValue = rest.headOption.getOrElse("") match {
            case "" => null
            case v => URLDecoder.decode(v, Encoding)
          }

          val values = data.get(key) match {
            case None => Seq(decodedValue)
            case Some(existing) => existing ++ Seq(decodedValue)
          }
          data += URLDecoder.decode(key, Encoding) -> values
        }

        case _ => {
          // Ignore
        }
      }
    }
    data.toMap
  }

  def toJson(data: Map[String, Seq[String]]): JsObject = {
    toJson(data.keys.toSeq, data)
  }

  @tailrec
  private[this] def toJson(
    keys: Seq[String],
    data: Map[String, Seq[String]],
    finalObject: JsObject = Json.obj()
  ): JsObject = {
    keys.headOption match {
      case None => {
        finalObject
      }

      case Some(key) => {
        val jsValue = data(key) match {
          case Nil => JsNull
          case one :: Nil => toJsPrimitive(one)
          case multiple => JsArray(multiple.map(toJsPrimitive))
        }

        val i = key.lastIndexOf('[')
        val thisObject = if (i < 0) {
          Json.obj(key -> jsValue)
        } else {
          toJsonObject(key, jsValue)
        }

        toJson(
          keys.tail,
          data,
          mergeObjects(finalObject, thisObject)
        )
      }
    }
  }

  // Based on play deepMerge method, but also merges values of underlying arrays
  private[this] def mergeObjects(existingObject: JsObject, otherObject: JsObject): JsObject = {
    val result = existingObject.value ++ otherObject.value.map {
      case (otherKey, otherValue) =>
        val maybeExistingValue = existingObject.value.get(otherKey)

        val newValue = (maybeExistingValue, otherValue) match {
          case (Some(e: JsObject), o: JsObject) => mergeObjects(e, o)
          case (Some(e: JsArray), o: JsArray) => mergeArrays(e, o)
          case _ => otherValue
        }

        otherKey -> newValue
    }
    JsObject(result)
  }

  /**
    * Merge two arrays, preserving order where possible. Use case is for
    * when we have two arrays sparsely populated, e.g:
    *   one: [1]
    *   two: [null, 2]
    * in this case we return [1, 2]
    */
  private[this] def mergeArrays(one: JsArray, two: JsArray): JsArray = {
    val length = Seq(one.value.length, two.value.length).max
    JsArray(
      0.until(length).map { i =>
        (one.value.lift(i).filter(_ != JsNull), two.value.lift(i).filter(_ != JsNull)) match {
          case (Some(a), None) => a
          case (None, Some(b)) => b
          case (Some(a), Some(b)) => (a, b) match {
            case (o1: JsObject, o2: JsObject) => mergeObjects(o1, o2)
            case (o1: JsArray, o2: JsArray) => mergeArrays(o1, o2)
            case _ => a
          }
          case (None, None) => JsNull
        }
      }
    )
  }

  private[this] val EndsWithIndexInBrackets = """^(.+)\[(\d+)\]$""".r
  private[this] val EndsWithEmptyBrackets = """^(.+)\[\]$""".r
  private[this] val EndsWithFieldNameInBrackets = """^(.+)\[(.+)\]$""".r

  // Given input of:
  //   locations[0][city] => Paris
  // Produce
  // { "locations": [
  //     { "city": "Paris" }
  //   ]
  // }
  @tailrec
  private[this] def toJsonObject(key: String, value: JsValue): JsObject = {
    // println(s"toJsonObject key[$key] value: $value")

    key match {
      case EndsWithIndexInBrackets(prefix, index) => {
        // Fill in JsNull up to our desired index to preserve the explicit
        // element order in the arrays
        toJsonObject(
          prefix,
          JsArray(
            0.until(index.toInt).map { _ => JsNull } ++ Seq(value)
          )
        )
      }

      case EndsWithEmptyBrackets(prefix) => {
        value match {
          case _: JsArray => toJsonObject(prefix, value)
          case _ => toJsonObject(prefix, JsArray(Seq(value)))
        }
      }

      case EndsWithFieldNameInBrackets(prefix, name) => {
        toJsonObject(prefix, Json.obj(name -> value))
      }

      case _ => {
        Json.obj(key -> value)
      }
    }
  }

  private[this] def toJsPrimitive(value: String): JsValue = {
    value match {
      case null => JsNull
      case "true" => JsBoolean(true)
      case "false" => JsBoolean(false)
      case other => {
        toNumber(other) match {
          case Some(v) => JsNumber(v)
          case None => JsString(other)
        }
      }
    }
  }

  private[this] val AcceptableRegexp = """^\-?[0-9]+$""".r

  def toNumber(value: String): Option[BigDecimal] = {
    value match {
      case AcceptableRegexp() => {
        Try {
          BigDecimal(value)
        } match {
          case Success(num) => Some(num)
          case Failure(_) => None
        }
      }
      case _ => None
    }
  }

}
