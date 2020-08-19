package io.apibuilder.validation

import io.apibuilder.spec.v0.models.Method
import io.apibuilder.validation.helpers.Helpers
import play.api.libs.json._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class MultiServiceImplSpec extends AnyFunSpec with Matchers with Helpers {

  private[this] lazy val multi = MultiServiceImpl(
    List("flow-api-service.json", "apibuilder-api-service.json").map(loadService)
  )

  it("loads multiple services") {
    multi.services.map(_.service.name) should equal(Seq("API", "apibuilder api"))
  }

  it("bodyTypeFromPath") {
    multi.bodyTypeFromPath("POST", "/unknown/path/that/does/not/resolve") should equal(None)

    // resources from flow api
    multi.bodyTypeFromPath("POST", "/users").map(_.name) should equal(Some("user_form"))
    multi.bodyTypeFromPath("POST", "/:organization/webhooks").map(_.name) should equal(Some("webhook_form"))

    // resources from apidoc api
    multi.bodyTypeFromPath("POST", "/:orgKey").map(_.name) should equal(Some("application_form"))
  }

  it("operation") {
    multi.findOperation(Method.Post, "/foo/bar/baz") should be(None)

    val op = multi.findOperation("POST", "/users").get.operation
    op.method should equal(Method.Post)
    op.path should equal("/users")
    op.parameters should be(Nil)

    multi.findOperation("GET", "/users").get.operation.parameters.map(_.name) should be(
      Seq("id", "email", "status", "limit", "offset", "sort")
    )
    multi.findOperation("GET", "/org/experiences/items").get.operation.parameters.map(_.name) should be(
      Seq("organization", "number", "status", "experience", "country", "ip", "currency", "language", "limit", "offset", "sort")
    )
  }

  it("validate") {
    // path from flow api
    multi.upcastOperationBody(
      "POST",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required field for webhook_form: events"))
    )

    // path from apidoc api
    multi.upcastOperationBody(
      "POST",
      "/:orgKey",
      Json.obj("url" -> "https://test.flow.io")
    ) should equal(
      Left(Seq("Missing required fields for application_form: name, visibility"))
    )
  }

  it("error on missing array indices") {
    // comes from decoding customer[address][streets][]=33b Bay St
    multi.upcastOperationBody(
      "PUT",
      "/:organization/orders/:number",
      Json.parse(
        """
          |{
          |    "items": [],
          |    "customer": {
          |        "name": {
          |            "first": "Matt",
          |            "last": "Flow"
          |        },
          |        "phone": "1234567890",
          |        "email": "mkersner@flow.io",
          |        "address": {
          |            "name": {
          |                "first": "Matt",
          |                "last": "Flow"
          |            },
          |            "streets": {
          |                "": "33b Bay St"
          |            },
          |            "city": "Toronto",
          |            "province": "Ontario",
          |            "postal": "M5J 2Z3",
          |            "country": "CAN",
          |            "company": "Flow"
          |        }
          |    }
          |}
        """.stripMargin)
    ) should equal(
      Left(Seq("order_put_form.customer.address.streets of type '[string]': element in position[0] must be a string and not an object"))
    )
  }

  it("url query example") {
    val form = FormData.parseEncoded("name=John%20Doe&expiration_month=12&expiration_year=2017&cvv=123&cipher=VnHzBw%2BbaGrKZL0fimklhKupHJeowxK2Mqa9LbECCnb3R%2FxIgS1vr0sFg2mUGsXR7bsNV61UURB91VrWr19V1g%3D%3D&challenge%5Btext%5D=Flow&challenge%5Bcipher%5D=df2BQZykhnTfIVIX6Vg9yjUmyEprz3dLmUYU0O8GeyCZ0t3pn1nXSP7DRDfsZAASwtNupqyYx3G4W%2BmGlWQreg%3D%3D&callback=__flowjsonp0&method=post")

    val js = multi.upcastOperationBody(
      "POST",
      "/:organization/cards",
      Json.toJson(form)
    ).rightValue

    (js \ "name").as[JsString].value should equal("John Doe")
    (js \ "expiration_month").as[JsNumber].value should equal(12)
    (js \ "expiration_year").as[JsNumber].value should equal(2017)
    (js \ "cvv").as[JsString].value should equal("123")
  }

  it("simple number example") {
    val form = Json.obj(
      "url" -> 123,
      "events" -> 456
    )

    val js = multi.upcastOperationBody(
      "POST",
      "/:organization/webhooks",
      Json.toJson(form)
    ).rightValue

    (js \ "url").as[JsString].value should equal("123")
    (js \ "events").as[JsArray] should equal(JsArray(Seq(JsString("456"))))
  }

  it("offers validation error w/ verb replacement") {
    multi.upcastOperationBody(
      "OPTIONS",
      "/:organization/webhooks",
      Json.obj("url" -> "https://test.flow.io", "events" -> "*")
    ) should equal(
      Left(Seq(
        "HTTP method 'OPTIONS' not defined for path '/:organization/webhooks' - Available methods: GET, POST, PUT, DELETE"
      ))
    )
  }

  it("validate union type discriminator") {
    multi.upcastOperationBody(
      "POST",
      "/:organization/authorizations",
      Json.obj("discriminator" -> "authorization_form")
    ) match {
      case Left(errors) => errors.head.contains("Invalid discriminator 'authorization_form'") should be(true)
      case Right(_) => sys.error("Expected error")
    }
  }

  it("validate union type") {
    multi.upcastOperationBody(
      "POST",
      "/:organization/authorizations",
      Json.obj(
        "order_number" -> "123",
        "discriminator" -> "merchant_of_record_authorization_form"
      )
    ) should equal(
      Left(Seq("Missing required field for merchant_of_record_authorization_form: token"))
    )
  }

  it("validates array types") {
    multi.upcastOperationBody(
      "PUT",
      "/:organization/experiences/:experience_key/payment/method/rules",
      Json.obj()
    ) should equal(
      Left(Seq("Missing required fields for experience_payment_method_rule_form: payment_method_id, tags"))
    )
  }

}
