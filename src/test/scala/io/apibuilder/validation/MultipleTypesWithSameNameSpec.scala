package io.apibuilder.validation

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import play.api.libs.json.Json

class MultipleTypesWithSameNameSpec extends AnyFunSpec with Matchers
  with helpers.Helpers
  with helpers.ApiBuilderServiceHelpers
{

  it("validates type found in multiple services") {
    // service1: item.price has an amount
    // service2: product.price has an amount and a currency
    val service1 = makeService(
      models = Seq(
        makeModel(
          name = "price",
          fields = Seq(
            makeField("amount")
          )
        ),
        makeModel(
          name = "item",
          fields = Seq(
            makeField("value", `type` = "price")
          )
        )
      )
    )

    val service2 = makeService(
      models = Seq(
        makeModel(
          name = "price",
          fields = Seq(
            makeField("amount"),
            makeField("currency")
          )
        ),
        makeModel(
          name = "product",
          fields = Seq(
            makeField("value", `type` = "price")
          )
        )
      )
    )
    val multi = MultiServiceImpl(
      List(
        ApiBuilderService(service1), ApiBuilderService(service2)
      )
    )

    multi.upcast(
      "item",
      service1.namespace,
      Json.obj("value" -> Json.obj())
    ) should equal(
      Left(Seq("Missing required field for price: amount"))
    )

    multi.upcast(
      "product",
      service2.namespace,
      Json.obj("value" -> Json.obj())
    ) should equal(
      Left(Seq("Missing required fields for price: amount, currency"))
    )
  }

}
