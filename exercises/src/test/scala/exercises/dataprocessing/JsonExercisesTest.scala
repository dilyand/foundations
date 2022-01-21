package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import exercises.dataprocessing.JsonExercises._

class JsonExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  val john: Json = JsonObject(
    Map(
      "name" -> JsonString(" John Doe "),
      "age"  -> JsonNumber(25),
      "address" -> JsonObject(
        Map(
          "street-number" -> JsonNumber(25),
          "street-name"   -> JsonString("  Cody Road"),
          "door-number"   -> JsonNull,
          "business"      -> JsonBoolean(false)
        )
      )
    )
  )

  val jane: Json = JsonObject(
    Map(
      "name" -> JsonString(" Jane Doe "),
      "age"  -> JsonNumber(35),
      "address" -> JsonObject(
        Map(
          "street-number" -> JsonNumber(21),
          "street-name"   -> JsonString("  Toady Road"),
          "door-number"   -> JsonNull,
          "business"      -> JsonBoolean(true)
        )
      )
    )
  )

  val does: Json = JsonArray(List(john, jane))

  test("trimAll") {
    assert(
      trimAll(does) == JsonArray(
        List(
          JsonObject(
            Map(
              "name" -> JsonString("John Doe"),
              "age"  -> JsonNumber(25),
              "address" -> JsonObject(
                Map(
                  "street-number" -> JsonNumber(25),
                  "street-name"   -> JsonString("Cody Road"),
                  "door-number"   -> JsonNull,
                  "business"      -> JsonBoolean(false)
                )
              )
            )
          ),
          JsonObject(
            Map(
              "name" -> JsonString("Jane Doe"),
              "age"  -> JsonNumber(35),
              "address" -> JsonObject(
                Map(
                  "street-number" -> JsonNumber(21),
                  "street-name"   -> JsonString("Toady Road"),
                  "door-number"   -> JsonNull,
                  "business"      -> JsonBoolean(true)
                )
              )
            )
          )
        )
      )
    )
  }

  test("anonymize") {
    assert(
      anonymize(does) == JsonArray(
        List(
          JsonObject(
            Map(
              "name" -> JsonString("***"),
              "age"  -> JsonNumber(0),
              "address" -> JsonObject(
                Map(
                  "street-number" -> JsonNumber(0),
                  "street-name"   -> JsonString("***"),
                  "door-number"   -> JsonNull,
                  "business"      -> JsonBoolean(false)
                )
              )
            )
          ),
          JsonObject(
            Map(
              "name" -> JsonString("***"),
              "age"  -> JsonNumber(0),
              "address" -> JsonObject(
                Map(
                  "street-number" -> JsonNumber(0),
                  "street-name"   -> JsonString("***"),
                  "door-number"   -> JsonNull,
                  "business"      -> JsonBoolean(true)
                )
              )
            )
          )
        )
      )
    )
  }

  test("search") {
    assert(search(JsonObject(Map.empty), "ll", 10) == false)
    assert(search(JsonNumber(5), "ll", 10) == false)
    assert(search(JsonString("Hello"), "ll", 10) == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ll", 10) == true)
    assert(search(JsonObject(Map("message" -> JsonString("Hello"))), "ss", 10) == false)
    assert(search(JsonObject(Map("message" -> JsonString("hi"))), "ll", 10) == false)
    assert(search(does, "ll", 10) == false)
    assert(search(does, "John", 10) == true)
    assert(search(does, "Ja", 10) == true)

    assert(search(JsonObject(Map("user" -> JsonObject(Map("name" -> JsonString("John"))))), "o", 2) == true)
    assert(search(JsonObject(Map("user" -> JsonObject(Map("name" -> JsonString("John"))))), "o", 1) == false)
  }

  test("depth") {
    assert(depth(JsonNumber(1)) == 0)
    assert(depth(JsonObject(Map.empty)) == 0)
    assert(depth(JsonObject(Map("k" -> JsonNumber(1)))) == 1)
    assert(depth(john) == 2)
    assert(depth(does) == 3)
  }

}
