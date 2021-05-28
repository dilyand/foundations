package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  import Predicate._
  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(1, 0).swap == Pair(0, 1))
  }

  test("Pair map") {
    assert(Pair("John", "Doe").map(identity) == Pair("John", "Doe"))
  }

  test("Pair decoded") {}

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
  }

  test("Pair map3") {
    assert(Pair(0, 2).map3_(Pair(3, 4), Pair(5, 6))((x, y, z) => (x + y) - z) == Pair(-2, 0))
  }

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert((isEven && isPositive)(12))
    assert(!(isEven && isPositive)(11))
    assert(!(isEven && isPositive)(-4))
    assert(!(isEven && isPositive)(-7))
  }

  test("Predicate && PBT") {
    forAll { (v: Int, f1: Int => Boolean, f2: Int => Boolean) =>
      val p1 = Predicate(f1)
      val p2 = Predicate(f2)

      assert(!(p1 && False)(v))
      assert((p1 && False)(v) == (p2 && False)(v))
      assert((p1 && True)(v) == p1(v))
      assert((p1 && p2)(v) == (p2 && p1)(v))
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12))
    assert((isEven || isPositive)(11))
    assert((isEven || isPositive)(-4))
    assert(!(isEven || isPositive)(-7))
  }

  test("Predicate || PBT") {
    forAll { (v: Int, f1: Int => Boolean, f2: Int => Boolean) =>
      val p1 = Predicate(f1)
      val p2 = Predicate(f2)

      assert((p1 || True)(v))
      assert((p1 || p2 || True)(v))
      assert((p1 || True)(v) == (p2 || True)(v))
      assert((p1 || False)(v) == p1(v))
      assert((p1 || p2 || False)(v) == (p1 || p2)(v))
      assert((p1 || p2)(v) == (p2 || p1)(v))
    }
  }

  test("Predicate flip") {
    assert(isEven.flip(11))
  }

  test("Predicate flip PBT") {
    forAll { (v: Int, f1: Int => Boolean, f2: Int => Boolean) =>
      assert(False.flip(v))
      assert(!True.flip(v))
    }
  }

  test("isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(!isValidUser(User("John", 17)))
    assert(!isValidUser(User("john", 20)))
    assert(!isValidUser(User("X", 23)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {}

  test("JsonDecoder LocalDate") {}

  test("JsonDecoder weirdLocalDateDecoder") {}

}
