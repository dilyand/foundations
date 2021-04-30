package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits contains only digits") {
    forAll { (text: String) =>
        selectDigits(text).foreach(c => assert(c.isDigit))
      }
  }

  test("secret contains only *") {
    forAll { (text: String) =>
      secret(text).foreach(c => assert(c == '*'))
    }
  }

  test("secret must be idempotent") {
    forAll { (text: String) =>
      val once = secret(text)
      val twice = secret(secret(text))
      assert(once == twice)
    }
  }

  test("isValidUsernameCharacter should work with examples") {
    assert(isValidUsernameCharacter('3'))
    assert(isValidUsernameCharacter('a'))
    assert(isValidUsernameCharacter('A'))
    assert(isValidUsernameCharacter('_'))
    assert(isValidUsernameCharacter('-'))
    assert(!isValidUsernameCharacter('!'))
  }

  test("isValidUsername should work with examples") {
    assert(isValidUsername("john-doe"))
    assert(!isValidUsername("*john*"))
  }

  test("isValidUsername should be idempotent") {
    forAll { (text: String) =>
      val first = isValidUsername(text)
      val second = isValidUsername(text)
      assert(first == second)
    }
  }

  test("isValidUsername should not be affected by reverse, toLowerCase or toUpperCase") {
    forAll { (text: String) =>
      val base = isValidUsername(text)
      val rev = isValidUsername(text.reverse)
      val lower = isValidUsername(text.toLowerCase)
      val upper = isValidUsername(text.toUpperCase)
      assert(base == rev == lower == upper)
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("isPositive should work with examples") {
    assert(Point(2, 4, 9).isPositive)
    assert(Point(0, 0, 0).isPositive)
    assert(!Point(0, -2, 1).isPositive)
  }

  test("isPositive should be true") {
    forAll { (a: Int, b: Int, c: Int) =>
      assert(Point(a.max(0), b.max(0), c.max(0)).isPositive)
    }
  }

  test("isEven should work with examples") {
    assert(Point(2, 4, 8).isEven)
    assert(Point(0, -8, -2).isEven)
    assert(!Point(3, -2, 0).isEven)
  }

  test("isEven should return the same result if a an even Point's coordinates are multiplied by the same number") {
    forAll { (x: Int, y: Int, z: Int, m: Int) =>
      if (Point(x, y, z)isEven) assert(Point(x * m, y * m, z * m).isEven)
    }
  }
}
