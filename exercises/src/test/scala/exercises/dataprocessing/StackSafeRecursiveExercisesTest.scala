package exercises.dataprocessing

import exercises.dataprocessing.StackSafeRecursiveExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class StackSafeRecursiveExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  val largeSize = 100000

  test("unsafeSum is not stack-safe") {
    try {
      unsafeSum(List.fill(largeSize)(0))
      fail("Expected stack overflow")
    } catch {
      case _: StackOverflowError => succeed
      case e: Throwable          => fail(e)
    }
  }

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
    assert(sum(List.fill(largeSize)(0)) == 0)
  }

  test("sum is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)).contains(1))
    assert(min(Nil).isEmpty)
    assert(min(List.fill(largeSize)(0)).contains(0))
  }

  test("min is consistent with std library") {
    forAll { (numbers: List[Int]) =>
      def safeStdMin(numbers: List[Int]): Option[Int] =
        try Some(numbers.min)
        catch {
          case _: Throwable => None
        }

      assert(min(numbers) == safeStdMin(numbers))
    }
  }

  test("reverse") {
    assert(reverse(List(2, 5, 1, 8)) == List(8, 1, 5, 2))
    assert(reverse(Nil) == Nil)

    val largeL = List.range(0, largeSize)
    assert(reverse(largeL) == largeL.reverse)
  }

  test("foldLeft is consistent with std library") {
    forAll { (items: List[Int], default: String, combine: (String, Int) => String) =>
      assert(foldLeft(items, default)(combine) == items.foldLeft(default)(combine))
    }
  }

}
