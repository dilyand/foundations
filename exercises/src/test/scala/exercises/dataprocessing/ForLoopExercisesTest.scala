package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size is consistent with List size") {
    forAll { (list: List[Int]) =>
      assert(size(list) == list.size)
    }
  }

  test("concatenating Lists creates a new List whose size is the sum of the original Lists' sizes") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      assert(size(l1 ++ l2) == size(l1) + size(l2))
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)).contains(1))
    assert(min(Nil).isEmpty)
  }

  test("concatenating two non-empty Lists creates a new List whose min is the min of the mins of the original Lists") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      (l1, l2) match {
        case (Nil, Nil) => assert(min(l1 ++ l2).isEmpty)
        case (_, Nil)   => assert(min(l1 ++ l2) == min(l1))
        case (Nil, _)   => assert(min(l1 ++ l2) == min(l2))
        case (_, _)     => assert(min(l1 ++ l2) == min(List(min(l1).get, (min(l2).get))))
      }
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("count is strictly positive") {
    forAll { (words: List[String]) =>
      for (freq <- wordCount(words).values)
        assert(freq > 0)
    }
  }

  test("for every word in words, there's a matching entry in the word count map") {
    forAll { (words: List[String]) =>
      val keys = wordCount(words).keys.toList
      words.foreach(w => assert(keys.contains(w)))
    }
  }

  test("foldLeft processes items from left to right") {
    forAll { (l: List[Int]) =>
      val res = foldLeft[Int, List[Int]](l, Nil)((acc, e) => e :: acc)
      assert(res == l.reverse)
    }
  }

  test("map is consistent with List map") {
    forAll { (list: List[Int], update: Int => Int) =>
      assert(map[Int, Int](list)(update) == list.map(update))
    }
  }

  test("reverse is consistent with List reverse") {
    forAll { (list: List[Int]) =>
      assert(reverse(list) == list.reverse)
    }
  }

  test("reverse examples") {
    assert(reverse(List(3, 8, 1)) == List(1, 8, 3))
    assert(reverse(Nil) == Nil)
  }

  test("lastOption examples") {
    assert(lastOption(List(3, 8, 1)).contains(1))
    assert(lastOption(Nil).isEmpty)
  }

  test("lastOption is consistent with List lastOption") {
    forAll { (list: List[Int]) =>
      assert(lastOption(list) == list.lastOption)
    }
  }

  test("generalMin examples") {
    assert(generalMin(List(1L, 2L, 3L)).contains(1L))
    assert(generalMin(List('A', 'B', 'C')).contains('A'))
    assert(generalMin(List("abc", "abc", "abcd", "xyz")).contains("abc"))
    assert(generalMin[String](Nil).isEmpty)
  }
}
