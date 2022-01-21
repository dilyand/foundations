package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature is consistent with List min") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.toList.minByOption(_.temperatureFahrenheit))
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature PBT") {
    forAll { (samples: ParList[Sample]) =>
      averageTemperature(samples) match {
        case None => assert(samples.toList.isEmpty)
        case Some(avg) =>
          val multiplied = samples.map(s => s.copy(temperatureFahrenheit = s.temperatureFahrenheit * 2))
          averageTemperature(multiplied) match {
            case None         => fail("multiplied List isEmpty")
            case Some(newAvg) => assert(avg * 2 == newAvg)
          }
      }
    }
  }

  test("size is consistent with List.size") {
    forAll { (samples: ParList[Sample]) =>
      assert(samples.size == samples.toList.size)
    }
  }

  test("monoFoldLeft sum") {
    forAll { (ints: ParList[Int]) =>
      val expectation = ints.toList.sum
      val result      = ints.monoFoldLeft(Monoid.sumInt)

      assert(result == expectation)
    }
  }

  test("foldMap(identity) is consistent with monoFoldLeft") {
    forAll { (ints: ParList[Int]) =>
      val expectation = ints.monoFoldLeft(Monoid.sumInt)
      val result      = ints.foldMap(identity)(Monoid.sumInt)

      assert(result == expectation)
    }
  }

  def noopDefaultMonoid[A](name: String, params: Monoid[A], gen: Gen[A]) =
    test(s"Monoid $name has a default that yields no-op when used in combine") {
      forAll(gen) { (value: A) =>
        assert(params.combine(params.default, value) == value)
        assert(params.combine(value, params.default) == value)
      }
    }

  def associativeMonoid[A](name: String, params: Monoid[A], gen: Gen[A]) =
    test(s"Monoid $name has an associative combine method") {
      forAll(gen, gen, gen) { (v1: A, v2: A, v3: A) =>
        assert(params.combine(v1, params.combine(v2, v3)) == params.combine(params.combine(v1, v2), v3))
      }
    }

  def testMonoid[A](name: String, params: Monoid[A], gen: Gen[A]) = {
    noopDefaultMonoid(name, params, gen)
    associativeMonoid(name, params, gen)
  }

  val intGen: Gen[Int]                      = Gen.choose(Int.MinValue, Int.MaxValue)
  val doubleGen: Gen[Double]                = Gen.choose(-100.0f, 100.0f).map(_.toDouble)
  val stringGen: Gen[String]                = Gen.alphaStr
  val doubleIntTupleGen: Gen[(Double, Int)] = Gen.zip(doubleGen, intGen)
  val optionSampleGen: Gen[Option[Sample]]  = Gen.option(sampleGen)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  testMonoid("sumInt", Monoid.sumInt, intGen)
  testMonoid("sumDouble", Monoid.sumDouble, doubleGen)
  testMonoid("zip", Monoid.zip(Monoid.sumDouble, Monoid.sumInt), doubleIntTupleGen)
  testMonoid("minSample", Monoid.minSample, optionSampleGen)

  testMonoid(
    "concatString",
    new Monoid[String] {
      override def default                       = ""
      override def combine(f: String, s: String) = s"$f$s"
    },
    stringGen
  )

  ignore("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
