package exercises.dataprocessing

/** When used with combine, the default value must be a no-op.
  * Combine must be associative.
  */
trait Monoid[A] {
  def default: A
  def combine(f: A, s: A): A
}

object Monoid {
  val sumInt: Monoid[Int] = new Monoid[Int] {
    override def default: Int                 = 0
    override def combine(f: Int, s: Int): Int = f + s
  }

  val sumDouble: Monoid[Double] = new Monoid[Double] {
    override def default: Double                       = 0.0
    override def combine(f: Double, s: Double): Double = f + s
  }

  val sumDoubleIntTuple: Monoid[(Double, Int)] = zip(sumDouble, sumInt)

  val minSample: Monoid[Option[Sample]] = new Monoid[Option[Sample]] {
    override def default: Option[Sample] = None

    override def combine(f: Option[Sample], s: Option[Sample]): Option[Sample] = (f, s) match {
      case (None, None)         => None
      case (Some(_), None)      => f
      case (None, Some(_))      => s
      case (Some(s1), Some(s2)) => if (s1.temperatureFahrenheit < s2.temperatureFahrenheit) Some(s1) else Some(s2)
    }
  }

  def zip[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def default: (A, B)                       = (ma.default, mb.default)
    override def combine(f: (A, B), s: (A, B)): (A, B) = (ma.combine(f._1, s._1), mb.combine(f._2, s._2))
  }
}
