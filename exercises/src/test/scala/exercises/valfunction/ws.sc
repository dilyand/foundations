case class Point(x: Int, y: Int, z: Int) {
  // 2a. Implement `isPositive` which returns true if `x`, `y` and `z` are all greater or equal to 0, false otherwise
  // such as Point(2, 4, 9).isPositive == true
  //         Point(0, 0, 0).isPositive == true
  // but     Point(0, -2, 1).isPositive == false
  // Note: `isPositive` is a function defined within `Point` class, so `isPositive` has access to `x`, `y` and `z`.
  def isPositive: Boolean =
    List(x, y, z).forall(_ >= 0)
}

val point = Point(2, 4, 9)
val q = Int.MinValue.abs
val newPoint = point.copy(point.x * q, point.y * q, point.z * q)
point.isPositive
newPoint.isPositive
