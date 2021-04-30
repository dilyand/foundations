import exercises.generic.GenericFunctionExercises._

secret.map(bytes => new String(bytes.toArray).reverse).swap
productNames.zipWith(productPrices)((n, p) => Product(n, p))

case class ASL(age: Int, sex: String, location: String)
val ages = Pair(31, 32)
val sexes = Pair("m", "f")
val locations = Pair("London", "Paris")

// map3 without zipWith
ages.map3[String, String, ASL](sexes, locations)(ASL)
