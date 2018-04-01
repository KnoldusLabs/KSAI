
val ss = Array(Array(1), Array(2), Array(3), Array(4))
val y = 1
val p = for{
  re <- 0 until 4
} yield {
  val newSS = ss(re)(y)+1
  newSS
}

p.toArray
