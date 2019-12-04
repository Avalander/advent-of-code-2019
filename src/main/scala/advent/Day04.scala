package advent

object Day04 {
  def validPasswords (from: Int, to: Int): Seq[Int] =
    for {
      a <- 2 to 9
      b <- a to 9
      c <- b to 9
      d <- c to 9
      e <- d to 9
      f <- e to 9
      digits = List(a, b, c, d, e, f)
      groups = digits groupBy (x => x)
      if groups.values exists (_.size > 1)
      num = digits.mkString.toInt
      if num <= to && num >= from
    } yield num

  def validPasswords2 (from: Int, to: Int): Seq[Int] =
    for {
      a <- 2 to 9
      b <- a to 9
      c <- b to 9
      d <- c to 9
      e <- d to 9
      f <- e to 9
      digits = List(a, b, c, d, e, f)
      groups = digits groupBy (x => x)
      if groups.values exists (_.size == 2)
      num = digits.mkString.toInt
      if num <= to && num >= from
    } yield num

  def main (args: Array[String]): Unit = {
    val result1 = validPasswords(278384, 824795).size
    println(s"Problem 1: $result1")

    val result2 = validPasswords2(278384, 824795).size
    println(s"Problem 2: $result2")
  }
}
