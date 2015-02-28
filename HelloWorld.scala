import Either._


object HelloWorld {

  def main (args: Array[String]) = {

    val l1 : List[Either[Nothing, Int]] = List(Right(1), Right(2), Right(3), Right(4))
    val l2 = List(Right(1), Right(2), Right(3), Right(4), Left("ERROR 1"), Left ("ERR 2"))

    println(l1)
    println(sequence (l1))
    println(l2)
    println(sequence (l2))
  }
}
