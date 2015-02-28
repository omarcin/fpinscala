package com.oczeretko

import java.util.concurrent.Executors

object Main {

  def main(args: Array[String]): Unit = {
    import MyParsers._

    val testParser =
      string("XX") *--
      double **
      (string("MAMA") | string("TEST")) *--

    Console.println(MyParsers.run(testParser)("null"))

    //Prop.run(MyParsers.Laws.mapLaw(testParser)(SGen.listOf(Gen.choose('A'.toInt, 'Z'.toInt).map(_.toChar)).map(cs => new String(cs.toArray))))
    Prop.run(MyParsers.Laws.testValidJson)
  }






  def test1() : Unit = {
    val gi = Gen.choose(-100, Int.MaxValue / 2)
    val sg = SGen.listOf1(gi)

    val p = Prop.forAll(sg)(list => list.forall(_ <= list.max))
    Prop.run(p)


    val sp = Prop.forAll(sg)(list => list.sorted.foldLeft((Int.MinValue, true)) { case ((prev, res), item) => (item, res && prev <= item)}._2)
    Prop.run(sp)

    val es = Executors.newCachedThreadPool()
    val parallelProp = Prop.check(Par.map(Par.unit(1))(_ + 1).apply(es).get == Par.unit(2).apply(es).get)

    Prop.run(parallelProp)

    val genFun = Gen.funInt[Int].unsized.map(f => f andThen (i => i < Int.MaxValue / 2))
    val takeProp = Prop.forAll(genFun ** sg) { case (f, l) => l.takeWhile(f).forall(f) && l == l.takeWhile(f) ++ l.dropWhile(f)}

    Prop.run(takeProp)
  }
}