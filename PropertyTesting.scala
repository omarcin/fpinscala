package com.oczeretko

import java.util.concurrent.Executors

import com.oczeretko.Prop.{MaxSize, TestCases}

case class SGen[+A](forSize : Int => Gen[A]) {
  def apply(size : Int) : Gen[A] =  this.forSize(size)
  def flatMap[B](f : A => Gen[B]) : SGen[B] =
    SGen(this.forSize andThen (_.flatMap(f)))

  def map[B] (f : A => B) : SGen[B] =
    SGen(this.forSize andThen (_.map(f)))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(size => this.apply(size) ** s2.apply(size))
}

object SGen {
  def listOf[A](g : Gen[A]) : SGen[List[A]] = SGen(size => g.listOfN(size))
  def listOf1[A](g : Gen[A]) : SGen[List[A]] = SGen(size => g.listOfN(if(size==0) 1 else size))
  def unit[A](a : A) : SGen[A] = Gen.unit(a).unsized
  def string : SGen[String] = SGen(Gen.stringOfLen)
  def double : SGen[Double] = SGen(size => Gen.union(Gen.double.map(_ * size), Gen.double.map(_ * -size)))
}

case class Gen[+A](sample: State[RNG, A]) {
  import Gen._

  def map[B] (f : A => B) : Gen[B] = this.flatMap(a => unit(f(a)))
  def flatMap[B](f : A => Gen[B]) : Gen[B] =
    Gen(this.sample.flatMap(f(_).sample))

  def listOfN(n: Int) : Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(sample)))

  def listOfN(n: Gen[Int]) : Gen[List[A]] =
    for {
      size <- n
      list <- this.listOfN(size)
    } yield list

  def unsized : SGen[A] = SGen(_ => this)

  def **[B] (g2 : Gen[B]) : Gen[(A,B)] =
    Gen(for {
      a <- this.sample
      b <- g2.sample
    } yield(a,b))
}

object Gen {

  def choose(start : Int, end : Int) : Gen[Int] =
    Gen(RNG.nonNegativeLessThan(end-start).map(_ + start))

  def unit[A](a : A) : Gen[A] =
    Gen(State.unit[RNG, A](a))

  def boolean : Gen[Boolean] =
    Gen(RNG.nonNegativeLessThan(2).map(_ == 0))

  def double : Gen[Double] =
    Gen(RNG.double)

  def union[A] (g1 : Gen[A], g2 : Gen[A]) : Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def char : Gen[Char] =
    choose('A'.toInt, 'Z'.toInt).map(_.toChar)

  def stringOfLen(len : Int) : Gen[String] =
    char.listOfN(len).map(cs => new String(cs.toArray))

  def weighted[A] (g1 : (Gen[A], Double), g2 : (Gen[A], Double)) : Gen[A] = {
    val t = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(RNG.double).flatMap(d => if (d < t) g1._1 else g2._1)
  }

  def funInt[A] : Gen[A => Int] =
    Gen(RNG.int.map(randomValue => a => a.hashCode() * randomValue))
}

sealed trait Result {
  def isFailed : Boolean
  def flatMap(f : Boolean => Result) : Result = f(this.isFailed)
  def getOrElse(r : => Result) = this.flatMap(if(_) this else r)
}

case object Passed extends Result {
  def isFailed = false
}

case object Proved extends Result { def isFailed = false }

case class Failed(failure: String, testCasesPassed: TestCases) extends Result {
  override def isFailed: Boolean = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other : Prop) : Prop = {
    Prop((max, tc, r) => {
      this.run(max ,tc, r)
          .getOrElse(other.run(max, tc, r))
    })
  }
  def ||(other : Prop) : Prop = {
    Prop((max, tc, r) => {
      val thisResult = this.run(max, tc, r)
      thisResult.flatMap(if (_) other.run(max, tc, r) else thisResult)
    })
  }

  def tag(msg : String) : Prop =
    Prop(this.run(_, _, _) match {
      case Passed => Passed
      case Proved => Proved
      case Failed(failure, num) => Failed(msg + "\n" + failure, num)
    })
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String

//  def forAll[A](genA: SGen[A])(prop: A => Boolean): Prop = {
//    Prop((max, tc, rng) => {
//
//      Stream.fill(tc)(genA).scanLeft((true, rng, 0)) {
//        case ((acc, r, count), gA) =>
//          if (!acc) (false, r, count + 1)
//          else {
//            val (a, r2) = gA.sample.run(r)
//            (prop(a), r2, count + 1)
//          }
//      }
//        .filter { case (b, _, _) => b}
//        .headOption
//        .map({ case (_, _, tc) => Failed("", tc)})
//        .getOrElse(Passed)
//    })
//  }

  def forAll[A](genA: SGen[A])(prop: A => Boolean): Prop = {
    Prop((max, tc, rng) => {
      def calcSize (i : Int) : Int = Math.floor(i.toDouble * (max.toDouble / tc.toDouble)).toInt

      unfold((rng, 0)) { case (r, i) => {
          val (a, r2) = genA.apply(calcSize(i)).sample.run(r)
          Some(a, (r2, i + 1))
          }
        }
        .zip(Stream.from(1))
        .take(tc)
        .map({ case (a, i) =>
        try {
          if (prop(a)) Passed
          else Failed(a.toString, i)
        }
        catch {
          case e: Exception => Failed(e.toString, i)
        }
      })
      .find(_.isFailed)
      .headOption
      .getOrElse(Passed)
    })
  }

  def check(p: => Boolean) : Prop =
    Prop((_, _, _) =>
      if(p) Proved else Failed("Falsified", 1))

  def run(p : Prop, maxSize : Int = 100, tests : Int = 100, rng : RNG = RNG.instance) : Unit =
    p.run(maxSize, tests, rng) match {
      case Passed => println("OK. Passed " + tests + " test cases.")
      case Proved => println("OK. Proved.")
      case Failed(f,tc) => println("False! After " + tc + " test cases.\n" + f)
    }

  private def unfold[A, B](b: B)(f: B => Option[(A, B)]): Stream[A] =
    f(b)
      .map { case (a, b1) => Stream.cons[A](a, unfold(b1)(f))}
      .getOrElse(Stream.empty[A])
}

object PropMain {
  def main() : Unit = {
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
