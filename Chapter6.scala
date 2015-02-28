package com.oczeretko

import com.oczeretko.Rand.Rand

trait RNG {
  def nextInt : (Int, RNG)
}

case class SimpleRNG (seed : Long) extends RNG{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }
}

object Rand {
  type Rand[+A] = State[RNG, A]
}

object RNG {
  def instance : RNG = SimpleRNG(System.currentTimeMillis())
  def int : Rand[Int] = State(_.nextInt)
  def unit[A](a :A) : Rand[A] = State.unit(a)
  def nextNonNegativeInt : Rand[Int] = int.map(n => if (n < 0) - (n + 1) else n)
  def double : Rand[Double] = nextNonNegativeInt.map(_.toDouble / Int.MaxValue.toDouble)
  def nonNegativeLessThan(n : Int) : Rand[Int] =
    nextNonNegativeInt.flatMap(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })
}

//  def ints (count: Int) (rng : RNG) : (List[Int], RNG) = {
//    if (count <= 0) (List.empty[Int], rng)
//    else {
//      val (ns, r) = ints (count - 1)(rng)
//      val (n, r1) = r.nextInt
//      (n :: ns, r1)
//    }
//  }
//
//  def ints2(count: Int)(rng : RNG) : (List[Int], RNG)  = {
//    @tailrec
//    def go(count: Int, acc : List[Int], rng : RNG) : (List[Int], RNG)=
//      if (count <= 0) (acc, rng)
//    else {
//        val (n, r) = rng.nextInt
//        go(count - 1, n :: acc, r)
//      }
//    go(count, List(), rng)
//  }
//
//
//  def nextDouble : RNG => (Double, RNG) = r => {
//    val (n, rng) = nextNonNegativeInt(r)
//    (n.toDouble/Int.MaxValue.toDouble, rng)
//  }
//
//  def nextNonNegativeEven : Rand[Int] =
//    RNG.map (nextNonNegativeInt)(i => i - i % 2)
//
//
//
//  def map2[A,B,C] (ra: Rand[A], rb : Rand[B])(f: (A, B) => C) : Rand[C] = rng => {
//    val (a, rng1) = ra(rng)
//    val (b, rng2) = rb(rng1)
//    (f(a,b), rng2)
//  }
//
//  def both[A,B] (ra: Rand[A], rb : Rand[B]) : Rand[(A,B)] = map2(ra,rb)((_,_))
//
//  def randIntDouble : Rand[(Int, Double)] = both(int, double)
//
//  def randDoubleInt : Rand[(Double, Int)] = both(double, int)
//
//  def sequence[A] (ras : List[Rand[A]]) : Rand[List[A]] = rng => ras match {
//    case Nil => (Nil, rng)
//    case ra_head :: ra_tail => {
//      val (a, rng1) = ra_head(rng)
//      val (as, rng_res) = sequence(ra_tail)(rng1)
//      (a :: as, rng_res)
//    }
//  }
//
//  def sequence2[A] (ras : List[Rand[A]]) : Rand[List[A]] =
//    ras.foldRight(unit(List.empty[A]))((ra, acc) => map2(ra, acc)(_ :: _))
//
//  def sequence3[A](ras : List[Rand[A]]): Rand[List[A]] =
//    map(ras.foldLeft(unit(List.empty[A]))((acc, ra) => map2(acc, ra)((a,b) => b :: a)))(_.reverse)
//
//  def ints (n : Int) : Rand[List[Int]] =
//    sequence2(List.fill[Rand[Int]](n)(int))
//
//  def flatMap[A,B](ra : Rand[A])(f : A => Rand[B]) : Rand[B] = r => {
//    val (a, r1) = ra(r)
//    f(a)(r1)
//  }
//
//
//  def mapByFlatMap[A,B](ra : Rand[A]) (f : A => B) : Rand[B] =
//    flatMap(ra)(a => unit(f(a)))
//
//  def map2ByFlatMap[A,B,C](ra : Rand[A], rb : Rand[B])(f : (A,B) => C) : Rand[C] =
//    flatMap(ra)(a => map(rb)(b => f(a,b)))
//
//
//  def rollDie : Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
//
//}
//
//
//
