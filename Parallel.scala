package com.oczeretko

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

import Parallel.Parallel.Par

package Parallel{
  package object Parallel {
    type Par[A] = ExecutorService => Future[A]
  }
}

object Par {

  /* spawn */
  def unit[A] (a : A) : Par[A] = (es: ExecutorService) => UnitFuture(a)
  def fork[A] (a : => Par[A]) : Par[A] = es => {
    es.submit(new Callable[A] {
      override def call(): A = run(es)(a).get
    })
  }

  /* evaluate */
  def run[A](executor : ExecutorService)(p : Par[A]) : Future[A] = p(executor)

  /* high level */
  def map2[A,B,C](pa : Par[A], pb : Par[B])(f : (A,B) => C) : Par[C] = es => {

    val a = run(es)(pa).get
    val b = run(es)(pb).get
    UnitFuture(f(a,b))
  }

  /* derived */
  def lazyUnit[A] (a : => A) : Par[A] = fork(unit(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sequence[A](pas : List[Par[A]]) : Par[List[A]] = {
    pas.foldRight(unit(List.empty[A]))(Par.map2(_, _)(_ :: _))
  }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc :Par[C])(f : (A,B,C) => D) : Par[D] = {
    map2(map2(pa, pb)((a,b) => (a,b)), pc) { case ((a,b), c) => f(a,b,c) }
  }

  def chooser[A,B](pa: Par[A])(pgen : A => Par[B]) : Par[B] = es => {
    val a = run(es)(pa).get
    run(es)(pgen(a))
  }

  def flatMap[A,B] : Par[A] => (A => Par[B]) => Par[B] = chooser

  def choice[A](cond : Par[Boolean])(t : Par[A])(f : Par[A]) : Par[A] =
    flatMap(cond)(if (_) t else f)

  def choiceN[A](num : Par[Int])(choices : Int => Par[A]) : Par[A] =
    flatMap(num)(n => choices(n))

  def join[A](p : Par[Par[A]]) : Par[A] = es => {
    run(es)(run(es)(p).get())
  }

  def join2[A](p : Par[Par[A]]) : Par[A] =
    flatMap(p)(identity)

  def flatMap2[A,B] : Par[A] => (A => Par[B]) => Par[B] =
    pa => f => join(map(pa)(f))

  private case class UnitFuture[A](value : A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def get(): A = value
    override def get(timeout: Long, unit: TimeUnit): A = value
    override def isDone: Boolean = true
  }
}

object ParallelDesign {

  def sum(ints : IndexedSeq[Int]) : Par[Int] = Par.map(reduce(ints)(_+_))(_.getOrElse(0))
  def max(ints : IndexedSeq[Int]) : Par[Option[Int]] = reduce(ints)(Math.max)

  def reduce[A](vals : IndexedSeq[A])(f : (A,A) => A) : Par[Option[A]] = {
    if(vals.size <= 1)
      Par.unit(vals.headOption)
    else {

      def foption(a : Option[A],b : Option[A]) : Option[A] =
        OptionExt.lift2(f)(a,b).orElse(a).orElse(b)

      val (l, r) = vals.splitAt(vals.length / 2)
      Par.map2(Par.fork(reduce(l)(f)), Par.fork(reduce(r)(f)))(foption)
    }
  }
}

object OptionExt {
  def lift2[A,B,C](f : (A,B) => C)(val1 : Option[A], val2 :Option[B]) : Option[C] =
    (for { v1 <- val1; v2 <- val2 } yield f(v1, v2))
}

object Temp {
  def asyncF[A,B](f: A => B): A => Par[B] = a => {
    Par.lazyUnit(f(a))
  }

  def sortPar(parList : Par[List[Int]]) : Par[List[Int]] = Par.map(parList)(_.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = Par.fork {
    val parB: List[Par[B]] = ps map asyncF(f)
    Par.sequence(parB)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = Par.fork {
    val maybeRes: Par[List[List[A]]] = parMap(as)(a => if (f(a)) List(a) else List())
    Par.map(maybeRes)(_.flatten)
  }
}


