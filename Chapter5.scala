package com.oczeretko.streams

sealed trait Stream[+A] {

  import com.oczeretko.streams.Stream._

  def head: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def tail: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(h, t) => Some(t())
  }

  def split: Option[(A, Stream[A])] = this match {
    case Empty => None
    case Cons(h, t) => Some(h(), t())
  }

  def isEmpty : Boolean = this match {
    case Cons(_, _) => false
    case _ => true
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) Empty else cons(h(), t() take (n - 1))
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
  }

  def drop(n: Int): Stream[A] =
    if (n <= 0)
      this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t() drop (n - 1)
    }

  def dropWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) t() dropWhile (p) else this
  }

  def scanLeft[B](z: B)(f: (A, B) => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => {
      val acc = f(h(), z)
      cons(z, t().scanLeft(acc)(f))
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def head2: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def append[AA >: A](other: Stream[AA]): Stream[AA] = foldRight(other)((a, b) => cons(a, b))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  def zipWith[AA >: A, B](as: Stream[AA])(f: (AA, AA) => B): Stream[B] = {
    if (this.head.isEmpty || as.head.isEmpty) empty[B]
    else
      cons(f(this.head.get, as.head.get), this.tail.get.zipWith(as.tail.get)(f))
  }

  def map2[B](f: A => B): Stream[B] = unfold(this)(s => s.split.map { case (h, t) => (f(h), t)})

  def take2(n: Int): Stream[A] = unfold(this, n) {
    case (Cons(h, t), 0) => None
    case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhile3(p: A => Boolean) = unfold(this)(_.split.flatMap { case (h, t) => if (p(h)) Some(h, t) else None})

  def zipWith2[AA >: A, B](xs: Stream[AA])(f: (AA, AA) => B): Stream[B] =
    unfold(this, xs) {
      case (as, bs) => as match {
        case Empty => None
        case Cons(ha, ta) => xs match {
          case Empty => None
          case Cons(hb, tb) => Some(f(ha(), hb()), (ta(), tb()))
        }
      }
    }

  def zipWith3[AA >: A, B](xs: Stream[AA])(f: (AA, AA) => B): Stream[B] =
    unfold(this, xs) { case (as, bs) => as.split.flatMap { case (ha, ta) => bs.split.flatMap { case (hb, tb) => Some(f(ha, hb), (ta, tb))}}}

  def zipAll[B] (xs : Stream[B]) : Stream[(Option[A], Option[B])] = unfold (Some(this) : Option[Stream[A]], Some(xs) : Option[Stream[B]]) {
    case (None, None) => None
    case (as, bs) => Some((as.flatMap(_.head), bs.flatMap(_.head)), (as.flatMap(_.tail).filter(!_.isEmpty), bs.flatMap(_.tail).filter(!_.isEmpty)))
  }

  def startsWith[A](s: Stream[A]): Boolean = this zipAll s  takeWhile (!_._2.isEmpty) forAll ({ case (a,b) => a == b })

  def tails : Stream[Stream[A]] = cons(this, unfold(this) (_.split.map { case (h,t) => (t,t)}))

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists( _ startsWith s)

  def scanRight[B](z: B) (f: (A, B) => B) : Stream[B] =
    foldRight((z, Stream(z)))((a, bs) => {
      lazy val (b, s) = bs
      val b2 = f(a, b)
      (b2, cons(b2, s))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A] (h: => A, t: => Stream[A]) : Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A] : Stream[A] = Empty

  def apply[A] (as: A*) : Stream[A] = {
    if (as.isEmpty)
      Empty
    else
      cons(as.head, apply (as.tail : _*))
  }

  def constant[A] (a : A) : Stream[A] = cons(a, constant(a))

  def from(n : Int) : Stream[Int] = cons(n, from(n+1))

  def fibs : Stream[Int] = cons(1, cons(1, fibs.zipWith(fibs.drop(1))((f1, f2) => f1 + f2)))

  def unfold[A,S] (s : S) (f : S => Option[(A,S)]) : Stream[A] = f(s).map(pair => cons(pair._1, unfold(pair._2)(f))).getOrElse(empty[A])

  def constant2[A] (a : A) : Stream[A] = unfold(a) (a => Some(a,a))

  def from2 (n: Int)  = unfold(n) (n => Some(n, n+1))

  def fibs2 : Stream[Int] = unfold((1,1))(f_prev => Some(f_prev._1, (f_prev._2, f_prev._1 + f_prev._2)))
}
