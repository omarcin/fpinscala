package com.oczeretko.list

import scala.annotation.tailrec

sealed trait List[+A] { }

case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] ={
    if(as.isEmpty) Nil
    else Cons(as.head, apply (as.tail: _*))
  }

  @tailrec
  def drop[A] (xs : List[A], num : Int) : List[A] = {
    if (num <= 0) xs
    else xs match {
      case Nil => Nil
      case Cons(x, xs2) => drop(xs2, num - 1)
    }
  }

  def tail[A] (xs: List[A]) = drop(xs, 1)

  def setHead[A] (x: A, xs: List[A]) = {
    xs match {
      case Nil => Cons(x, Nil)
      case Cons(x1, xs1) => Cons(x, xs1)
    }
  }

  @tailrec
  def dropWhile[A] (l: List[A], f: A => Boolean) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else l
    }
  }

  def dropWhile2[A] (l :List[A]) (f : A => Boolean) : List[A] = {
    dropWhile(l, f)
  }

  def foldRight[A,B] (l : List[A], z : B) (f: (A,  B) => B) : B = {
    l match {
      case Nil => z
      case Cons(h, t) => f (h, foldRight(t, z) (f))
    }
  }

  @tailrec
  def foldLeft[A,B] (l : List[A], z : B) (f: (A,  B) => B) : B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(h, z)) (f)
    }
  }

  def flatten [A] (ls : List[List[A]]) : List[A] = {
    foldRight(ls, Nil : List[A]) (List.append)
  }

  def reverse [A] (l : List[A]) : List[A] = {
    @tailrec
    def revRec [A] (l : List[A], acc : List[A]) : List[A] = {
      l match {
        case Nil => acc
        case Cons(h,t) => revRec(t, Cons(h, acc))
      }
    }

    revRec(l, Nil)
  }

  def mapRev[A,B]  (l : List[A])(f : A => B) : List[B] = {
    @tailrec
    def mapRec(l : List[A], acc : List[B]) (f : A => B)  : List[B]= {
      l match {
        case Cons(hL, tL) => mapRec(tL, Cons(f(hL), acc))(f)
        case Nil => acc
      }
    }
    mapRec(l, Nil)(f)
  }

  def map[A,B] (l : List[A])(f : A => B) : List[B] = {
    reverse(mapRev(l)(f))
  }

  def sum (l : List[Int]) = {
    foldRight(l, 0)(_ + _)
  }

  def product (l : List[Double]) = {
    foldRight(l, 1.0)(_ * _)
  }

  def append[A] (l1 : List[A], l2 : List[A]) : List[A] = {
    @tailrec
    def appendRec(l1: List[A], l2 : List[A], acc : List[A]) : List[A] = {
      l1 match {
        case Cons(h1, t1) => appendRec(t1, l2, Cons(h1, acc))
        case Nil =>
          acc match {
            case Nil => l2
            case Cons (hA, tA) => appendRec(Nil, Cons(hA, l2), tA)
          }
      }
    }
    appendRec(l1, l2, Nil)
  }

  def stringify[A] (l : List[A]) ={
    map(l)(_.toString)
  }

  def filter [A] (l : List[A]) (f : A => Boolean) ={
    @tailrec
    def filterRevRec (l: List[A], acc : List[A]) (f : A => Boolean) : List[A] ={
      l match {
        case Nil => acc
        case Cons(h, t) => filterRevRec(t, Cons(h, acc))(f)
      }
    }
    reverse(filterRevRec(l, Nil)(f))
  }

  def flatMap [A,B] (l: List[A])(f: A => List[B]) : List[B] = {
    flatten(map(l)(f))
  }

  def filter2[A] (l : List[A]) (f : A => Boolean) = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def zipWith[A,B,C] (as : List[A], bs : List[B])(f : (A,B) => C) : List[C] = {
    @tailrec
    def zipWithRevRec [A,B,C] (as : List[A], bs : List[B], acc : List[C])(f : (A,B) => C) : List[C] = {
      as match {
        case Nil => acc
        case Cons(hA, tA) =>
          bs match {
            case Nil => acc
            case Cons(hB, tB) => zipWithRevRec(tA, tB, Cons(f(hA, hB), acc))(f)
          }
      }
    }

  reverse(zipWithRevRec(as, bs, Nil: List[C]) (f))
  }

  def length[A] (l: List[A]) = {
    foldRight(l, 0)((a,b) => b + 1)
  }
}