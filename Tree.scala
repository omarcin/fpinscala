package com.oczeretko.tree

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A] (left : Tree[A], right : Tree[A]) extends Tree[A]


object Tree {

  def size[A] (t : Tree[A]) : Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum (t : Tree[Int]) : Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A] (t: Tree[A]) : Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def fold[A,B] (t: Tree[A]) (f_leaf : A => B, f_branch : (B, B) => B) : B = {
    val foldRec =  (t: Tree[A]) => fold(t)(f_leaf, f_branch)

    t match {
      case Leaf(v) => f_leaf(v)
      case Branch(l, r) => f_branch(foldRec(l), foldRec(r))
    }
  }

  def size2[A] (t:Tree[A]) : Int =
    fold[A,Int](t)(a => 1, (b1,b2) => 1 + b1 + b2)

}