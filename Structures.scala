/**
 * Created by marcinoc on 2015-01-09.
 */
class Structures {

  trait Stack[+A] {
    def push[AA >: A](a : AA)
    def pop [AA >: A](a : AA)
  }

//  class ListStack[A] extends Stack[A] {
//
//    override def push[AA >: A](a: AA): Stack[AA] = a :: stack
//
//    override def pop[AA >: A](a: AA): Unit = stack match {
//      case Nil => throw new Exception("Empty stack")
//      case h :: t => {
//        stack = t
//
//      }
//    }
//  }



}
