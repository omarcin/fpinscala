package com.oczeretko

//type State[S, +A] = S => (A, S)

case class State[S, +A](run: S => (A,S)) {

  import State._

  def map2[B](f: A => B): State[S, B] = State(s => {
    val (a, state1) = this.run(s)
    (f(a), state1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb : State[S,B])(f : (A,B) => C) : State[S,C] = {
    this.flatMap(a => sb.map(b => f(a,b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, state1) = this.run(s)
    f(a).run(state1)
  })
}


object State {
  def unit[S, A] (a : A) : State[S, A] = State((a,_))

  def sequence[S,A] (sas : List[State[S,A]]) : State[S, List[A]] =
    sas.foldRight(unit(List.empty[A]) : State[S, List[A]])((sa, acc) => sa.map2(acc)(_ :: _))

  def get[S] : State[S,S] = State(s => (s,s))

  def set[S](s : S) : State[S, Unit] = State(_ => ((), s))

  def modify[S](f : S => S) : State[S, Unit] =
    get.flatMap(s => set(s).map(_ => ()))

  def modify2[S](f : S => S) : State[S, Unit] = for {
    s <- get
    r <- set(s)
  } yield r


}