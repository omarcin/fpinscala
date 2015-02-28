package com.oczeretko

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked : Boolean, candies: Int, coins: Int)

object Machine {

  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(simulateMachine))
    s <- get
  } yield (s.candies, s.coins)

  def simulateMachine(input: Input): State[Machine, Unit] =
    modify(
      m => (input, m) match {

        /* invalid moves */
        case (Turn, Machine(true, _, _)) => m
        case (Coin, Machine(false, _, _)) => m

        /* no more candy :( */
        case (_, Machine(_, 0, _)) => m

        /* two valid moves */
        case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
        case (Coin, Machine(true, ca, co)) => Machine(false, ca, co + 1)
    })
}