package fpinscala.ch06

case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldLeft(unit(Nil): State[S, List[A]]) { (acc, sa) =>
      sa.map2(acc)(_ :: _)
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

object CandyDispenser {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def transition(machine: Machine, input: Input): Machine =
    (machine, input) match {
      case (Machine(true, _, _), Turn) => machine
      case (Machine(false, _, _), Coin) => machine
      case (Machine(_, 0, _), _) => machine
      case (Machine(true, candies, coins), Coin) => Machine(false, candies, coins + 1)
      case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(
        inputs.map(input =>
            State.modify[Machine](m => transition(m, input))
        )
      )
      m <- State.get
    } yield (m.candies, m.coins)
}
