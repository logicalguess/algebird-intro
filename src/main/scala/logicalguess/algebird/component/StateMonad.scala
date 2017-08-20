package logicalguess.algebird.component


trait StateMonad[S, A] {
  val run: S => (S, A)

  def apply(s: S): (S, A) =
    run(s)

  def eval(s: S): A =
    apply(s)._2

  def map[B](f: A => B): StateMonad[S, B] = StateMonad { s: S =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => StateMonad[S, B]): StateMonad[S, B] = StateMonad { s: S =>
    val (s1, a) = run(s)
    f(a)(s1)
  }
}

object StateMonad {
  def apply[S, A](f: S => (S, A)): StateMonad[S, A] = new StateMonad[S, A] {
    final val run = f
  }

  def state[S, A](a: A): StateMonad[S, A] = StateMonad { s: S => (s, a) }
  def get[S]: StateMonad[S, S] = StateMonad { s: S => (s, s) }
  def gets[S, A](f: S => A): StateMonad[S, A] = StateMonad { s: S => (s, f(s)) }
  def modify[S](f: S => S): StateMonad[S, Unit] = StateMonad { s: S => (f(s), ()) }
}

trait ComponentMonad[I, S, O] extends StateMonad[S, O] {
  def logic: PartialFunction[(I, S), S]
  def transformer: Function[S, O]

  val run = { s: S => (s, transformer(s)) }

  def process(input: I): StateMonad[S, O] = {
    for {
      updated <-  StateMonad {s: S =>
        val s1 = logic.apply((input, s))
        (s1, transformer(s1))}
      //_ <- StateMonad.modify { s: S => logic.apply((input, s)) }
    } yield updated
  }
}

object Test {
  def main (args: Array[String] ): Unit = {
    val c = new ComponentMonad[Int, Int, Int] {
      override def logic: PartialFunction[(Int, Int), Int] = { case (i, j) => i + j }
      override def transformer: Function[Int, Int] = identity
    }

    var m = for {
      _ <- c.process(2)
      _ <- c.process(3)
      res <- c.process(5)
    } yield res

    println(m.run(1))

    val sm = List(2, 3, 5).foldLeft(StateMonad.state[Int, Int](0))((m, in) => StateMonad { s: Int =>
      val s1 = c.logic.apply((in, m.run(s)._1))
      (s1, c.transformer(s1))
    })

    println(sm.run(1))

  }
}
