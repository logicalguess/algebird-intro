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

  def map2[S, A, B, C](ma: StateMonad[S, A], mb: StateMonad[S, B])(f: (A, B) => C): StateMonad[S, C] =
    ma.flatMap(a => mb.map(b => f(a, b)))

  def sequence[S, A](lma: List[StateMonad[S, A]]): StateMonad[S, List[A]] =
    lma.foldRight(state[S, List[A]](List[A]()))((ma, mla) => map2[S, A, List[A], List[A]](ma, mla)(_ :: _))

  def chain[S, A](lma: List[StateMonad[S, A]]): StateMonad[S, A] =
    lma.foldRight(state[S, List[A]](List[A]()))((ma, mla) => map2[S, A, List[A], List[A]](ma, mla)(_ :: _)).map(_.last)


  def traverse[S, A, B](la: List[A])(f: A => StateMonad[S, B]): StateMonad[S, List[B]] =
    la.foldRight(state[S, List[B]](List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

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

  def update(input: I): StateMonad[S, Unit] = {
    StateMonad.modify { s: S => logic.apply((input, s)) }
  }
}

object Test {

  def main (args: Array[String] ): Unit = {
    val c = new ComponentMonad[Int, Int, Int] {
      override def logic: PartialFunction[(Int, Int), Int] = { case (i, j) => i + j }
      override def transformer: Function[Int, Int] = 2*_
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

    val seqm = StateMonad.sequence(List(2, 3, 5).map(c.process))
    println(seqm.run(1))

    val chainm = StateMonad.chain(List(2, 3, 5).map(c.process))
    println(chainm.run(1))


    val updm: StateMonad[Int, List[Unit]] = StateMonad.sequence[Int, Unit](List(2, 3, 5).map(c.update))
    println(updm.run(1)._1)

    val sum: (Int, Int, Int) => Int = _ + _ + _
    val sumCurried: Int => Int => Int => Int = sum.curried

    val f: Int => Int = sum.curried(1)(2)
    val g: Int => Int = sum(1, 2, _: Int)

    println(f(3)) //6
    println(g(3)) //6

    def factory[A](a: A): StateMonad[A => Any, Any] = {
      type X = A => Any
      StateMonad[X, Any] { f =>
        val x: X = if (f.apply(a).isInstanceOf[X]) f.apply(a).asInstanceOf[X] else identity

        (x, f.apply(a))
      }
    }

    val fm = StateMonad.chain(List(2, 3, 5).map(factory))
    println(fm.run(sum.curried)._2)

    val concat: (String, String, String) => String = _ + _ + _
    val fm1 = StateMonad.chain(List("a", "b", "c").map(factory))
    println(fm1.run(concat.curried)._2)

    object fs {
      def comb(list: Any*): String = {
        list.mkString
      }

      def comb1(i: Int, s1: String, s2: String): String = i + s1 + s2

    }

    val args = List(2, "b", "c")
    println(fs.comb(args: _*))

    import scala.reflect.runtime.universe._
    val im = runtimeMirror(this.getClass.getClassLoader).reflect(fs)

    val method = im.symbol.typeSignature.member(TermName("comb")).asMethod
    println("comb: " + im.reflectMethod(method)(args))

    val method1 = im.symbol.typeSignature.member(TermName("comb1")).asMethod
    println("comb1: " + im.reflectMethod(method1)(args: _*))

  }
}
