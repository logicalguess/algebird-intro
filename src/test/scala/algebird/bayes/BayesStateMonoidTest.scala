package algebird.bayes

import logicalguess.algebird.bayes._
import logicalguess.algebird.component.{Component, ComponentInput, ComponentState}
import org.scalatest.PropSpec
import thinkbayes.Pmf

/**
  * Created by logicalguess on 6/18/17.
  */
class BayesStateMonoidTest extends PropSpec {
  import logicalguess.algebird.component.Component._


  def dice() {
    val likelihood: (Int, Int) => Double = (d: Int, h: Int) => if (h < d) 0 else 1.0 / h
    val bayesMonoid = BayesStateMonoid(likelihood)

    println("Priors:")
    val pmf = Pmf(List(4, 6, 8, 12, 20))
    pmf.printChart()

    println()
    println("After a 6 is rolled:")
    val bm = bayesMonoid.plus(ComponentState(pmf), ComponentInput(List(6)))
    bm.state.printChart()

    println()
    println("After 6, 8, 7, 7, 5, 4 are rolled after the first 6:")
    val bm1 = bayesMonoid.plus(bm, ComponentInput(List(6, 8, 7, 7, 5, 4)))
    bm1.state.printChart()

    import thinkbayes.extensions.Plotting._
    val plot = pmf.showBar("prior", title = "Throwing Dice with BayesMonoid", xLabel = "Hypotheses")
    bm.state.plotBarOn(plot, "posterior1")
    bm1.state.plotBarOn(plot, "posterior2")
  }

  def monty() = {
    val hypos = List(1, 2, 3)
    val firstChoice = 1

    // likelihood of Monty opening a door given that a hypothesis about the winning one
    def likelihood(opened: Int, winner: Int): Double =
      (winner, opened) match {
        case (w, o) if w == o => 0.0 // if the door was opened, it is surely not the winning door
        case (w, _) if w == firstChoice => 1.0 / (hypos.length - 1) // Monty can open any door other than the winning one
        case _ => 1.0 / (hypos.length - 2) // Monty can open any door other than the winning one and the chosen one
      }

    val bayesMonoid = BayesStateMonoid[Int, Int](likelihood _)

    println("Before any door is opened:")
    val pmf: Pmf[Int] = Pmf(hypos)
    pmf.printChart()

    println()
    println("After Monty opens door 3:")
    val bm: Component[Int, Pmf[Int]] = bayesMonoid.plus(pmf, 3)
    bm.state.printChart()

    import thinkbayes.extensions.Plotting._
    val plot = pmf.showBar("prior", title = "Monty Hall (first choice is door 1, Monty opens door 3)",
      xLabel = "Hypotheses (Doors)")
    bm.state.plotBarOn(plot, "posterior")
  }

  property("dice") {
    dice()
  }

  property("monty") {
    monty()
  }

}

object TestApp extends App {
  new BayesStateMonoidTest().monty()
}
