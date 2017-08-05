package algebird.component

/**
  * Created by logicalguess on 7/31/17.
  */

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import com.twitter.algebird.{Aggregator, Semigroup}
import logicalguess.algebird.bayes._
import logicalguess.algebird.component._
import org.scalatest.{Matchers, WordSpecLike}
import thinkbayes.Pmf

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.StdIn

class ComponentTest extends WordSpecLike with Matchers {

  "Aggregator as interactor" should {
    "component" in {

      implicit val system = ActorSystem("akka-stream-partial-functions")
      implicit val materializer = ActorMaterializer()

      val hypos = List(1, 2, 3)
      val firstChoice = 1

      // likelihood of Monty opening a door given that a hypothesis about the winning one
      def likelihood(opened: Int, winner: Int): Double =
        (winner, opened) match {
          case (w, o) if w == o => 0.0 // if the door was opened, it is surely not the winning door
          case (w, _) if w == firstChoice => 1.0 / (hypos.length - 1) // Monty can open any door other than the winning one
          case _ => 1.0 / (hypos.length - 2) // Monty can open any door other than the winning one and the chosen one
        }

//      val interactor = PFInteractor[Int, Pmf[Int], Pmf[Int]]({ case (d, pmf) =>
//        pmf.map { case (h, prob) => (h, prob * likelihood(d, h)) }.normalized
//      }, identity)
//
//      val component = LogicComponent(Pmf[Int](hypos), interactor, Sink.foreach(println))


//      val bayesMonoid = BayesMonoid[Int, Int](likelihood _)
//
//      val aggregator = new Aggregator[Int, Bayes[Int, Int], Pmf[Int]] {
//        override def prepare(d: Int): Bayes[Int, Int] = BayesData(List(d))
//        override def semigroup: Semigroup[Bayes[Int, Int]] = bayesMonoid
//        override def present(bpmf: Bayes[Int, Int]): Pmf[Int] = bpmf.asInstanceOf[BayesPmf[Int]].pmf
//      }

      val bayesMonoid = BayesStateMonoid[Int, Int](likelihood _)

      type Bayes[I, H] = Component[I, Pmf[H]]

      val aggregator = new Aggregator[Int, Component[Int, Pmf[Int]], Pmf[Int]] {
        override def prepare(d: Int): Component[Int, Pmf[Int]] = ComponentInput(List(d))
        override def semigroup: Semigroup[Component[Int, Pmf[Int]]] = bayesMonoid
        override def present(c: Component[Int, Pmf[Int]]): Pmf[Int] = c.state
      }

      //val interactor = SemigroupInteractor(aggregator.prepare _, aggregator.semigroup, aggregator.present _)

      val interactor = AggregatorInteractor(aggregator)
      val component: LogicComponent[Int, Pmf[Int], Bayes[Int, Int]] =
        LogicComponent(Pmf[Int](hypos), interactor, Sink.foreach(println))


      List(3)
        .foreach(component.receive)

      val whenTerminated = system.terminate()
      Await.result(whenTerminated, Duration.Inf)

    }

    "wait" in {
      StdIn.readLine()
    }
  }

}

