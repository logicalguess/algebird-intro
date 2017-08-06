package algebird.component

/**
  * Created by logicalguess on 7/31/17.
  */

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import com.twitter.algebird.{Aggregator, Semigroup}
import logicalguess.algebird.component._
import logicalguess.algebird.sgd.SGDStateMonoid
import org.scalatest.{Matchers, WordSpecLike}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class SGDComponentTest extends WordSpecLike with Matchers {

  "Aggregator as interactor" should {
    "component" in {

      implicit val system = ActorSystem("akka-stream-partial-functions")
      implicit val materializer = ActorMaterializer()


      val sgdMonoid = SGDStateMonoid(0.0001)

      type SGDComponent = Component[(Double, IndexedSeq[Double]), IndexedSeq[Double]]

      val aggregator = new Aggregator[(Double, IndexedSeq[Double]), SGDComponent, IndexedSeq[Double]] {
        override def prepare(d: (Double, IndexedSeq[Double])): SGDComponent = ComponentInput(List(d))
        override def semigroup: Semigroup[SGDComponent] = sgdMonoid
        override def present(c: SGDComponent): IndexedSeq[Double] = c.state
      }

      //val interactor = SemigroupInteractor(aggregator.prepare _, aggregator.semigroup, aggregator.present _)

      val interactor = AggregatorInteractor(aggregator)
      val component: LogicComponent[(Double, IndexedSeq[Double]), IndexedSeq[Double], SGDComponent] =
        LogicComponent(IndexedSeq[Double](0.0, 0.0), interactor, Sink.foreach(println))

      val dataSource = scala.io.Source.fromFile("src/main/resources/data.csv").getLines()
      for (entry <- dataSource) {
        // read entries and stream them to the component
        val Array(x: Double, y: Double) = entry.split(",").map(_.trim.toDouble)
        component.receive(y, IndexedSeq(x))
      }

      //Vector(1.5785046049371114, 0.03369039016126796)

      //StdIn.readLine()
      Thread.sleep(3000)

      val whenTerminated = system.terminate()
      Await.result(whenTerminated, Duration.Inf)
    }
  }

}

