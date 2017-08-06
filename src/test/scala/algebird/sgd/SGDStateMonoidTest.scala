package algebird.sgd

import logicalguess.algebird.component.ComponentInput
import logicalguess.algebird.sgd.SGDStateMonoid
import org.scalatest.PropSpec

class SGDStateMonoidTest extends PropSpec {
  import SGDStateMonoid._

  property("sgd with monoid") {
    val smallStepMonoid = SGDStateMonoid(0.0001)

    val dataSource = scala.io.Source.fromFile("src/main/resources/data.csv").getLines()

    var dataPoints: ComponentInput[(Double, IndexedSeq[Double])] = ComponentInput(List())
    for (entry <- dataSource) {
      // read entries and add them to the data set using the monoid
      val Array(x, y) = entry.split(",").map(_.trim.toDouble)
      dataPoints = smallStepMonoid.plus(dataPoints, (y, IndexedSeq(x)))
    }

    printError(IndexedSeq(0.0, 0.0), dataPoints.inputs) // weights = Vector(0.0, 0.0), error = 5565.107834483211

    // update the weights using the monoid
    val w = smallStepMonoid.plus(IndexedSeq(0.0, 0.0), dataPoints).state
    printError(w, dataPoints.inputs) //weights = Vector(1.5785046049371114, 0.03369039016126796), error = 137.39686567659004

  }
}