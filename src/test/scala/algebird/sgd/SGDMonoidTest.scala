package algebird.sgd

import com.twitter.algebird.SGD.dot
import com.twitter.algebird._
import org.scalatest.PropSpec

class SGDMonoidTest extends PropSpec {

  implicit def SGDToSGDWeights(sgd: SGD[_]) = sgd.asInstanceOf[SGDWeights]
  implicit def SGDPosToSGDWeights(sgd: SGDPos[_]) = sgd.asInstanceOf[SGDWeights]
  implicit def SGDToSGDWPos[Pos](sgd: SGD[Pos]) = sgd.asInstanceOf[SGDPos[Pos]]

  def printError(w: IndexedSeq[Double], pos: SGDPos[(Double, IndexedSeq[Double])]) = {
    var err = 0.0
    for (p <- pos.pos) {
      val xsPlusConst = p._2 :+ 1.0
      err += Math.pow(dot(w, xsPlusConst) - p._1, 2)
    }
    println(s"weights = $w, error = ${err/pos.pos.size}")
  }

  property("sgd with monoid") {
    val smallStepMonoid = new SGDMonoid(SGD.constantStep(0.0001), SGD.linearGradient)

    val dataSource = scala.io.Source.fromFile("src/main/resources/data.csv").getLines()

    var dataPoints: SGDPos[(Double, IndexedSeq[Double])] = SGDPos(List())
    for (entry <- dataSource) {
      // read entries and add them to the data set using the monoid
      val Array(x, y) = entry.split(",").map(_.trim.toDouble)
      dataPoints = smallStepMonoid.plus(dataPoints, SGDPos((y, IndexedSeq(x))))
    }

    printError(IndexedSeq(0.0, 0.0), dataPoints) // weights = Vector(0.0, 0.0), error = 5565.107834483211

    // update the weights using the monoid
    val w = smallStepMonoid.plus(SGDWeights(IndexedSeq(0.0, 0.0)), dataPoints).weights
    printError(w, dataPoints) //weights = Vector(1.5785046049371114, 0.03369039016126796), error = 137.39686567659004

  }
}