package algebird.sgd

import com.twitter.algebird.SGD.dot
import com.twitter.algebird._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec

class SGDLaws extends PropSpec {

  implicit val sgdMonoid = new SGDMonoid(SGD.constantStep(0.001), SGD.linearGradient)
  val zeroStepMonoid = new SGDMonoid(SGD.constantStep(0.0), SGD.linearGradient)

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


  val (m, b) = (2.0, 4.0)
  val eps = 1e-3

  val sgdPosGen = for (
    x <- Gen.choose(0.0, 1.0);
    n <- Gen.choose(0.0, 0.001)
  ) yield SGDPos((m * x + b + n, IndexedSeq(x)))

  val sgdWGen = for (
    cnt <- Gen.choose(0L, 100000L);
    m <- Gen.choose(-10.0, 10.0);
    b <- Gen.choose(-10.0, 10.0)
  ) yield SGDWeights(cnt, Vector(m, b))

  val zeroGen = Gen.const(SGDZero)

  implicit val sgdPos = Arbitrary(sgdPosGen)
  implicit val sgdWArb = Arbitrary(sgdWGen)
  implicit val sgdArb: Arbitrary[SGD[(Double, IndexedSeq[Double])]] = Arbitrary {
    Gen.oneOf(sgdWGen, sgdPosGen, zeroGen)
  }

//  property("is a Monoid") {
//    monoidLaws[SGD[(Double, IndexedSeq[Double])]]
//  }

  property("Gradient is zero on the line") {
    forAll { (w: SGDWeights, x: Double) =>
      val m = w.weights(0)
      val b = w.weights(1)
      val y = m * x + b

      (y.isInfinity || {
        val pos = (y, IndexedSeq(x))
        val grad = SGD.linearGradient(w.weights, pos)
        (scala.math.abs(grad(0)) < eps) && (scala.math.abs(grad(1)) < eps)
      })
    }
  }

  property("Gradient at x=0 has zero first component") {
    forAll { (w: SGDWeights, y: Double) =>
      (SGD.linearGradient(w.weights, (y, IndexedSeq(0.0)))(0) == 0.0)
    }
  }

  property("Zero-step leaves Weights unchanged") {
    forAll {
      (w: SGDWeights, pos: SGDPos[(Double, IndexedSeq[Double])]) =>
        //val next = zeroStepMonoid.newWeights(w, pos.pos.head)
        val next = oneStepMonoid.plus(w, pos)
        (next.weights == w.weights && next.count == (w.count + 1L))
    }
  }

  def minus(x: IndexedSeq[Double], y: IndexedSeq[Double]): IndexedSeq[Double] = {
    x.zip(y).map { case (x: Double, y: Double) => x - y }
  }

  val oneStepMonoid = new SGDMonoid(SGD.constantStep(1.0), SGD.linearGradient)

  property("unit step can be undone by adding gradient") {
    forAll {
      (w: SGDWeights, pos: SGDPos[(Double, IndexedSeq[Double])]) =>
        //val next = oneStepMonoid.newWeights(w, pos.pos.head)
        val next = oneStepMonoid.plus(w, pos)
        next.weights == minus(w.weights, SGD.linearGradient(w.weights, pos.pos.head))
    }
  }

  property("test") {
    val gradient: (IndexedSeq[Double], (Double, IndexedSeq[Double])) => IndexedSeq[Double] = { (w, pos) =>
      val (y, xs) = pos
      val xsPlusConst = xs :+ 1.0
      val err = dot(w, xsPlusConst) - y
      // Here is the gradient
      xsPlusConst.map { _ * err * 2 }
    }

    def newWeights(sgdW: SGDWeights, p: (Double, IndexedSeq[Double])): SGDWeights = {
      val grad = gradient(sgdW.weights, p)
      //println(grad)
      val step = 0.0001
      SGDWeights(sgdW.count + 1L,
        sgdW.weights.view
          .zip(grad)
          .map { case (l: Double, r: Double) => l - step * r }
          .toIndexedSeq)
    }

    val bufferedSource = scala.io.Source.fromFile("src/main/resources/data.csv")

    var pos: SGDPos[(Double, IndexedSeq[Double])] = SGDPos(List())
    for (line <- bufferedSource.getLines) {
      val Array(x, y) = line.split(",").map(_.trim.toDouble)
      pos = zeroStepMonoid.plus(pos, SGDPos((y, IndexedSeq(x)))).asInstanceOf[SGDPos[(Double, IndexedSeq[Double])]]
    }

    var w = IndexedSeq(0.0, 0.0)
    printError(w, pos)

    for (p <- pos.pos) {
      w = newWeights(SGDWeights(w), p).weights
    }

//    for (p <- pos.pos) {
//      w = pointOneStepMonoid.plus(SGDWeights(w), SGDPos(p)).weights
//    }

//    for (i <- Range(0, 10)) {
//      w = newWeights(SGDWeights(w), pos.pos(0)).weights
//    }
    printError(w, pos)
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