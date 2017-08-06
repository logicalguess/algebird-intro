package logicalguess.algebird.sgd

import logicalguess.algebird.component.StateMonoid

case class SGDStateMonoid(step: Double, gradient: (IndexedSeq[Double], (Double, IndexedSeq[Double])) => IndexedSeq[Double] = SGDStateMonoid.linearGradient)
  extends StateMonoid[(Double, IndexedSeq[Double]), IndexedSeq[Double]] {
  override def logic: PartialFunction[((Double, IndexedSeq[Double]), IndexedSeq[Double]), IndexedSeq[Double]] = {
    case (pos, w) =>
      val grad = gradient(w, pos)
      w.view
        .zip(grad)
        .map { case (l: Double, r: Double) => l - step * r }
        .toIndexedSeq
  }

}

object SGDStateMonoid {
  val linearGradient: (IndexedSeq[Double], (Double, IndexedSeq[Double])) => IndexedSeq[Double] = { (w, pos) =>
    val (y, xs) = pos
    val xsPlusConst = xs :+ 1.0
    val err = dot(w, xsPlusConst) - y
    // Here is the gradient
    xsPlusConst.map {
      _ * err
    }
  }

  def dot(x: IndexedSeq[Double], y: IndexedSeq[Double]): Double =
    x.view.zip(y).map { case (a: Double, b: Double) => a * b }.sum

  def printError(w: IndexedSeq[Double], pos: List[(Double, IndexedSeq[Double])]) = {
    var err = 0.0
    for (p <- pos) {
      val xsPlusConst = p._2 :+ 1.0
      err += Math.pow(dot(w, xsPlusConst) - p._1, 2)
    }
    println(s"weights = $w, error = ${err/pos.size}")
  }
}


