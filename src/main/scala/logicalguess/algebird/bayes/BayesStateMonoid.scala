package logicalguess.algebird.bayes

import logicalguess.algebird.component.StateMonoid
import thinkbayes.Pmf

case class BayesStateMonoid[Data, Hypothesis](likelihood: (Data, Hypothesis) => Double)
  extends StateMonoid[Data, Pmf[Hypothesis]] {
  override def logic: PartialFunction[(Data, Pmf[Hypothesis]), Pmf[Hypothesis]] = {
    case (d: Data, pmf: Pmf[Hypothesis]) =>
      pmf.map { case (h, prob) => (h, prob * likelihood(d, h)) }.normalized
  }
}

