package logicalguess.algebird.component

import com.twitter.algebird.Monoid
import thinkbayes.Pmf

abstract class StateMonoid[I, S] extends Monoid[Component[I, S]] {

  def logic: PartialFunction[(I, S), S]

  val zero = ComponentZero

  def plus(left: Component[I, S], right: Component[I, S]): Component[I, S] = {
    (left, right) match {
      case (_, ComponentZero) => left
      case (ComponentInput(ls), ComponentInput(rs)) => ComponentInput(ls ::: rs)
      case (ComponentState(state), ComponentInput(inputs)) => ComponentState(inputs.foldLeft(state) { (current, input) =>
        logic(input, current)
      })
      case _ => right
    }
  }
}


sealed abstract class Component[+I, +S]
case object ComponentZero extends Component[Nothing, Nothing]
case class ComponentInput[I](val inputs: List[I]) extends Component[I, Nothing]
case class ComponentState[S](val state: S) extends Component[Nothing, S]

object Component {
  implicit def ComponentToComponentState[I, S](c: Component[I, S]) = c.asInstanceOf[ComponentState[S]]
  implicit def ComponentToComponentInput[I](c: Component[I, _]) = c.asInstanceOf[ComponentInput[I]]

  //implicit def ComponentInputToComponentState[I, S](input: ComponentInput[I]) = input.asInstanceOf[ComponentState[S]]
  //implicit def ComponentStateToComponent[I, S](cs: ComponentState[S]) = cs.asInstanceOf[Component[I, S]]


  implicit def StateToComponentState[S](s: S) = ComponentState(s)
  implicit def InputToComponentInput[I](input: I) = ComponentInput[I](List(input))

  implicit def PartialFunctionToStateMonoid[I, S](pf: PartialFunction[(I, S), S]) = new StateMonoid[I, S] {
    override def logic: PartialFunction[(I, S), S] = pf
  }


}
