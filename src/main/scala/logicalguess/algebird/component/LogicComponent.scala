package logicalguess.algebird.component

/**
  * Created by logicalguess on 7/31/17.
  */

import akka.stream.scaladsl.{Flow, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.twitter.algebird.{Aggregator, Monoid, Semigroup}

trait Interactor[I, O, -S] {
  def apply(implicit init: S): Flow[I, O, _]

}
case class PFInteractor[I, O, S](logic: PartialFunction[(I, S), S], transformer: Function[S, O])
                              (implicit mat: ActorMaterializer) extends Interactor[I, O, S] {

  def apply(implicit init: S): Flow[I, O, _] = {
    val f = Flow[I].scan(init)((state: S, event: I) => logic(event, state))
    val t = Flow[S].map(transformer)
    f.via(t)
  }
}

case class MonoidInteractor[I, O, S](prepare: Function[I, S], logic: Monoid[S], transformer: Function[S, O])
                                (implicit mat: ActorMaterializer) extends Interactor[I, O, S] {

  def apply(implicit init: S): Flow[I, O, _] = {
    val f = Flow[I].scan(init)((state: S, event: I) => logic.plus(state, prepare(event)))
    val t = Flow[S].map(transformer)
    f.via(t)
  }
}

case class AggregatorInteractor[I, O, S](aggregator: Aggregator[I, S, O])
                                    (implicit mat: ActorMaterializer) extends Interactor[I, O, S] {
  def apply(implicit init: S): Flow[I, O, _] = {
    val f = Flow[I].scan(init)((state: S, event: I) => aggregator.semigroup.plus(state, aggregator.prepare(event)))
    val t = Flow[S].map(aggregator.present _)
    f.via(t)
  }
}

case class LogicComponent[I, O, S](init: S, interactor: Interactor[I, O, S], sink: Sink[O, _])
                                  (implicit mat: ActorMaterializer) {

  private lazy val source: Source[I, SourceQueueWithComplete[I]] =
    Source.queue[I](10, OverflowStrategy.backpressure)

  implicit def interactorToFlow[I, O](i: Interactor[I, O, S]): Flow[I, O, _] = i(init)

  private lazy val computation: SourceQueueWithComplete[I] = source.via(interactor).to(sink).run()

  def receive(event: I): Unit = {
    computation.offer(event)
  }
}
