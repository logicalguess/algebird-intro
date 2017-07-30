package logicalguess.algebird.spark

import java.util.Random

import akka.actor.ActorSystem
import com.twitter.algebird._
import org.apache.log4j.{Level, Logger}
import org.apache.spark.SparkConf
import org.apache.spark.api.java.StorageLevels
import org.apache.spark.streaming.receiver.Receiver
import org.apache.spark.streaming.{Seconds, StreamingContext}

/**
 * Example of using CMS monoid from Twitter's Algebird together with Spark Streaming
 */
object StreamingCMS {
  def main(args: Array[String]) {


    val conf = new SparkConf(false)
      .setMaster("local[*]")
      .setAppName("StreamingHLL")

    val ssc = new StreamingContext(conf, Seconds(1))
    val stream = ssc.receiverStream(Source)

    val users = stream.map(id => id)

    // CMS parameters
    val DELTA = 1E-3
    val EPS = 0.01
    val SEED = 1
    val PERC = 0.001
    // K highest frequency elements to take
    val TOPK = 10

    val cms = TopPctCMS.monoid[Long](EPS, DELTA, SEED, PERC)
    var globalCMS = cms.zero
    val mm = new MapMonoid[Long, Int]()
    var globalExact = Map[Long, Int]()

    val approxTopUsers = users.mapPartitions(ids => {
      ids.map(id => cms.create(id))
    }).reduce(_ ++ _)

    val exactTopUsers = users.map(id => (id, 1))
      .reduceByKey((a, b) => a + b)

    approxTopUsers.foreachRDD(rdd => {
      if (rdd.count() != 0) {
        val partial = rdd.first()
        val partialTopK = partial.heavyHitters.map(id =>
          (id, partial.frequency(id).estimate)).toSeq.sortBy(_._2).reverse.slice(0, TOPK)
        globalCMS ++= partial
        val globalTopK = globalCMS.heavyHitters.map(id =>
          (id, globalCMS.frequency(id).estimate)).toSeq.sortBy(_._2).reverse.slice(0, TOPK)
        println("Approx heavy hitters at %2.2f%% threshold this batch: %s".format(PERC,
          partialTopK.mkString("[", ",", "]")))
        println("Approx heavy hitters at %2.2f%% threshold overall: %s".format(PERC,
          globalTopK.mkString("[", ",", "]")))
      }
    })

    exactTopUsers.foreachRDD(rdd => {
      if (rdd.count() != 0) {
        val partialMap = rdd.collect().toMap
        val partialTopK = rdd.map(
          {case (id, count) => (count, id)})
          .sortByKey(ascending = false).take(TOPK)
        globalExact = mm.plus(globalExact.toMap, partialMap)
        val globalTopK = globalExact.toSeq.sortBy(_._2).reverse.slice(0, TOPK)
        println("Exact heavy hitters this batch: %s".format(partialTopK.mkString("[", ",", "]")))
        println("Exact heavy hitters overall: %s".format(globalTopK.mkString("[", ",", "]")))
      }
    })

    val rootLogger = Logger.getRootLogger()
    rootLogger.setLevel(Level.ERROR)

    ssc.checkpoint("checkpoint")
    ssc.start()

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val actorSystem = ActorSystem()

    val rnd = new Random()
    def rndInRange(rnd: Random, from: Int, to: Int): Long = {
      rnd.nextInt(to - from) + from
    }
    val cancellable =
      actorSystem.scheduler.schedule(0 milliseconds, 100 milliseconds) {
        Source.store(rndInRange(rnd, 1, 10))
      }

    actorSystem.scheduler.scheduleOnce(20 seconds) {
      cancellable.cancel()
      ssc.stop()
      System.exit(0)
    }
  }

  object Source extends Receiver[Long](StorageLevels.MEMORY_AND_DISK) {

    override def onStart() = {
    }

    override def onStop(): Unit = {
    }
  }
}