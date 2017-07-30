package logicalguess.algebird.spark.state

import akka.actor.ActorSystem
import com.twitter.algebird.HyperLogLog._
import com.twitter.algebird.{HLL, HyperLogLogMonoid}
import logicalguess.algebird.spark.StreamingHLL.Source
import org.apache.log4j.{Level, Logger}
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.streaming._

object StateHLL {
  def main(args: Array[String]) {

    val conf = new SparkConf(false)
      .setMaster("local[*]")
      .setAppName("StateHLL")

    val session = SparkSession.builder().config(conf).getOrCreate()

    def createStreamingContext(): StreamingContext = {
      @transient val newSsc = new StreamingContext(session.sparkContext, Seconds(2))
      newSsc.remember(Minutes(10))
      newSsc.checkpoint(s"""checkpoint""")
      newSsc
    }

    val ssc = StreamingContext.getActiveOrCreate(createStreamingContext)


    val itemsDF = session.sqlContext.read.format("json")
      .load(s"""src/main/resources/actors.json""")

    val ratingsStream = ssc.receiverStream(Source)

    // Setup the Algebird HyperLogLog data struct using 14 bits
    // Note:  this is the same as the Redis implementation
    //        2^14 = 16,384 registers, 0.81% standard error
    val hllMonoid = new HyperLogLogMonoid(14)

    // Merge the current HLL for a given itemId with the new HLLs for the itemId
    def updateStateFunction(newHlls: Seq[HLL], currentHll: Option[HLL]) = {
      val sumHll = hllMonoid.sum(currentHll.getOrElse(hllMonoid.zero) +: newHlls)
      Some(sumHll)
    }

    // Create (key, value) pairs which is what updateStateByKey expects
    val itemIdHllStream = ratingsStream.map(message => {
      val tokens = message.split(",")
      val userId = tokens(0).trim.toInt
      val itemId = tokens(1).trim.toInt
      val userIdHll = hllMonoid.create(userId)
      (itemId, userIdHll)
    })

    // Update the state
    // Spark Streamings internals will organize all HLLs (values) for a given itemId (key)
    //   and pass to the updateStateFunction() method
    val sumItemIdHllStream = itemIdHllStream.updateStateByKey[HLL](updateStateFunction _)

    // Format for printing by pulling out the estimatedSize from the HLL
    val sumItemIdApproxDistinctCountStream = sumItemIdHllStream.map(itemIdHll => (itemIdHll._1, itemIdHll._2.estimatedSize.toLong))

    val schema = StructType(StructField("itemId", IntegerType, true) :: StructField("approxDistinctCount", LongType, true) :: Nil)

    val sumItemIdApproxDistrinctCountRowStream = sumItemIdApproxDistinctCountStream.map(rdd => (rdd._1, rdd._2))

    sumItemIdApproxDistrinctCountRowStream.foreachRDD(rdd => {
      import session.sqlContext.implicits._
      val sumItemIdApproxDistinctCountRowsDF = rdd.toDF("itemId", "approxDistinctCount")

      val enrichedDF =
        sumItemIdApproxDistinctCountRowsDF.join(itemsDF, $"itemId" === $"id")
          .select($"itemId", $"title", $"approxDistinctCount")
          .sort($"approxDistinctCount" desc)
          .limit(5)

      val enriched = enrichedDF.collect()

      println(s"""Approx Distinct Count HLL: ${enriched.mkString("[", ",", "]")}""")
    })

    val rootLogger = Logger.getRootLogger()
    rootLogger.setLevel(Level.ERROR)

    ssc.checkpoint("checkpoint")
    ssc.start()

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val actorSystem = ActorSystem()

    val cancellable =
      actorSystem.scheduler.schedule(0 milliseconds, 100 milliseconds) {
        Source.store("1, 90001")
        Source.store("2, 90002")
        Source.store("3, 90001")

      }

    actorSystem.scheduler.scheduleOnce(10 seconds) {
      cancellable.cancel()
      ssc.stop()
      System.exit(0)
    }
  }
}