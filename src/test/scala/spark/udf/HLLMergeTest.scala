package spark.udf

import logicalguess.algebird.spark.udf._

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.{FlatSpec, Matchers}

class HLLMergeTest extends FlatSpec with Matchers{
 "Algebird's HyperLogLog" can "be used from Spark" in {
  val sparkConf = new SparkConf().setAppName("HyperLogLog")
  sparkConf.setMaster(sparkConf.get("spark.master", "local[1]"))

  val sc = new SparkContext(sparkConf)
  val sqlContext = SparkSession.builder().getOrCreate().sqlContext
  import sqlContext.implicits._

  val hllMerge = new HLLMerge
  sqlContext.udf.register("hll_merge", hllMerge)
  sqlContext.udf.register("hll_create", hllCreate _)
  sqlContext.udf.register("hll_cardinality", hllCardinality _)

  val frame = sc.parallelize(List("a", "b", "c", "c"), 4).toDF("id")
  val count = frame
    .select(expr("hll_create(id, 12) as hll"))
    .groupBy()
    .agg(expr("hll_cardinality(hll_merge(hll)) as count"))
    .collect()
  count(0)(0) should be (3)
 }
}
