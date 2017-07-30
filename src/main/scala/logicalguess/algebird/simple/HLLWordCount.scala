package logicalguess.algebird.simple

import scala.io.Source

object HLLWordCount {
  import com.twitter.algebird.HyperLogLogMonoid

  def main(args: Array[String]) {

    val alice: Stream[String] = Source.fromFile("src/main/resources/alice.txt").getLines.toStream
    val aliceWords: Stream[String] = alice.flatMap(_.toLowerCase.split("\\s+"))

    println("exact count: " + aliceWords.distinct.length)

    // create algebird HLL
    val hll = new HyperLogLogMonoid(bits = 22)
    // convert data elements to a seq of hlls
    val hlls = aliceWords.map { str =>
      val bytes = str.getBytes("utf-8")
      hll.create(bytes)
    }

    // merge seq of hlls in one hll object
    val merged = hll.sum(hlls)

    println("estimate count: " + merged.approximateSize.estimate)

  }
}