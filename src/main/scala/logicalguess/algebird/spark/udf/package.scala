package logicalguess.algebird.spark

package object udf {

  import com.twitter.algebird.{HyperLogLog, HyperLogLogMonoid}

  def hllCardinality(hll: Array[Byte]): Long = {
    HyperLogLog.fromBytes(hll).approximateSize.estimate
  }

  // See algebird-core/src/main/scala/com/twitter/algebird/HyperLogLog.scala#L194-L210
  // E.g. 12 bits corresponds to an error of 0.0163
  def hllCreate(x: String, bits: Int): Array[Byte] = {
    val monoid = new HyperLogLogMonoid(bits)
    HyperLogLog.toBytes(monoid.toHLL(x))
  }

}
