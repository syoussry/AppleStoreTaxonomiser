package AppStore

import org.apache.spark.{SparkContext, SparkConf}
import java.io.{File, PrintWriter}

object Testing {

  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("AppStore Spark App")
  val sc: SparkContext = new SparkContext(conf)

  //val kList = List(2, 3, 5)
  val kList = List(2)

  //val mList = List(2, 3, 10, 100)
  val mList = List(100)

  //val nList = Range(10000,50000, 10000).toList

  val nList = List(30000, 40000, 50000)

  def fakeDataList: List[Map[String, Int]] = {
    val writer = new PrintWriter(new File("output.txt"))
    val lMap = kList.map(
      k => mList.map(
        m =>
          nList.map(
            n => {
              val fd: FakeData = new FakeData(sc, k, m, n)
              val kf: Int = fd.xm(1,15).launch()
              val out: String = "k:" + k + " m:" + m + " n: " + n + " kf:" + kf
              writer.write(out)
              Map("k" -> k, "m" -> m, "n"-> n, "kf"->kf)
            }
          )
      ).flatten
    ).flatten

    writer.close()

    lMap

  }


}
