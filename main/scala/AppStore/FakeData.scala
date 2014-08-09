package AppStore

import scala.util.Random
import org.apache.spark.mllib.linalg.Vector
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.mllib._
import java.lang.Math._
import MLModels.KMeansClust
import MLModels.XMeans

case class FakeData(sc: SparkContext = new SparkContext(new SparkConf().setMaster("local").setAppName("AppStore Spark App")),
                     k:Int,
                     m:Int,
                     n:Int
                   ) extends java.io.Serializable {


  val r = new Random()

  val fakeCentroids: IndexedSeq[Array[Double]] = Range(0,k).map( id => Range(id*100, id*100+m).map(_.toDouble).toArray )

  def generatePointFromCentroid(centroid: Array[Double]): Vector = linalg.Vectors.dense(centroid.map( v => v + r.nextGaussian()))

  val data: IndexedSeq[Vector] = fakeCentroids.map( centroid => Range(0,n).map( i => generatePointFromCentroid(centroid) ).toList).flatMap(identity)

  val apps: IndexedSeq[App] = data.map( point => App(r.nextInt(),"","") )

  def km(kClusters:Int): KMeansClust = KMeansClust(sc.makeRDD(data), sc.makeRDD(apps), 50, kClusters, runs=1)

  def km(): KMeansClust = KMeansClust(sc.makeRDD(data), sc.makeRDD(apps), 50, k, runs = 1)

  def xm(kMin:Int, kMax:Int): XMeans = XMeans(sc.makeRDD(data), sc.makeRDD(apps), 20, 20, kMin, kMax, runs2Means = 1, runsGlobalKMeans = 1)



  def BIC(km: KMeansClust): Double = {

    val K: Double = km.clustersSize.size.toDouble
    val R: Double = km.data.count().toDouble
    val M: Double = km.data.first().size.toDouble
    val totalDistanceBetweenPointsAndAssignedClusters: Double = km.WSSSE.toDouble
    val sigmaSquare: Double =  1/(R-K) * totalDistanceBetweenPointsAndAssignedClusters
    val degreesOfFreedom: Double = K*(M+1)
    val logLike: Double = km.clustersSize.foldLeft[Double](0.0)((tmp, clusterSize) => tmp - clusterSize/2*log(2*PI) - clusterSize*M/2*log(sigmaSquare) - (clusterSize-K)/2 + clusterSize*log(clusterSize) - clusterSize*log(R) )

    logLike  - degreesOfFreedom/2*log(R)

  }


}
