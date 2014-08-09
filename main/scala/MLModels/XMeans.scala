package MLModels

import org.apache.spark.rdd.RDD
import Math.{log,PI}
import AppStore._
import org.apache.spark.mllib.linalg


case class XMeans(data:RDD[linalg.Vector], apps: RDD[App], numIterationsForGlobalKMeans:Int, numIterationsFor2Means: Int, kInit:Int, kMax:Int, runsGlobalKMeans: Int, runs2Means:Int) extends MLModel {

  def improveParams(k:Int): KMeansClust = KMeansClust(data, apps, numIterationsForGlobalKMeans, k, runsGlobalKMeans)

  // returns an array of boolean
  // for each cluster, returns a local 2-Means iteration
  def improveStructure(km: KMeansClust): Array[(Double, Double)] = {
    (km.clusters.clusterCenters zip Range(0,km.clusters.k).toList).map{
      case (clusterCenter: linalg.Vector, clusterID:Int) =>
        val clusterAppsData: RDD[((App, Int), linalg.Vector)] = (km.assignedClusters zip data).filter(_._1._2==clusterID)
        val clusterData: RDD[linalg.Vector] = clusterAppsData.map(_._2)
        val clusterApps: RDD[App] = clusterAppsData.map(_._1._1)
        val local2Means: KMeansClust = KMeansClust(clusterData,clusterApps,numIterationsFor2Means,k = 2, runs2Means)
        val local1Mean:KMeansClust = KMeansClust(clusterData,clusterApps,numIterationsFor2Means,k = 1, runs=1)
        val BICTwoClusters = BIC(local2Means)
        val BICOneCluster = BIC(local1Mean)
        (BICOneCluster, BICTwoClusters)
    }
  }

  def findNewK(improvedStructure:Array[(Double, Double)]): Int =  {
    val oldK = improvedStructure.length
    println("old k: " + oldK)
    val newK = improvedStructure.foldLeft[Int](oldK){ (tmp, bics) => println(bics);if(bics._1 < bics._2) tmp + 1 else tmp }
    println("new k: " + newK)
    newK
  }

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


  def loop(tmpK:Int):Int = {

    val newK: Int = findNewK(improveStructure(improveParams(tmpK)))
    if(newK == tmpK | newK > kMax) tmpK
    else loop(newK)

  }

  def launch():Int = loop(kInit)

}
