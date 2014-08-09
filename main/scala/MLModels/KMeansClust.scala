package MLModels

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering.{KMeansModel, KMeans}
import AppStore._
import java.io.{File, PrintWriter}
import org.apache.spark.mllib.linalg

case class KMeansClust(data:RDD[linalg.Vector], apps: RDD[App] ,numIterations: Int, k:Int, runs:Int) extends MLModel {


  // Cluster the data into two classes using KMeans
  val clusters: KMeansModel = KMeans.train(data, k, numIterations, runs)

  val distanceToClusters: RDD[Array[Double]] = {

    data.map{
      (point: linalg.Vector) => {
        clusters.clusterCenters.map( (clusterCenter: linalg.Vector) => sqDistance(point, clusterCenter) )
      }
    }

  }

  val distanceToClosestClusters: RDD[Double] = distanceToClusters.map(_.min)

  val assignedClusters: RDD[(App, Int)] = apps zip distanceToClusters.map( distances => distances.indexOf(distances.min) )

  val clustersSize:Array[Long] = Range(0,clusters.clusterCenters.size).toArray.map( clusterID => assignedClusters.filter(_._2==clusterID).count() )

  val subCategories: RDD[AppSubCategory] = assignedClusters.groupBy(_._2).map{ case (id, setOfAppAndId) => AppSubCategory(id, setOfAppAndId.map(_._1).toList) }


  // Evaluate clustering by computing Within Set Sum of Squared Errors
  val WSSSE: Double = clusters.computeCost(data)

  def printClustersToFile() = {

    val writer = new PrintWriter(new File("clusters" + k + "-" + numIterations + ".txt"))
    assignedClusters.collect().foreach( x => writer.write(x._1.name + "!#!" + x._2 + System.getProperty("line.separator")) )

  }


}