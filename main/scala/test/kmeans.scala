package test

import org.apache.spark.mllib.clustering.KMeans
import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.{Vector, Vectors}


object kmeans {

  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("x")
  val sc: SparkContext = new SparkContext(conf)

  // Load and parse the data
  val data: RDD[String] = sc.textFile("kmeans_data.txt")
  val parsedData: RDD[Vector] = data.map( _.split(' ').map(_.toDouble)).map(point => Vectors.dense(point))

  // Cluster the data into two classes using KMeans
  val numIterations = 20
  val numClusters = 2
  val clusters = KMeans.train(parsedData, numClusters, numIterations)

  // Evaluate clustering by computing Within Set Sum of Squared Errors
  val WSSSE = clusters.computeCost(parsedData)
  println("Within Set Sum of Squared Errors = " + WSSSE)

}
