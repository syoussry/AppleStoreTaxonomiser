package AppStore

import org.apache.spark.SparkContext
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import MLModels.{MCL, XMeans, KMeansClust}

case class AppCat(apps: Array[App], id: Double, name:String, nFeaturesUnigrams: Int, nFeaturesBigrams: Int, nFeaturesTrigrams: Int, sc:SparkContext) extends java.io.Serializable with WordsProcessor {

  // Extract features
  lazy val featuresUnigrams: Array[(String, Int)] = mapOccurrences[String]("unigrams", name)(apps.toList.flatMap(_.flatWords)).toArray.take(nFeaturesUnigrams)
  lazy val featuresBigrams: Array[(String, Int)] = mapOccurrences[String]("bigrams", name)(apps.toList.flatMap(_.flatBigrams)).toArray.take(nFeaturesBigrams)
  lazy val featuresTrigrams: Array[(String, Int)] = mapOccurrences[String]("trigrams", name)(apps.toList.flatMap(_.flatTrigrams)).toArray.take(nFeaturesTrigrams)

  lazy val featuresOnlyUnigrams: Array[String] = featuresUnigrams.map(_._1)
  lazy val featuresOnlyBigrams: Array[String] = featuresBigrams.map(_._1)
  lazy val featuresOnlyTrigrams: Array[String] = featuresTrigrams.map(_._1)

  // method to print these features to files
  def printFeatures(): Unit = {
    writeToFile(featuresUnigrams, "crawlData/" + name + "/" + name + "-unigrams.csv")
    writeToFile(featuresBigrams, "crawlData/" + name + "/" + name + "-bigrams.csv")
    writeToFile(featuresTrigrams, "crawlData/" + name + "/" + name + "-trigrams.csv")
  }


  // MACHINE LEARNING

  // 1: methods to transform our features into MLLIB formats
  def appsFeatures(featUni: Array[String], featBi: Array[String], featTri: Array[String]): Array[LabeledPoint] = apps.map( app => LabeledPoint( id, Vectors.sparse(featUni.length + featBi.length + featTri.length, denseToSparse(app.features(featUni, featBi, featTri) ) ) ) )
  def appsFeaturesNoLabel(featUni: Array[String], featBi: Array[String], featTri: Array[String]): Array[Vector] = apps.map( app => Vectors.sparse(featUni.length + featBi.length + featTri.length, denseToSparse(app.features(featUni, featBi, featTri) ) ) )

  // ml methods, only unsupervised
  def kmeans(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterations: Int, k:Int, runs:Int): KMeansClust = KMeansClust(sc.makeRDD(appsFeaturesNoLabel(featUni, featBi, featTri)), sc.makeRDD(apps) , numIterations, k, runs)
  def xmeans(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterationsGlobal: Int, numIterations2:Int, kInit:Int, kMax:Int, runsGlobal:Int, runs2:Int): XMeans = XMeans(sc.makeRDD(appsFeaturesNoLabel(featUni, featBi, featTri)), sc.makeRDD(apps) , numIterationsGlobal, numIterations2, kInit, kMax, runsGlobal, runs2)
  def mcl(featUni: Array[String], featBi: Array[String], featTri: Array[String], r:Int): MCL = MCL(sc.makeRDD(appsFeaturesNoLabel(featUni, featBi, featTri)), r, 10)

}
