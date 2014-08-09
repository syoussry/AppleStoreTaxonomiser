package AppStore

import org.apache.spark.{SparkContext, SparkConf}
import org.apache.spark.rdd.RDD
import MLModels._
import org.apache.spark.mllib.regression.LabeledPoint

case class AppStore(categoriesIDsAndNames: List[(Double, String)], nFeaturesUnigrams:Int, nFeaturesBigrams:Int, nFeaturesTrigrams:Int) extends java.io.Serializable with WordsProcessor {

  //val allCategories: List[(String, Double)] = List((0.0,Books), (1.0,"Business"), (2.0,"Catalogs"), (3.0,"Education"), (4.0,"Entertainment"), (5.0,"Finance"), (6.0,"Food_Drink"), (7.0,"Games"), (8.0,"Health_Fitness"), (9.0,"Lifestyle"), (10.0,"Medical"), (11.0,"Music"), (12.0,"Navigation"), (13.0,"News"), (14.0,"Newsstand"), (15.0,"Photo_Video"), (16.0,"Productivity"), (17.0,"Reference"), (18.0,"Social_Networking"), (19.0,"Sports"), (20.0,"Travel"), (21.0,"Utilities"), (22.0,"Weather"))

  assert(categoriesIDsAndNames.sortBy(_._1) == categoriesIDsAndNames, "Categories IDs must be sorted in increasing order")
  assert(categoriesIDsAndNames.map(_._1) == Range(0,categoriesIDsAndNames.length).map(_.toDouble).toList, "Categories IDs must start with 0.0 and be incremented by 1")

  // 1st step: initialize a Spark Context to work with
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("AppStore Spark App")
  val sc: SparkContext = new SparkContext(conf)

  // 2nd step: get DATA from each category
  val categories: List[AppCat] = categoriesIDsAndNames.map{ case (categoryID, categoryName) => AppCat(fileToAppsMap("crawlData/" + categoryName + "/megafiles/" + categoryName + "Data-en.csv") ,categoryID, categoryName, nFeaturesUnigrams, nFeaturesBigrams, nFeaturesTrigrams, sc) }

  // 3rd step: group global features
  lazy val globalFeaturesOnlyUnigrams: Array[String] = categories.flatMap(_.featuresOnlyUnigrams).toArray
  lazy val globalFeaturesOnlyBigrams: Array[String] = categories.flatMap(_.featuresOnlyBigrams).toArray
  lazy val globalFeaturesOnlyTrigrams: Array[String] = categories.flatMap(_.featuresOnlyTrigrams).toArray


  // Machine Learning

  // use only unigrams for classification

  def splitToTrainAndTest(parsedData:RDD[LabeledPoint]): (RDD[LabeledPoint], RDD[LabeledPoint]) = {
    // Split data into training (60%) and test (40%).
    val splits = parsedData.randomSplit(Array(0.6, 0.4), seed = 1)
    (splits(0),splits(1))
  }

  // ML Algorithms: only supervised for AppStore

  def nb(featUni: Array[String], featBi: Array[String], featTri: Array[String]): NB = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    NB(categoriesIDsAndNames.map(_._1), training, test)
  }

  def nbLaplace(featUni: Array[String], featBi: Array[String], featTri: Array[String]): NBLaplace = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    val categoriesSizes: Map[Double, Int] = (categoriesIDsAndNames.map(_._1) zip categories.map(_.apps.size)).toMap
    NBLaplace(categoriesSizes, training, test, nFeaturesUnigrams, nFeaturesBigrams)
  }

  def svm(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterations: Int): SVM = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    SVM(training, test, numIterations)
  }

  def mSvm(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterations: Int): SVMMultiClass = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    SVMMultiClass(categoriesIDsAndNames.map(_._1), training, test, numIterations)
  }

  def svmOne(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterations: Int):SVMMultiClassOneVsOne = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    SVMMultiClassOneVsOne(categoriesIDsAndNames.map(_._1), training, test, numIterations)
  }

  def logreg(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterations: Int): LogReg = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    LogReg(training, test, numIterations)
  }


  def mLogreg(featUni: Array[String], featBi: Array[String], featTri: Array[String], numIterations: Int): LogRegMultiClass = {
    val (training: RDD[LabeledPoint], test: RDD[LabeledPoint]) = splitToTrainAndTest(parseData(featUni, featBi, featTri))
    LogRegMultiClass(categoriesIDsAndNames.map(_._1), training, test, numIterations)
  }

  // Helper functions

  // parseData takes features as input, and outputs each app features in a giant RDD
  def parseData(featUni: Array[String], featBi: Array[String], featTri: Array[String]): RDD[LabeledPoint] = sc.makeRDD(categories.map(_.appsFeatures(featUni, featBi, featTri)).flatten)


  // OTHERS

  // EXPLORATORY DATA ANALYSIS
  def numberOfAppsPerCategories(): Map[String, Int] = categories.map( cat => (cat.name, cat.apps.size) ).toMap

}