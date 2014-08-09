package test

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.mllib.classification.{SVMModel, SVMWithSGD}
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.rdd.RDD


object svm {

  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("x")
  val sc: SparkContext = new SparkContext(conf)

  // Load training data in LIBSVM format.
  val data: RDD[LabeledPoint] = MLUtils.loadLibSVMFile(sc, "sample_libsvm_data.txt")

  // Split data into training (60%) and test (40%).
  val splits: Array[RDD[LabeledPoint]] = data.randomSplit(Array(0.6, 0.4), seed = 11L)
  val training: RDD[LabeledPoint] = splits(0).cache()
  val test: RDD[LabeledPoint] = splits(1)

  // Run training algorithm to build the model
  val numIterations: Int = 100
  val model: SVMModel = SVMWithSGD.train(training, numIterations)

  // Clear the default threshold.
  model.clearThreshold()

  // Compute raw scores on the test set.
  val scoreAndLabels = test.map { point =>
    val score = model.predict(point.features)
    (score, point.label)
  }
  // Get evaluation metrics.
  lazy val metrics = new BinaryClassificationMetrics(scoreAndLabels)
  lazy val auROC = metrics.areaUnderROC()

}
