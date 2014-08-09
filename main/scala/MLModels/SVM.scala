package MLModels

import org.apache.spark.mllib.classification.{SVMWithSGD, SVMModel}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics

case class SVM(training:RDD[LabeledPoint], test:RDD[LabeledPoint], numIterations: Int) extends MLModel {

  // Run training algorithm to build the model
  val model: SVMModel = SVMWithSGD.train(training, numIterations)

  // Clear the default threshold.
  model.clearThreshold()

  // Compute raw scores on the test set.
  val scoreAndLabels: RDD[(Double, Double)] = test.map { point =>
    val score: Double = model.predict(point.features)
    (score, point.label)
  }

  def accuracy(threshold:Double): Double = {
    val predictionAndLabel = scoreAndLabels.map{
      case (score,label) => if(score>=threshold) 1.0 else 0.0
    } zip test.map(_.label)

    1.0 * predictionAndLabel.filter(x => x._1 == x._2).count() / test.count()

  }

  val accuracy0 = accuracy(0.0)

  lazy val bestThresholdAndAccuracy: (Double,Double) = {
    val scores: RDD[Double] = scoreAndLabels.map(_._1)
    val thresholdRange: List[Double] = Range(scores.min.floor.toInt * 10 , scores.max.floor.toInt * 10).toList.map(_.toDouble).map(_/10)
    ( thresholdRange zip thresholdRange.map( t => accuracy(t) ) ).maxBy(_._2)
  }


  // Get evaluation metrics.
  def metrics: BinaryClassificationMetrics = new BinaryClassificationMetrics(scoreAndLabels)
  def auROC: Double = metrics.areaUnderROC()


}
