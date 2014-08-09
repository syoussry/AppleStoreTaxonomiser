package MLModels

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.classification.{NaiveBayes, NaiveBayesModel}

case class NB(categoriesIDs: List[Double], training:RDD[LabeledPoint], test:RDD[LabeledPoint]) extends MLModel {

  val model: NaiveBayesModel = NaiveBayes.train(training, lambda = 1.0)

  val prediction: RDD[Double] = model.predict(test.map(_.features))

  val predictionAndLabel: RDD[(Double, Double)] = prediction zip test.map(_.label)

  val confusionMatrix: Map[(Double, Double), Int] = predictionAndLabel.groupBy(x => x).map( x=> (x._1,x._2.size)).collect().toMap

  val indicators: Map[Double, Map[String, Double]] = categoriesIDs.map{
    categoryID =>
      val indTrue = confusionMatrix.filterKeys(_._1 == categoryID).map(_._1._2)
      val indFalse = confusionMatrix.filterKeys(_._1 != categoryID).map(_._1._2)
      val trueSize = indTrue.size
      val falseSize = indFalse.size
      val truePositive = indTrue.count(_ == categoryID)
      val falseNegative = trueSize - truePositive
      val falsePositive = indFalse.count(_ != categoryID)
      val trueNegative = falseSize - falsePositive
      (categoryID,
        Map(
          "true" -> trueSize.toDouble,
          "false" -> falseSize.toDouble,
          "truePositive" -> truePositive.toDouble,
          "trueNegative" -> trueNegative.toDouble,
          "falsePositive" -> falsePositive.toDouble,
          "falseNegative" -> falseNegative.toDouble
        )
      )
  }.toMap

  val derivations: Map[Double, Map[String, Double]] = indicators.mapValues{
    indicator =>
      Map(
        "recall" -> indicator("truePositive") / (indicator("truePositive")+indicator("falseNegative")),
        "specificity" -> indicator("trueNegative") / (indicator("trueNegative")+indicator("falsePositive")),
        "precision" -> indicator("truePositive") / (indicator("truePositive") + indicator("falsePositive")),
        "negativepredictivevalue" -> indicator("trueNegative") / (indicator("trueNegative") + indicator("falseNegative")),
        "fallout" -> indicator("falsePositive") / (indicator("falsePositive") + indicator("trueNegative")),
        "falsediscoveryrate" -> indicator("falsePositive") / (indicator("falsePositive") + indicator("truePositive")),
        "fscore" -> 2*indicator("truePositive") / (2 * indicator("truePositive") + indicator("falsePositive") + indicator("falseNegative")),
        "accuracy" -> (indicator("truePositive") + indicator("trueNegative")) / (indicator("truePositive") + indicator("trueNegative") + indicator("falsePositive") + indicator("falseNegative")),
        "matthews" -> ( indicator("truePositive") * indicator("trueNegative") - indicator("falsePositive")*indicator("falseNegative") ) / Math.sqrt( (indicator("truePositive") + indicator("falsePositive")) * (indicator("truePositive") + indicator("falseNegative")) * (indicator("trueNegative") + indicator("falsePositive")) * (indicator("trueNegative") + indicator("falseNegative")) )
      )
  }

}