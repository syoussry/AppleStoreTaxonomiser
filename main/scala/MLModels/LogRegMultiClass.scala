package MLModels

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.classification.{LogisticRegressionWithSGD, LogisticRegressionModel}

case class LogRegMultiClass(categoriesIDs: List[Double], training:RDD[LabeledPoint], test:RDD[LabeledPoint], numIterations: Int) extends MLModel {

  val mTraining: List[RDD[LabeledPoint]] =
    categoriesIDs.map(
      categoryID => {
        // what corresponds to the category ID
        val thisCategory: RDD[LabeledPoint] = training.filter( point => point.label == categoryID).map( point => LabeledPoint(1.0, point.features) )
        val complementOfThisCategory: RDD[LabeledPoint] = training.filter( point => point.label == categoryID).sample(withReplacement = false, fraction = 1.0/categoriesIDs.length).map( point => LabeledPoint(0.0, point.features) )
        thisCategory union complementOfThisCategory
      }
    )

  val mTest: RDD[List[LabeledPoint]] =
    test.map{
      point => categoriesIDs.map{
        categoryID => LabeledPoint(goodLabel(point.label, categoryID), point.features)
      }
    }

  // Run training algorithm to build the model
  val mModel: List[LogisticRegressionModel] = mTraining.map( training => LogisticRegressionWithSGD.train(training, numIterations) )

  // Clear the default threshold.
  mModel.map(_.clearThreshold())

  // Compute raw scores on the test set.
  val mScoreAndLabels: RDD[List[(Double, Double)]] =
    mTest.map{
      points => {
        val scores: List[Double] = (points zip mModel).map{ case (point, model) => model.predict(point.features) }
        scores zip points.map(_.label)
      }
    }


  // Results
  val predictionAndLabel: RDD[(Double, Double)] = mScoreAndLabels.map{
    (listOfScoresAndLabels: List[(Double, Double)]) => {
      val onlyScores = listOfScoresAndLabels.map{ case (score,label) => score - 0.5 }
      //listOfScoresAndLabels.sortBy{ case (score, label) => -Math.abs(score) }.head._2
      onlyScores.indexOf(onlyScores.max).toDouble
    }
  } zip test.map(_.label)

  val accuracy: Double = 1.0 * predictionAndLabel.filter(x => x._1 == x._2).count() / test.count()


}
