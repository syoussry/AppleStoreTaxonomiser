package MLModels

import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.rdd.RDD
import scala.math.{log, exp}
import org.apache.spark.mllib.linalg

case class NBLaplace(categoriesSizes:Map[Double, Int], training:RDD[LabeledPoint], test:RDD[LabeledPoint], nUnigrams: Int, nBigrams:Int, lambda: Double = 1.0) extends MLModel {

  val train: Map[Double, Array[Double]] = {

    val featuresCount: Map[Double, (Array[Double], Array[Double])] = training.
        groupBy(_.label).
        collect().
        map{ case (id, mixedFeatures) =>
            val (unigramsFeatures, bigramsFeatures) = mixedFeatures.splitAt(nUnigrams)
            (id, (unigramsFeatures.map(_.features.toArray).reduce(sumArrays), bigramsFeatures.map(_.features.toArray).reduce(sumArrays) ) )
        }.toMap

    val nbWords: Map[Double, (Double, Double)] = featuresCount.
      map{ case (id, (unigramsFeaturesCount, bigramsFeaturesCount)) => (id, (unigramsFeaturesCount.reduce(_ + _), bigramsFeaturesCount.reduce(_ + _))) }

    featuresCount.map{ case (id, (fixedCategoryUnigramsCount, fixedCategoryBigramsCount)) =>
      val f1: Array[Double] = fixedCategoryUnigramsCount.map(value => log(value + lambda) - log(nbWords(id)._1 + nUnigrams))
      val f2: Array[Double] = fixedCategoryBigramsCount.map(value => log(value + lambda) - log(nbWords(id)._2 + nBigrams))
      (id, f1 union f2) }
  }

  val globalNumberOfApps: Double = categoriesSizes.values.reduce(_ + _).toDouble

  def predict(point: linalg.Vector): Double = train.
    map{
      case (categoryId, probas) =>
        (
          categoryId,
          (probas zip point.toArray).map{case (p, pointFeature) => if(pointFeature > 0) p else log(1 - exp(p))}.reduce(_ + _) +
          log(categoriesSizes(categoryId) + globalNumberOfApps)
        )
    }.maxBy(_._2)._1

  def predict(data:RDD[linalg.Vector]): RDD[Double] = data.map(predict)

  val predictionAndLabel: RDD[(Double, Double)] = predict(test.map(_.features)) zip test.map(_.label)

  val confusionMatrix: Map[(Double, Double), Int] = predictionAndLabel.groupBy(identity).map( x=> (x._1,x._2.size)).collect().toMap

  val accuracy: Double = confusionMatrix.filterKeys(key => key._1 == key._2).values.reduce(_ + _).toDouble / confusionMatrix.values.reduce(_ + _).toDouble * 100

  // helper functions

  def sumArrays(vec1:Array[Double], vec2:Array[Double]) = (vec1 zip vec2).map( c => c._1 + c._2)
  def sumLabeledPoints(l1: LabeledPoint, l2: LabeledPoint): Array[Double] = sumArrays(l1.features.toArray, l2.features.toArray)

}