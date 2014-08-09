package MLModels

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.classification.{SVMWithSGD, SVMModel}
import org.apache.spark.mllib.linalg.Vector

case class SVMMultiClassOneVsOne(categoriesIDs: List[Double], training:RDD[LabeledPoint], test:RDD[LabeledPoint], numIterations: Int) extends MLModel {


  val mTraining: List[((Double, Double), RDD[LabeledPoint])] =
    categoriesIDs.map{
      category1ID => {
        categoriesIDs.dropWhile( _<=category1ID ).map{
          category2ID => {
            val cat1 = training.filter( point => point.label == category1ID).map( point => LabeledPoint(1.0, point.features) ) // score >= 0.0 for this one
            val cat2 = training.filter( point => point.label == category2ID).map( point => LabeledPoint(0.0, point.features) )
            ((category1ID,category2ID), cat1 union cat2)
          }
        }
      }
    }.flatten

    // Run training algorithm to build the model
    val mModel: List[((Double,Double), SVMModel)] = mTraining.map{ case (key, smallTraining) => (key,SVMWithSGD.train(smallTraining, numIterations).clearThreshold()) }

/*
    // confusion matrix for multiclass SVM one versus one
    val confusionMatrix: mutable.Map[(Double, Double), Long] = {

      val confMat: scala.collection.mutable.Map[(Double, Double), Long] = scala.collection.mutable.Map() ++ categoriesIDs.map( category1ID => categoriesIDs.map( category2ID => ( (category1ID, category2ID) , 0.toLong)  )).flatten.toMap

      println(confMat)

      test.foreach{

        labeledPoint =>

          println(confMat)

          val label: Double = labeledPoint.label
          val features: Vector = labeledPoint.features

          mModel.filter{ case (key, model) => key._1 == label | key._2 == label}.foreach{

            case ((category1ID: Double,category2ID: Double), binaryModel: SVMModel) =>

              val score: Double = binaryModel.predict(features)

              println(score)

              if(label==category1ID){
                if(score >= 0.0) confMat.update((label,label), confMat((label,label)) + 1)
                else confMat.update((label,category2ID), confMat((label,category2ID)) + 1)
              }
              else{
                if(score >= 0.0) confMat.update((label,category1ID), confMat((label,category1ID)) + 1)
                else confMat.update((label,label), confMat((label,label)) + 1)
              }

          }
          println(confMat)
      }

      println(confMat)
      confMat

    }

*/

  // confusion matrix for multiclass SVM one versus one
  val confusionMatrix: Map[(Double, Double), Int] = {

    val l: RDD[(Double, Double)] = test.flatMap{

      labeledPoint =>

        val label: Double = labeledPoint.label
        val features: Vector = labeledPoint.features

        mModel.filter{ case (key, model) => key._1 == label | key._2 == label}.map{

          case ((category1ID: Double,category2ID: Double), binaryModel: SVMModel) =>

            val score: Double = binaryModel.predict(features)

            if(label==category1ID){
              if(score >= 0.0) (label,label)
              else (label,category2ID)
            }
            else{
              if(score >= 0.0) (label,category1ID)
              else (label,label)
            }

        }

    }

    l.groupBy(x => x).map( x=> (x._1,x._2.size)).collect().toMap

  }


    // implements accuracy from scores and labels RDD
    val accuracy: Double = {
      val total: Double = confusionMatrix.map(_._2).reduceLeft(_ + _)
      val totalGood: Long = confusionMatrix.filterKeys{ case (cat1,cat2) => cat1==cat2 }.map(_._2).reduceLeft(_ + _)
      totalGood/total
    }

  val indicators: Map[Double, Map[String, Int]] = categoriesIDs.map{
    categoryID =>
      val indTrue = confusionMatrix.filterKeys(_._1 == categoryID).map(_._1._2)
      val indFalse = confusionMatrix.filterKeys(_._1 != categoryID).map(_._1._2)
      val trueSize = indTrue.size
      val falseSize = indFalse.size
      val truePositive = indTrue.count(_ == categoryID)
      val trueNegative = trueSize - truePositive
      val falsePositive = indFalse.count(_ != categoryID)
      val falseNegative = falseSize - falsePositive
      (categoryID,
        Map(
          "true" -> trueSize,
          "false" -> falseSize,
          "truePositive" -> truePositive,
          "trueNegative" -> trueNegative,
          "falsePositive" -> falsePositive,
          "falseNegative" -> falseNegative
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
