package MLModels

import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.regression.LabeledPoint

case class NBKBO(categoriesIDs: List[Double], training:RDD[LabeledPoint], test:RDD[LabeledPoint]) extends MLModel  {



}
