package MLModels

import org.apache.spark.mllib.linalg

trait MLModel extends java.io.Serializable{

  def sqDistance(v1:linalg.Vector, v2:linalg.Vector): Double = {
    assert(v1.size == v2.size, "The two vectors should have same length to calculate their squared distance.")
    (v1.toArray zip v2.toArray).map{ case (x:Double,y:Double) => (x-y)*(x-y) }.foldLeft[Double](0.0)( (tmp,z) => tmp+z )
  }


  // helper functions
  def goodLabel(initialLabel: Double, wantedLabel: Double): Double =
    if(initialLabel==wantedLabel) 1.0 else 0.0


}
