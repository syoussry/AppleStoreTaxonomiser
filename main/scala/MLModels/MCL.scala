package MLModels

import org.apache.spark.rdd.RDD
import breeze.linalg.{sum, *, DenseMatrix}
import breeze.numerics.pow


case class MCL(data:RDD[org.apache.spark.mllib.linalg.Vector], r:Int, numIterations:Int) extends MLModel {

  def Minit: DenseMatrix[Double] = {

    val n: Int= data.count().toInt

    val normalizedDistanceArray: Array[Double] =
      data.map{
        v1 =>
          val distRow = data.map{ v2 => sqDistance(v1,v2) }.collect()
          val const = distRow.foldLeft[Double](0.0)((tmp,x)=>tmp+x)
          distRow.map(_/const)
      }.collect().flatten

      new DenseMatrix(n, n, normalizedDistanceArray)

  }

  def makeStochastic(M: DenseMatrix[Double]): DenseMatrix[Double] = {
    val columnSum: DenseMatrix[Double] = sum(M(::,*))
    M(*, ::) / columnSum.toDenseVector
  }

  def inflate(M: DenseMatrix[Double]): DenseMatrix[Double] = pow(M,r)

  def expand(M: DenseMatrix[Double]): DenseMatrix[Double] = M * M

  def algo(oldM:DenseMatrix[Double], newM:DenseMatrix[Double], accIterations:Int):DenseMatrix[Double] = {

    if(accIterations<=numIterations){
      println("iter num:" + accIterations + " done")
      algo(newM, inflate(expand(newM)),accIterations+1)
    }else newM

  }

  def go = {
    val M = Minit
    algo(null, M, 0)
  }

}
