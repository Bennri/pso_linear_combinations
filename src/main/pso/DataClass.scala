import breeze.linalg.{DenseMatrix, DenseVector}

case class DataClass(m: DenseMatrix[Double],
                     labels: DenseVector[Int],
                     features: DenseVector[String],
                     rowsAsSamples: Boolean)

object DataClass {
  def apply(m: DenseMatrix[Double],
            labels: DenseVector[Int],
            features: DenseVector[String],
            rowsAsSamples: Boolean): DataClass = new DataClass(m, labels, features, rowsAsSamples)
}

