import FileHandler.loadCSVdata
import breeze.linalg.{DenseMatrix, DenseVector}


object DataHandler {

  def getMatrix(filePath: String, seperator: String = ","): Option[DenseMatrix[String]] = {
    /*
    Load data from CSV file and store it within a breeze matrix.
     */
    loadCSVdata(filePath).get match {
      case l: Array[String] => {
        // split each line at the separator
        val stringSplits = l.map(s => s.split(seperator))
        // create a matrix with the dimension (number of lines, splits of line)
        val dm = DenseMatrix.zeros[String](stringSplits.length, stringSplits.head.length)
        // place the data in the matrix
        for (i <- stringSplits.indices) {
          dm(i, ::) := DenseVector(stringSplits(i)).t // transpose to match row shape
        }
        Some(dm)
      }
      case _ => None
    }
  }


  // DenseMatrix[Double], DenseVector[String], DenseVector[String]
  def getDataMatrixString(m: DenseMatrix[String],
                          rowsAsSamples: Boolean = true,
                          sampleNames: Boolean = true,
                          featureNames: Boolean = true):
  // return the data matrix, vector of feature names, vector of sampleNames
  Option[(DenseMatrix[Double], DenseVector[String])] = {

    // ToDo extract target and check indices again
    val res = (rowsAsSamples, sampleNames, featureNames) match {

      // usage e.g. armstrong data set
      // samples as column vectors
      case (false, true, true) => {
        // one row as sample names, one row as target, one column as feature names
        val data: DenseMatrix[Double] = DenseMatrix.zeros[Double](m.rows - 1, m.cols - 1)
        for (i <- Range(1, m.rows)) {
          data(i - 1, ::) := m(i, 1 until m.cols).t.map(x => x.toDouble).t
        }
        val featureNames = m(1 until m.rows, 0)
        Some(data, featureNames)
      }
      // usage e.g. heart.csv
      // sample as rows, no sample names
      case (true, false, true) => {
        val data: DenseMatrix[Double] = DenseMatrix.zeros[Double](m.rows - 1, m.cols)
        for (i <- Range(1, m.rows)) {
          data(i - 1, ::) := m(i, ::).t.map(x => x.toDouble).t
        }
        val featureNames = m(0, 1 until m.cols)
        Some(data, featureNames.t)

      }
      case _ => None
    }
    res
  }

  def getDataObject(m: DenseMatrix[String],
                    rowsAsSamples: Boolean = true,
                    sampleNames: Boolean = false,
                    featureNames: Boolean = true): Option[DataClass] = {
    // assumptions:
    // row as samples is true -> last column contains labels
    // rows as features is true -> first row contains labels
    val tup = DataHandler.getDataMatrixString(m, rowsAsSamples, sampleNames, featureNames)
    tup match {
      case Some(res) => {
        if (rowsAsSamples) {
          // last column contains the labels
          val data: DenseMatrix[Double] = DenseMatrix.zeros[Double](res._1.rows, res._1.cols - 1)
          for (i <- Range(0, res._1.rows)) {
            data(i, ::) := res._1(i, 0 to -2)
          }
          val labels: DenseVector[Int] = res._1(::, -1).map(x => x.toInt)
          Some(DataClass(data, labels, res._2, rowsAsSamples))
        }
        else {
          // consider assumption: first row contains targets
          val data: DenseMatrix[Double] = DenseMatrix.zeros[Double](res._1.rows -1, res._1.cols)
          for (i <- Range(1, res._1.rows)) {
            data(i - 1, ::) := res._1(i, ::)
          }
          val labels: DenseVector[Int] = res._1(0, ::).t.map(x => x.toInt)
          Some(DataClass(data, labels, res._2, rowsAsSamples))
        }
      }
      case _ => None
    }
  }
}

