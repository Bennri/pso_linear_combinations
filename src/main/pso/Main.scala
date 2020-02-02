import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Uniform

object Main {

  def main(args: Array[String]): Unit = {

    
    val rowsAsSamples: Boolean = true
    val sampleNames: Boolean = false
    val data = DataHandler.getMatrix("data_2_class_Iris-setosa_Iris-virginica.csv").get
    // val data = DataHandler.getMatrix("heart.csv").get
    DataHandler.getDataObject(data, rowsAsSamples, sampleNames) match {
      case Some(dataObj) => {
        // println(dataObj.m(0 until 2, ::))
        println("Shape: (" + dataObj.m.rows + ", " + dataObj.m.cols + ")")
        // extend data matrix with bias 1
        var dataMatrix: DenseMatrix[Double] = null
        if (dataObj.rowsAsSamples) {
          dataMatrix = DenseMatrix.zeros[Double](dataObj.m.rows, dataObj.m.cols + 1)
          dataMatrix(::, 0) := 1.0
          dataMatrix(::, 1 to -1) := dataObj.m
        }
        else {
          dataMatrix = DenseMatrix.zeros[Double](dataObj.m.rows + 1, dataObj.m.cols)
          dataMatrix(0, ::) := 1.0
          dataMatrix(1 to -1, ::) := dataObj.m
        }
        val modDataObj = DataClass(dataMatrix, dataObj.labels, dataObj.features, dataObj.rowsAsSamples)
        val f = PSO.fitnessAccuracy(modDataObj)(_)
        // TODO init particle within min/max range of the data per dimension
        // TODO flag to on/off iteration based storing
        // for each dataset: check dimension of particles
        val pso = new PSO(modDataObj.m.cols, 5, 20, f, PSO.findGlobalBestAccuracy, randDistribution = Uniform(-10, 10))
        val finalPopulation = pso.fit

        // println(pso.toString(finalPopulation))
        val bestParticle = finalPopulation.tail.foldLeft(finalPopulation.head)((p1, p2) => Particle.max(p1, p2))
        // println(finalPopulation.head.neighborhoodBestPosAndFitness._2)
        println(bestParticle.fitness)
        println(bestParticle.position)
        println(bestParticle.position.length)
      }
      case _ => println("No data object available")
    }
    // optimization of sphere function
    /*    val pso = new PSO(2, 200, 100)
        val finalPopulation = pso.fit
        for (p <- finalPopulation) {
          println(p)
        }
        println(finalPopulation.head.neighborhoodBestPosAndFitness)*/
  }
}
