package pso

import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Uniform

object Main {

  // argument parsing adopted from https://stackoverflow.com/questions/2315912/best-way-to-parse-command-line-parameters/3183991#3183991
  val helpText =
    """ Usage: java -jar pso_linear_combinations*.jar
      [--n-particles <int>] number of particles in the swarm
      [--dim <int>] dimension of the particle
      [--data-path <path>] path to a csv file which contains data set delimiter by ','
      [--output-file-path <path to a results file>] a file where the results are stored in json format e.g. outputDir/results.json
      [--iterations <int iterations>]  evolving the swarm over n iterations
      [--rows-as-samples <true|false>] each row represents a sample
      [--sample-names-available <true|false>]  samples have an extra column which stores a unique name or ID
    """

  def main(args: Array[String]): Unit = {
    val necessaryParameters = Set("rowsAsSamples", "sampleNames", "nParticles", "dataset", "dim", "iterations", "outputFile")
    var everythingIsFine: Boolean = true
    var options: Map[String, Any] = Map()

    @scala.annotation.tailrec
    def nextOption(map: Map[String, Any], list: List[String]): Map[String, Any] = {
      val testValue: Int => Int = x => if (x > 0) x else 1
      list match {
        case Nil => map
        case "--n-particles" :: value :: tail =>
          println(if (value.toInt > 0) value.toInt else 1)
          nextOption(map ++ Map("nParticles" -> testValue(value.toInt)), tail)
        case "--dim" :: value :: tail =>
          nextOption(map ++ Map("dim" -> testValue(value.toInt)), tail)
        case "--data-path" :: value :: tail => nextOption(map ++ Map("dataset" -> value.toString), tail)
        case "--output-file-path" :: value :: tail => nextOption(map ++ Map("outputFile" -> value.toString), tail)
        case "--iterations" :: value :: tail => nextOption(map ++ Map("iterations" -> testValue(value.toInt)), tail)
        case "--rows-as-samples" :: value :: tail => nextOption(map ++ Map("rowsAsSamples" -> value.toString), tail)
        case "--sample-names-available" :: value :: tail => nextOption(map ++ Map("sampleNames" -> value), tail)
        case _ :: tail => {
          println("Unknown option ")
          println(list)
          everythingIsFine = false
          map
        }
      }
    }


    if (args.length == 0) {
      println(helpText)
    } else {
      options = nextOption(options, args.toList)
      if (everythingIsFine && options.keys == necessaryParameters) {
        println("parameter ok")
        println(options)

        print("Dataset path: ")
        println(options("dataset"))
        print("Dimension: ")
        println(options("dim"))
        print("Iterations: ")
        println(options("iterations"))
        print("Number of particles: ")
        println(options("nParticles"))
        print("Rows are samples: ")
        println(options("rowsAsSamples"))
        print("Sample names set: ")
        println(options("sampleNames"))
        print("Output file: ")
        println(options("outputFile"))

        // val rowsAsSamples: Boolean = true
        // val sampleNames: Boolean = false
        val data = DataHandler.getMatrix(options("dataset").toString).get
        DataHandler.getDataObject(data, options("rowsAsSamples").toString.toBoolean, options("sampleNames").toString.toBoolean) match {
          case Some(dataObj) => {
            // println(dataObj.m(0 until 2, ::))
            println("Shape: (" + dataObj.m.rows + ", " + dataObj.m.cols + ")")
            // extend data matrix with bias 1
            var dataMatrix: DenseMatrix[Double] = null
            if (options("rowsAsSamples").toString.toBoolean) {
              dataMatrix = DenseMatrix.zeros[Double](dataObj.m.rows, dataObj.m.cols + 1)
              dataMatrix(::, 0) := 1.0
              dataMatrix(::, 1 to -1) := dataObj.m
            }
            else {
              dataMatrix = DenseMatrix.zeros[Double](dataObj.m.rows + 1, dataObj.m.cols)
              dataMatrix(0, ::) := 1.0
              dataMatrix(1 to -1, ::) := dataObj.m
            }
            val modDataObj = DataClass(dataMatrix, dataObj.labels, dataObj.features, options("rowsAsSamples").toString.toBoolean)
            val f = PSO.fitnessAccuracy(modDataObj)(_)
            // TODO init particle within min/max range of the data per dimension
            // TODO flag to on/off iteration based storing
            // for each dataset: check dimension of particles
            val pso = new PSO(modDataObj.m.cols,
              options("iterations").toString.toInt,
              options("nParticles").toString.toInt, f, PSO.findGlobalBestAccuracy,
              resultPath = options("outputFile").toString,
              randDistribution = Uniform(-10, 10))
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
      } else {
        System.exit(1)
      }
    }


  }
}
