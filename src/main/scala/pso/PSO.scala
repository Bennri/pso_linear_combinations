package pso

import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{*, DenseVector}
import breeze.stats.distributions.Rand

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector



class PSO(val particleDimension: Int,
          val iterations: Int,
          val nParticles: Int = 20,
          val f: Particle => Particle = PSO.fitnessSphereFunction,
          val neighborhood: ParVector[Particle] => Particle = PSO.findGlobalBestSphere,
          val resultPath: String = "result.json",
          // R.C. Eberhart, Y. Shi  Comparing inertia weights and constriction factors in particle swarm
          //optimization, in: Proceedings of the IEEE Congress on Evolutionary Computation, San
          //Diego, USA, 2000, pp. 84–88.
          val C1: Double = 1.49618,
          val C2: Double = 1.49618,
          val w: Double = 0.7298,
          val randDistribution: Rand[Double] = Rand.uniform,
          val bestParticleFunction: (Particle, Particle) => Particle = Particle.max) {

  // NOT extended weight vector, initialized using uniform distribution
  private[this] def createPositionVector(dim: Int): DenseVector[Double] = DenseVector.rand(dim, randDistribution)

  var V_min: DenseVector[Double] = DenseVector.zeros[Double](this.nParticles)
  var V_max: DenseVector[Double] = DenseVector.zeros[Double](this.nParticles)

  def createPopulation(populationSize: Int): Vector[Particle] = {
    @tailrec
    def buildPopulation(counter: Int, populationSize: Int, population: Vector[Particle]): Vector[Particle] = {
      counter compare populationSize match {
        case 0 => population
        case _ => val pos = createPositionVector(particleDimension)
          buildPopulation(counter + 1,
            populationSize,
            population :+ f(Particle( // call f(...) to evaluate the particle
              pos,
              DenseVector.zeros[Double](particleDimension),
              (pos, 0.0), // current own best position => 0.0 for fitness since it is not evaluated yet
              (DenseVector.rand(particleDimension, randDistribution), 0.0), // current global best position => 0.0 for fitness since it is not evaluated yet
              0.0))) // fitness not evaluated yet
      }
    }
    buildPopulation(0, populationSize, Vector[Particle]())
  }


  def updateVelocity(currParticle: Particle): Particle = {
    // compute new velocity
    val r1 = math.random()
    val r2 = math.random()
    val newVelocity: DenseVector[Double] = this.w * currParticle.velocity + this.C1 * r1 * (currParticle.ownBestPosAndFitness._1 - currParticle.position) + this.C2 * r2 * (currParticle.neighborhoodBestPosAndFitness._1 - currParticle.position)

    // velocity clipping
    for (i <- Range(0, newVelocity.length)) {
      newVelocity(i) = newVelocity(i) match {
        case x if x > V_max(i) => V_max(i)
        case x if x < V_min(i) => V_min(i)
        case _ => newVelocity(i)
      }
    }
    currParticle.velocity = newVelocity
    currParticle
  }
  def toString(population: ParVector[Particle]): String = {
    var s = ""
    for (i <- Range(0, population.length)) {
      s += population(i).toString + "\n"
    }
    s
  }

  def fit: ParVector[Particle] = {
    var stats: Vector[ujson.Obj] = Vector[ujson.Obj]()
    // create population
    var population: ParVector[Particle] = createPopulation(this.nParticles).par
    // compute init best neighbor
    val currentBest = neighborhood(population)
    // evaluate random init positions
    population.par.foreach(p => {
      p.neighborhoodBestPosAndFitness = (currentBest.position, currentBest.fitness)
    })
    // compute V_min and V_max for clipping extreme velocities
    val tup = PSO.velocityClippingStrategy(population)
    this.V_min = tup._1
    this.V_max = tup._2

    for (i <- Range(0, iterations)) {
      if (i % 5 == 0) {
        println(s"Iteration: $i")
      }
      // compute new velocity
      population = population.par.map(p => updateVelocity(p))
      // update position
      population = population.par.map(p => {
        p.position = p.position + p.velocity
        p
      })
      // evaluate particle by given fitness function
      // which also checks for best known position of particle i
      population = population.par.map(p => f(p))
      // TODO: provide better way for using other neighborhood functions
      //choose best in the neighborhood (global)
      val currentBest = neighborhood(population)
      // update all particles: set global best
      population.par.foreach(p => {
        p.neighborhoodBestPosAndFitness = (currentBest.position, currentBest.fitness)
      })

      // store each iteration
      stats = stats :+ storePopulationInfosPerIteration(population, i)
    }

    val file = new File(resultPath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(ujson.write(stats, indent = 4))
    bw.close()

    // best in population
    // val bestParticle: Particle = population.tail.foldLeft(population.head)((p1, p2) => bestParticleFunction(p1, p2))
    population
  }

  def storePopulationInfosPerIteration(population: ParVector[Particle], i: Int): ujson.Obj = {
    // current best position
    val bestParticle: Particle = population.tail.foldLeft(population.head)((p1, p2) => bestParticleFunction(p1, p2))
    val cBest = Particle.getPositionAsString(bestParticle.position)
    // create JSON object to store some information per iteration
    // a list of JSON objects will be stored to disc after training
    val currenDataI = ujson.Obj("iteration" -> ujson.Num(i),
      "currentBest" -> ujson.Str(cBest),
      "currentBestFitness" -> ujson.Num(population.head.neighborhoodBestPosAndFitness._2),
    "meanFitnessSwarm" -> ujson.Num(population.foldLeft(0.0)((acc, p) => acc + p.fitness) / population.length))
    currenDataI
  }

}

object PSO {

  def apply(particleDimension: Int,
            iterations: Int,
            nParticles: Int,
            f: Particle => Particle,
            neighborhood: ParVector[Particle] => Particle,
            resultPath: String,
            C1: Double,
            C2: Double,
            w: Double,
            randDistribution: Rand[Double]): PSO = new PSO(particleDimension,
    iterations,
    nParticles,
    f,
    neighborhood,
    resultPath,
    C1,
    C2,
    w,
    randDistribution)


  def fitnessSphereFunction(particle: Particle): Particle = {
    // f(x) = \sum\limits_{i=1}^{d} x_i^2
    val fitness: Double = particle.position.foldLeft(0.0)((acc, x) => acc + math.pow(x, 2))
    // if the new position is better than the current own best position
    // store the new one
    if (fitness < particle.ownBestPosAndFitness._2) {
      particle.ownBestPosAndFitness = (particle.position, fitness)
    }
    // set new fitness
    particle.fitness = fitness
    particle
  }

  def findGlobalBestSphere(population: ParVector[Particle]): Particle = {
    population.tail.foldLeft(population.head)(
      (p1, p2) => {
        Particle.min(p1, p2)
      })
  }

  def findGlobalBestAccuracy(population: ParVector[Particle]): Particle = {
    population.tail.foldLeft(population.head)(
      (p1, p2) => {
        Particle.max(p1, p2)
      })
  }

  def velocityClippingStrategy(population: ParVector[Particle], delta: Double = 0.1): (DenseVector[Double], DenseVector[Double]) = {
    val V_min_current: DenseVector[Double] = DenseVector.zeros[Double](population.head.position.length)
    val V_max_current: DenseVector[Double] = DenseVector.zeros[Double](population.head.position.length)
    for (i <- Range(0, V_min_current.length)) {
      V_min_current(i) = population.par.map(p => p.position(i)).min
      V_max_current(i) = population.par.map(p => p.position(i)).max
    }
    val V_max = delta * (V_max_current - V_min_current)
    val V_min = delta * (V_min_current - V_max_current)
    (V_min, V_max)
  }

  def fitnessAccuracy(dataClass: DataClass)(particle: Particle): Particle = {
    var preds: DenseVector[Double] = null
    if (dataClass.rowsAsSamples) {
      // for each row, take all columns
      preds = dataClass.m(*, ::).map(x => x dot particle.position)
    }
    else {
      // for each column, take all rows
      preds = dataClass.m(::, *).map(x => x dot particle.position).t
    }
    val castedLabels: DenseVector[Double] = dataClass.labels.map(x => x.toDouble)

    val zippedTuples = preds.toArray.zip(castedLabels.toArray)
    val fitness = zippedTuples.foldLeft(0.0)((acc, tup) => {
      var a = 0.0
      if (tup._1 >= 0.0 && tup._2 == 1.0) {
        // correct class
        a += 1.0
      }
      else if (tup._1 < 0.0 && tup._2 == 0.0) {
        // correct class
        a += 1.0
      }
      // if both cases are false, a is still 0 - so no problem here
      acc + a
    })
    // check for own best position
    if (fitness > particle.ownBestPosAndFitness._2) {
      particle.ownBestPosAndFitness = (particle.position, fitness)
    }
    particle.fitness = fitness
    particle
  }


}
