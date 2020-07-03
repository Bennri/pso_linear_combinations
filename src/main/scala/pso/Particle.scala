package pso

import breeze.linalg.DenseVector

// particle position vector, velocity vector, fitness, own best position, neighborhood best position
case class Particle(var position: DenseVector[Double],
                    var velocity: DenseVector[Double],
                    var ownBestPosAndFitness: (DenseVector[Double], Double),
                    var neighborhoodBestPosAndFitness: (DenseVector[Double], Double),
                    var fitness: Double) {


  override def toString: String = "Fitness: " + this.fitness + ", Position: " + this.position

}
object Particle {

  def apply(postion: DenseVector[Double],
            velocity: DenseVector[Double],
            ownBestPosAndFitness: (DenseVector[Double], Double),
            neighborhoodBestPosAndFitness: (DenseVector[Double], Double),
            fitness: Double): Particle = new Particle(postion, velocity, ownBestPosAndFitness, neighborhoodBestPosAndFitness, fitness)

  def getPositionAsString(p: DenseVector[Double]): String = {

    getPositionAsStringHelper(p, 0, "")
  }

  @scala.annotation.tailrec
  def getPositionAsStringHelper(p: DenseVector[Double], currIdx: Int, posString: String): String = {
    if (currIdx < p.length - 1) {
      getPositionAsStringHelper(p, currIdx + 1, posString + p.valueAt(currIdx) + " ")
    }
    else {
      posString + p.valueAt(currIdx)
    }
  }


  def max(p1: Particle, p2: Particle): Particle = {
    if (p1.fitness > p2.fitness) p1 else p2
  }

  def min(p1: Particle, p2: Particle): Particle = {
    if (p1.fitness < p2.fitness) p1 else p2
  }
}