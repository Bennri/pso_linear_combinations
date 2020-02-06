package pso

import java.io.FileNotFoundException

import breeze.linalg.{DenseMatrix, DenseVector, csvread}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FileHandler {

  def loadCSVdata(filePath: String): Option[Array[String]] = {
    /*
    Load CSV file from disc into an array of strings, one string per line.
     */
    val res = try {
      val fd = scala.io.Source.fromFile(filePath)
      val data = fd.getLines().toArray
      fd.close()
      Some(data)
    }
    catch {
      case e: FileNotFoundException => {
        e.printStackTrace()
        None
      }
    }
    res
  }
}
