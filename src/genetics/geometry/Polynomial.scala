package genetics.geometry

/**
  * Represents a polynomial starting with the constant coefficient
  *
  * Ex. new Polynomial(List(5, -2, 1)) represents x^2^ - 2x + 5
  *
  */
class Polynomial(val coefficients: List[Double]) {

  def evaluate(x: Double): Double = {
    var y = 0.0
    for (coefficient <- coefficients.reverse) {
      y *= x
      y += coefficient
    }
    y
  }

  override def toString: String = {
    coefficients.toString
  }

}
