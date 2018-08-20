import HighOLists.queens

object HighOLists extends App {

  def isPrime(n: Int): Boolean = !(2 until n).exists(x => n % x == 0)

  println(isPrime(13))
  println(isPrime(44))

  val n: Int = 7

  val k = for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i + j)

  println(k)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (for {
      x <- xs zip ys
    } yield (x._1 * x._2)).sum

  val m = scalarProduct(Vector(1,1,1,1), Vector(2,2,2,2))

  println(m)

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int) : Set[List[Int]] = {
      if(k==0) Set(List())
      else
        for {
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val rList = for(i <- 1 to queens.length) yield(queens.length-i)
    val offCol = for {
      (x,y) <- rList zip queens
      bal <- 1 to queens.length
      if(x+bal == queens.length)
    } yield(bal, y)
    val finalSet = offCol.map{case (x,y) => List(y+x, y-x)}.flatten
    if(queens.contains(col)) false
    else if(finalSet.contains(col)) false
    else true
  }

  /*def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row-1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r,c) => col != c && math.abs(col-c) != row-r
    }
  }*/

  def show(queens: List[Int]) = {
    val lines = for(col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" +(lines mkString "\n")
  }

  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue(0.0)
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
      val (exp, coeff) = term
      terms ++ Map(exp -> (coeff+terms(exp)))
    }


      /*new Poly(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }*/
    override def toString = (for ((exp,coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString "+"
  }

  val p1 = new Poly(1->2.0, 3->4.0, 5->6.2)
  val p2 = new Poly(0->3.0, 3->7.0)

  p1 + p2



}
