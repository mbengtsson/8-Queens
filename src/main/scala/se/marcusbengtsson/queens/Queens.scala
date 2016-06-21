package se.marcusbengtsson.queens

/**
  * Functional solution to the "8 Queens problem" in Scala
  */
object Queens {

  def main(args: Array[String]) {
    allSolutions(8, Nil).head foreach print _
  }


  def directions: List[Int => Int] = ((x: Int) => x + 1) :: ((x: Int) => x - 1) :: ((x: Int) => x) :: Nil

  def okToAdd(row: Int, partial: List[Int]): Boolean = directions map (okToAddDirection(row, partial)(_)) reduce (_ && _)

  def okToAddDirection(row: Int, partial: List[Int])(dir: Int => Int): Boolean = (partial, Stream.iterate(row)(dir(_)).tail).zipped map (_ != _) forall (_ == true)

  def extendSolution(partial: List[Int]): List[List[Int]] = List.range(0, 8) filter (okToAdd(_, partial)) map (_ :: partial)

  def allSolutions(queens: Int, acc: List[Int]): List[List[Int]] = {
    if (queens == 0) List(acc)
    else extendSolution(acc) flatMap (allSolutions(queens - 1, _))
  }
}