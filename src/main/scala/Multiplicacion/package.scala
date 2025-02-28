import javax.naming.spi.DirStateFactory.Result
import scala.annotation.tailrec

package object Multiplicacion {
  def PeasantAlgorithm (a:Int, b:Int): Int = {
    def PeasantAlgorithmRecursiva(a:Int, b:Int): Int = {
      if (a == 1) b else
        if (a%2 != 0) b + PeasantAlgorithmRecursiva(a / 2, b + b)
        else PeasantAlgorithmRecursiva(a / 2, b + b)
    }
    PeasantAlgorithmRecursiva(a:Int, b:Int)
  }

  def PeasantAlgorithmIt (x:Int, y:Int): Int = {
    def PeasantAlgorithmRev(x:Int, y:Int, r:Int): Int = {
      if (x == 1) y+r else
        if (x%2 != 0) PeasantAlgorithmRev(x/2, y + y, r + y)
        else PeasantAlgorithmRev(x/2, y + y, r)
    }
    PeasantAlgorithmRev(x, y, 0)
  }

}
