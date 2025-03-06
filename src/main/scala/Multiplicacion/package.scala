package object Multiplicacion {
  def PeasantAlgorithm (a:Int, b:Int): Int = {
    def PeasantAlgorithmRecursiva(a:Int, b:Int): Int = {
      if (a == 1) b else
        if (a%2 != 0) b + PeasantAlgorithmRecursiva(a / 2, b + b)
        else PeasantAlgorithmRecursiva(a / 2, b + b)
    }
    PeasantAlgorithmRecursiva(a, b)
  }

  def PeasantAlgorithmIt (x:Int, y:Int): Int = {
    def PeasantAlgorithmRev(x:Int, y:Int, r:Int): Int = {
      if (x == 1) y+r else
        if (x%2 != 0) PeasantAlgorithmRev(x/2, y + y, r + y)
        else PeasantAlgorithmRev(x/2, y + y, r)
    }
    PeasantAlgorithmRev(x, y, 0)
  }

  def splitMultiply(a:Int, b:Int): Int = {

    def maxAB(a: Int, b: Int): Int = {
      if (a >= b) a else b
    }
    def countDig(a:Int, count:Int): Int = {
      if (a<10) count else countDig(a/10, count + 1)
    }

    def splitMuliplyR(a:Int, b:Int):Int = {

      if (a < 10 && b < 10)
        a*b
      else
        val m: Int = maxAB(countDig(a, 1), countDig(b, 1))/2
        val pow: Int = math.pow(10, m).toInt

        val x: Int = a / pow
        val y: Int = a % pow
        val z: Int = b / pow
        val w: Int = b % pow

        val p1: Int = splitMuliplyR(x, z)
        val p2: Int = splitMuliplyR(y, z)
        val p3: Int = splitMuliplyR(x, w)
        val p4: Int = splitMuliplyR(y, w)

        p1 * math.pow(10, 2 * m).toInt + (p2 + p3) * pow + p4
    }
    splitMuliplyR(a,b)
  }

  def FastAlgorithm(a: Int, b: Int): Int = {

    def maxAB(a: Int, b: Int): Int = {
      if (a >= b) a else b
    }

    def countDig(a: Int, count: Int): Int = {
      if (a < 10) count else countDig(a / 10, count + 1)
    }

    def FastAlgorithmR(a: Int, b: Int): Int = {
      if (a < 10 && b < 10)
        a * b
      else
        val m: Int = maxAB(countDig(a, 1), countDig(b, 1)) / 2
        val pow: Int = math.pow(10, m).toInt

        val x: Int = a / pow
        val y: Int = a % pow
        val z: Int = b / pow
        val w: Int = b % pow

        val p1: Int = FastAlgorithmR(x, z)
        val p2: Int = FastAlgorithmR(y, w)
        val p3: Int = p1 + p2 - FastAlgorithmR(x-y,z-w)

        p1 * math.pow(10, 2 * m).toInt + p3 * pow + p2
    }

    FastAlgorithmR(a, b)
  }

}

