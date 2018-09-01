def nrOnes(x: Int): Int = {
  def _tailOnes(a: Int, r: Int): Int = {
     if (a == 0) r
     else _tailOnes(a & (a-1), r+1)
  }
  _tailOnes(x, 0)
}

def nrOnes2(x: Int): Int = {
  Stream.iterate(x) { a => a & (a-1)}.takeWhile(_ > 0).length
}
