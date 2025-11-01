import common._

package object kmedianas2D {
  class Punto(val x: Double, val y: Double) {
    private def cuadrado(v: Double):Double = v * v

    def distaciaAlCuadrado(that: Punto):Double =
      cuadrado(that.x - x) + cuadrado(that.y - y)

    private def round(v: Double):Double = (v * 100).toInt  / 100.0

    override def toString = s"(${round(x)}, ${round(y)})"
  }

  def hallarPuntoMasCercano(p: Punto, medianas: Seq[Punto]):Punto = {
    assert(medianas.nonEmpty)
    medianas.map(pto => (pto, p.distaciaAlCuadrado(pto))).sortWith((a,b) => (a._2 < b._2)).head._1
  }

  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]):Map[Punto, Seq[Punto]] = {
    puntos.groupBy(punto => hallarPuntoMasCercano(punto, medianas))
  }
  
  def clasificarPar(umb:Int)(puntos: Seq[Punto], medianas: Seq[Punto]):Map[Punto, Seq[Punto]] = {
    if (puntos.length <= umb) {
      clasificarSeq(puntos, medianas)
    } else {
      val n = puntos.length
      val m = n / 2
      val (mapa1, mapa2) = parallel(clasificarPar(umb)(puntos.slice(0,m), medianas), clasificarPar(umb)(puntos.slice(m,n), medianas))

      mapa1 ++ mapa2.map { case (k, v) => k -> (mapa1.getOrElse(k, Seq()) ++ v) }
    }
  }

  def
  
}
