import common._

import scala.collection.parallel.CollectionConverters._

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

  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]):Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      new Punto(puntos.map(p => p.x).sum / puntos.length, puntos.map(p=>p.y).sum / puntos.length)
    }
  }

  
  def calculePromedioPar(medianaVieja: Punto, puntos: Seq[Punto]):Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      val puntosPar = puntos.par
      new Punto(puntosPar.map(p => p.x).sum / puntos.length, puntosPar.map(p=>p.y).sum / puntos.length)
    }
  }

  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]) = {
    medianasViejas.map { medianaVieja =>
      val puntosCluster = clasif.getOrElse(medianaVieja, Seq())
      calculePromedioSeq(medianaVieja, puntosCluster)
    }
  }

  // paralelismo de datos
  def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]) = {
    medianasViejas.par.map { medianaVieja =>
      val puntosCluster = clasif.getOrElse(medianaVieja, Seq())
      calculePromedioPar(medianaVieja, puntosCluster)
    }.seq
  }

  def hayConvergenciaSeq(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    medianasViejas.zip(medianasNuevas).forall { case (vieja, nueva) =>
      vieja.distaciaAlCuadrado(nueva) <= eta
  }
  }

  def hayConvergenciaPar(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    val n = medianasViejas.length

    if (n <= 1) {
      //caso base
      hayConvergenciaSeq(eta, medianasViejas, medianasNuevas)
    } else {
      val m = n / 2
      val (convergencia1, convergencia2) = parallel(
        hayConvergenciaPar(eta, medianasViejas.slice(0,m), medianasNuevas.slice(0,m)),
        hayConvergenciaPar(eta, medianasViejas.slice(m,n), medianasNuevas.slice(m,n))
      )
      //todas dos deben converger
      convergencia1 && convergencia2
    }
  }

}
