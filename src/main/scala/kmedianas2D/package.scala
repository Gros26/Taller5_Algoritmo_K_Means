import common._

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.util.Random

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

  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    for (medianaVieja <- medianasViejas) yield {
      val puntosCluster = clasif.getOrElse(medianaVieja, Seq())
      calculePromedioSeq(medianaVieja, puntosCluster)
    }
  }

  // paralelismo de datos
  def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    (for (medianaVieja <- medianasViejas.par) yield {
      val puntosCluster = clasif.getOrElse(medianaVieja, Seq())
      calculePromedioPar(medianaVieja, puntosCluster)
    }).seq
  }

  //debe ser iterativa o sea de cola
  @tailrec
  def hayConvergenciaSeq(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    (medianasViejas, medianasNuevas) match {
      case (Seq(), Seq()) => true  //convergio
      case (v +: vs, n +: ns) =>
        if (v.distaciaAlCuadrado(n) <= eta) {
          hayConvergenciaSeq(eta, vs, ns)
        } else {
          false
        }
      case _ => false
    }
  }

  def hayConvergenciaPar(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    (medianasViejas, medianasNuevas) match {
      case (Seq(), Seq()) | (Seq(_), Seq(_)) => 
        //caso base
        hayConvergenciaSeq(eta, medianasViejas, medianasNuevas)
      case (vs, ns) =>
        val m = vs.length / 2
        val (convergencia1, convergencia2) = parallel(
          hayConvergenciaPar(eta, vs.slice(0, m), ns.slice(0, m)),
          hayConvergenciaPar(eta, vs.slice(m, vs.length), ns.slice(m, ns.length))
        )
        //los dos deben converger
        convergencia1 && convergencia2
    }
  }

  @tailrec
  final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val clasificados = clasificarSeq(puntos, medianas)
    val medianasNuevas = actualizarSeq(clasificados, medianas)
    if (hayConvergenciaSeq(eta, medianas, medianasNuevas)) medianasNuevas
    else kMedianasSeq(puntos, medianasNuevas , eta)
  }

  @tailrec
  final def kMedianasPar(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val umbral = 10
    val clasificados = clasificarPar(umbral)(puntos, medianas)
    val medianasNuevas = actualizarPar(clasificados, medianas)
    if (hayConvergenciaPar(eta, medianas, medianasNuevas)) medianasNuevas
    else kMedianasPar(puntos, medianasNuevas, eta)
  }

  def generarPuntos(k: Int, num: Int):Seq[Punto] = {
    val randx= new Random
    val randy = new Random
    (0 until num).map({ i =>
      val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
      val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
      new Punto(x,y)
    })
  }

  def inicializarMedianas(k: Int, puntos: Seq[Punto]):Seq[Punto] = {
    val rand = new Random
    (0 until k).map( _ => puntos(rand.nextInt(puntos.length)))
  }
}
