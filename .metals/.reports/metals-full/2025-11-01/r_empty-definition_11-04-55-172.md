error id: file://<WORKSPACE>/src/main/scala/package.scala:`<none>`.
file://<WORKSPACE>/src/main/scala/package.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -combinarMapas.
	 -combinarMapas#
	 -combinarMapas().
	 -scala/Predef.combinarMapas.
	 -scala/Predef.combinarMapas#
	 -scala/Predef.combinarMapas().
offset: 1645
uri: file://<WORKSPACE>/src/main/scala/package.scala
text:
```scala
package object kmedianas2d {
  class Punto(val x: Double, val y: Double) {
    private def cuadrado(v: Double):Double = v * v

    def distaciaAlCuadrado(that: Punto):Double =
      cuadrado(that.x - x) + cuadrado(that.y - y)

    private def round(v: Double):Double = (v * 100).toInt  / 100.0

    override def toString = s"(round(x), {round(y)})"
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
      val (puntos1, puntos2) = parallel(clasificarPar(umb)(puntos.slice(0,m)), clasificarPar(umb)(m,n))
    }
  }

  def clasificarPar(umb:Int)(puntos: Seq[Punto], medianas: Seq[Punto]):Map[Punto, Seq[Punto]] = {
  if (puntos.length <= umb) {
    // Caso base: usar versión secuencial
    clasificarSeq(puntos, medianas)
  } else {
    // Caso recursivo: dividir en mitades
    val mitad = puntos.length / 2
    val (puntos1, puntos2) = puntos.splitAt(mitad)
    
    // Procesar ambas mitades en paralelo
    val (mapa1, mapa2) = parallel(
      clasificarPar(umb)(puntos1, medianas),
      clasificarPar(umb)(puntos2, medianas)
    )
    
    // Combinar los mapas resultantes
    combinarMap@@as(mapa1, mapa2)
  }
}
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.