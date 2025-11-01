import common._
import kmedianas2D._

// Crear algunos puntos de ejemplo
val puntos = Seq(
  new Punto(1.0, 1.0),
  new Punto(1.5, 1.2),
  new Punto(2.0, 1.8),
  new Punto(8.0, 8.0),
  new Punto(8.5, 8.2),
  new Punto(9.0, 8.5),
  new Punto(1.0, 8.0),
  new Punto(1.2, 8.5),
  new Punto(8.0, 1.0),
  new Punto(8.5, 1.5)
)

// Definir medianas (centros iniciales)
val medianas = Seq(
  new Punto(1.0, 1.0), // Mediana en esquina inferior izquierda
  new Punto(8.0, 8.0), // Mediana en esquina superior derecha
  new Punto(1.0, 8.0) // Mediana en esquina superior izquierda
)

val resultadoSeq = clasificarSeq(puntos, medianas)
resultadoSeq.foreach { case (mediana, pts) =>
  pts.foreach(p => println(s"  $p"))
}

val resultadoPar = clasificarPar(3)(puntos, medianas)
resultadoPar.foreach { case (mediana, pts) =>
  pts.foreach(p => println(s"  $p"))
}

val sonIguales = resultadoSeq.keys.forall { k =>
  resultadoSeq(k).toSet == resultadoPar(k).toSet
}
