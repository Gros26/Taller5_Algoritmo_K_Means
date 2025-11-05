import Benchmark._
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


// ==================== PRUEBAS BÁSICAS ====================

// Crear puntos de prueba simples
val p1 = new Punto(1.0, 1.0)
val p2 = new Punto(2.0, 2.0)
val p3 = new Punto(8.0, 8.0)
val p4 = new Punto(9.0, 9.0)

// ==================== 1. PRUEBAS hallarPuntoMasCercano ====================

// Caso 1: Punto cerca de la primera mediana
// Esperado: p1
val medianas1 = Seq(p1, p3)
val resultado1 = hallarPuntoMasCercano(new Punto(1.5, 1.5), medianas1)

// Caso 2: Punto cerca de la segunda mediana
// Esperado: p3
val resultado2 = hallarPuntoMasCercano(new Punto(8.5, 8.5), medianas1)

// Caso 3: Punto equidistante (más cerca del primero por orden)
// Esperado: alguna de las medianas
val resultado3 = hallarPuntoMasCercano(new Punto(5.0, 5.0), medianas1)

// ==================== 2. PRUEBAS clasificarSeq ====================

// Caso 1: Clasificar 4 puntos en 2 clusters
val puntosTest = Seq(p1, p2, p3, p4)
val medianasTest = Seq(p1, p3)
val clasif1 = clasificarSeq(puntosTest, medianasTest)
// Esperado: Map con 2 claves, p1 tiene (p1, p2), p3 tiene (p3, p4)

// Caso 2: Todos los puntos cerca de una mediana
val puntosIguales = Seq(new Punto(1.1, 1.1), new Punto(1.2, 1.2), new Punto(1.3, 1.3))
val clasif2 = clasificarSeq(puntosIguales, medianasTest)
// Esperado: Map con 1 o 2 claves, mayoría en p1

// Caso 3: Clasificación vacía
val clasif3 = clasificarSeq(Seq(), medianasTest)
// Esperado: Map vacío

// ==================== 3. PRUEBAS clasificarPar ====================

// Caso 1: Con umbral bajo (fuerza paralelismo)
val clasifPar1 = clasificarPar(2)(puntosTest, medianasTest)
// Esperado: Mismo resultado que clasificarSeq

// Caso 2: Con umbral alto (usa secuencial)
val clasifPar2 = clasificarPar(10)(puntosTest, medianasTest)
// Esperado: Mismo resultado que clasificarSeq

// Caso 3: Comparación con muchos puntos
val puntosMuchos = generarPuntos(3, 100)
val medianasMuchos = inicializarMedianas(3, puntosMuchos)
val clasifSeqGrande = clasificarSeq(puntosMuchos, medianasMuchos)
val clasifParGrande = clasificarPar(20)(puntosMuchos, medianasMuchos)
// Esperado: Ambos deben tener las mismas claves y valores

// ==================== 4. PRUEBAS calculePromedioSeq ====================

// Caso 1: Promedio de 2 puntos
val promedio1 = calculePromedioSeq(p1, Seq(p1, p2))
// Esperado: Punto(1.5, 1.5)

// Caso 2: Promedio de secuencia vacía
val promedio2 = calculePromedioSeq(p1, Seq())
// Esperado: p1 (mediana vieja)

// Caso 3: Promedio de un solo punto
val promedio3 = calculePromedioSeq(p1, Seq(p2))
// Esperado: p2

// Caso 4: Promedio de muchos puntos
val promedio4 = calculePromedioSeq(p1, Seq(
  new Punto(0.0, 0.0),
  new Punto(2.0, 2.0),
  new Punto(4.0, 4.0)
))
// Esperado: Punto(2.0, 2.0)

// ==================== 5. PRUEBAS actualizarSeq ====================

// Caso 1: Actualizar con clasificación simple
val clasifActualizar = Map(
  p1 -> Seq(new Punto(1.0, 1.0), new Punto(2.0, 2.0)),
  p3 -> Seq(new Punto(8.0, 8.0), new Punto(9.0, 9.0))
)
val medianas2 = Seq(p1, p3)
val nuevasMedianas1 = actualizarSeq(clasifActualizar, medianas2)
// Esperado: Seq(Punto(1.5, 1.5), Punto(8.5, 8.5))

// Caso 2: Actualizar con cluster vacío
val clasifVacio = Map(p1 -> Seq())
val nuevasMedianas2 = actualizarSeq(clasifVacio, Seq(p1))
// Esperado: Seq(p1) - mantiene la mediana vieja


// Caso 3: Verificar que mantiene el orden
val medianas3 = Seq(p3, p1)  // Orden invertido
val nuevasMedianas3 = actualizarSeq(clasifActualizar, medianas3)
// Esperado: Primero promedio de p3, luego de p1

// ==================== 6. PRUEBAS actualizarPar ====================

// Caso 1: Comparar con versión secuencial
val nuevasMedianasPar1 = actualizarPar(clasifActualizar, medianas2)
// Esperado: Mismo resultado que actualizarSeq

// Caso 2: Con más medianas
val puntos16 = generarPuntos(4, 16)
val medianas4 = inicializarMedianas(4, puntos16)
val clasif16 = clasificarSeq(puntos16, medianas4)
val actualizadaSeq = actualizarSeq(clasif16, medianas4)
val actualizadaPar = actualizarPar(clasif16, medianas4)
// Esperado: Ambas iguales

// ==================== 7. PRUEBAS hayConvergenciaSeq ====================

// Caso 1: Medianas idénticas (convergió)
val conv1 = hayConvergenciaSeq(0.01, Seq(p1, p3), Seq(p1, p3))
// Esperado: true

// Caso 2: Medianas muy cercanas (convergió)
val conv2 = hayConvergenciaSeq(1.0, 
  Seq(new Punto(1.0, 1.0)), 
  Seq(new Punto(1.05, 1.05))
)
// Esperado: true

// Caso 3: Medianas lejanas (no convergió)
val conv3 = hayConvergenciaSeq(0.01, 
  Seq(new Punto(1.0, 1.0)), 
  Seq(new Punto(5.0, 5.0))
)
// Esperado: false

// Caso 4: Secuencias vacías
val conv4 = hayConvergenciaSeq(0.01, Seq(), Seq())
// Esperado: true

// Caso 5: Una converge, otra no
val conv5 = hayConvergenciaSeq(0.01,
  Seq(new Punto(1.0, 1.0), new Punto(8.0, 8.0)),
  Seq(new Punto(1.01, 1.01), new Punto(15.0, 15.0))
)
// Esperado: false

// ==================== 8. PRUEBAS hayConvergenciaPar ====================

// Caso 1: Comparar con versión secuencial - convergió
val convPar1 = hayConvergenciaPar(0.01, Seq(p1, p3), Seq(p1, p3))
// Esperado: true (igual que secuencial)

// Caso 2: Comparar con versión secuencial - no convergió
val convPar2 = hayConvergenciaPar(0.01,
  Seq(new Punto(1.0, 1.0), new Punto(8.0, 8.0)),
  Seq(new Punto(5.0, 5.0), new Punto(15.0, 15.0))
)
// Esperado: false

// Caso 3: Con muchas medianas
val medianas8 = generarPuntos(8, 8)
val medianas8Mod = medianas8.map(p => new Punto(p.x + 0.001, p.y + 0.001))
val convPar3 = hayConvergenciaPar(0.01, medianas8, medianas8Mod)
// Esperado: true (muy cercanas)

// ==================== 9. PRUEBAS generarPuntos ====================

// Caso 1: Generar 10 puntos para 2 clusters
val puntos10_2 = generarPuntos(2, 10)
// Esperado: Seq de 10 puntos

// Caso 2: Verificar longitud
val longitud1 = puntos10_2.length
// Esperado: 10

// Caso 3: Generar muchos puntos
val puntos1000 = generarPuntos(8, 1000)
val longitud2 = puntos1000.length
// Esperado: 1000

// ==================== 10. PRUEBAS inicializarMedianas ====================

// Caso 1: Inicializar 3 medianas de 16 puntos
val puntos16_init = generarPuntos(3, 16)
val medianas3_init = inicializarMedianas(3, puntos16_init)
// Esperado: Seq de 3 puntos

// Caso 2: Verificar longitud
val longitud3 = medianas3_init.length
// Esperado: 3

// Caso 3: Verificar que las medianas están en los puntos originales
val todasEnPuntos = medianas3_init.forall(m => puntos16_init.contains(m))
// Esperado: true

// =================== 11. PRUEBAS kMedianasSeq y kMedianasPar =====================

val eta = 0.01

// Caso 1: n=16, k=2
val puntos1 = generarPuntos(2, 16).toSeq
val meds1   = inicializarMedianas(2, puntos1)
val r1Seq   = kMedianasSeq(puntos1, meds1, eta)
val r1Par   = kMedianasPar(puntos1, meds1, eta)

// Caso 2: n=64, k=4
val puntos2 = generarPuntos(4, 64).toSeq
val meds2   = inicializarMedianas(4, puntos2)
val r2Seq   = kMedianasSeq(puntos2, meds2, eta)
val r2Par   = kMedianasPar(puntos2, meds2, eta)

// Caso 3: n=256, k=8
val puntos3 = generarPuntos(8, 256).toSeq
val meds3   = inicializarMedianas(8, puntos3)
val r3Seq   = kMedianasSeq(puntos3, meds3, eta)
val r3Par   = kMedianasPar(puntos3, meds3, eta)

// Caso 4: n=1024, k=4
val puntos4 = generarPuntos(4, 1024).toSeq
val meds4   = inicializarMedianas(4, puntos4)
val r4Seq   = kMedianasSeq(puntos4, meds4, eta)
val r4Par   = kMedianasPar(puntos4, meds4, eta)

// Caso 5: n=4096, k=16
val puntos5 = generarPuntos(16, 4096).toSeq
val meds5   = inicializarMedianas(16, puntos5)
val r5Seq   = kMedianasSeq(puntos5, meds5, eta)
val r5Par   = kMedianasPar(puntos5, meds5, eta)


// ============================================================================
// Script para probar el desempeño de kMedianasSeq y kMedianasPar
// ============================================================================
val eta = 0.01
val Ns = Seq(1024, 4096, 8192, 16384, 32768, 65536)
val Ks = Seq(2, 4, 8, 16, 128, 256)
val reps = 5

case class Row(n:Int, k:Int, tSeq:Double, tPar:Double, acc:Double)
def avg(xs: Seq[Double]) = xs.sum / xs.size

println("n,k,t_seq_ms,t_par_ms,acc")
for {
  n <- Ns
  k <- Ks if k < n
} {
  val rows = (1 to reps).map { _ =>
    val puntos = generarPuntos(k, n).toSeq
    val (tSeq, tPar, acc) = tiemposKmedianas(puntos, k, eta)
    Row(n,k,tSeq.value,tPar.value,acc)
  }
  val r = Row(
    n, k,
    avg(rows.map(r => r.tSeq)),
    avg(rows.map(r=> r.tPar)),
    avg(rows.map(r => r.acc))
  )
  println(f"${r.n},${r.k},${r.tSeq}%.2f,${r.tPar}%.2f,${r.acc}%.3f")
}