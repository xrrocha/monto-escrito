
/**
  * Monto escrito en español.
  */
object MontoEscrito {

  // método apply invocado con nombre de objeto como en:
  //     MontoEscrito(123.45, "dólar", "centavo")
  def apply(valor: Double, moneda: String, centesimo: String): String = {
    require(valor <= 999999999.99)

    // Separa valor entero de fraccionario
    val valorEntero = valor.toInt
    val valorFraccionario = (100 * (valor - valorEntero)).round.toInt // Toma solo dos dígitos fraccionarios

    // Determina monto escrito de parte entera
    val montoEscritoEntero = apply(valorEntero)

    // Pluraliza y añade moneda a parte entera
    val montoEscritoEnteroMoneda =
      s"$montoEscritoEntero${plural(valorEntero, moneda)}"

    // Añade parte fraccionaria (si es diferente de cero) con indicativo centesimal especificado
    val montoEscritoConFraccion =
      if (valorFraccionario == 0) {
        s"$montoEscritoEnteroMoneda"
      } else {
        s"$montoEscritoEnteroMoneda con ${montoCentenas(valorFraccionario)} ${plural(valorFraccionario, centesimo)}"
      }

    // Retorna monto escrito completo
    montoEscritoConFraccion
  }

  // Monto escrito de parte entera
  def apply(valor: Int): String = {
    require(valor <= 999999999.99) // Funciona hasta 999,999,999.99

    // Serie infinita que comienza con tupla (valor, lista vacía)
    // Colecta (en orden inverso) todos los grupos de 3 o menos dígitos
    // Ejm: el valor 12,427,892 genera sucesivamente:
    //     (12427, Seq(892)),
    //     (12, Seq(892, 427)),
    //     (0, Seq(892, 427, 12))
    // Solo nos interesa la tupla final, cuando el remanente se hace cero
    val montoEscrito =
    Stream.iterate((valor, Seq[Int]())) { case (remanente, grupos) =>
      val siguienteGrupo = remanente % 1000
      val siguienteRemanente = remanente / 1000
      (siguienteRemanente, grupos :+ siguienteGrupo)
    }
      // Se detiene tan pronto el primer elemento de la tupla (el remanente) se hace cero
      // Resultado: Some((0, Seq(892, 427, 12)))
      .find(_._1 == 0) // find { case(remanente, grupos) => remanente == 0 }
      // Extrae solo la colección de grupos de hasta 3 cifras (ignorando el remanente cero)
      // Resultado: Some(Seq(892, 427, 12))
      .map(_._2) // map { case(remanente, grupos) => grupos }
      // Remueve envoltura Some(...) retornada por find() (o secuencia vacía si es None)
      // Resultado: Seq(892, 427, 12)
      .getOrElse(Seq.empty)
      // Asocia cada grupo de 3 dígitos con su función sufijo correspondiente:
      // Resultado: Seq((892, textoUnidades), (427, textoMiles), (12, textoMillones))
      .zip(sufijosMedida)
      // Elimina de consideración todo grupo de 3 dígitos cuyo valor sea cero
      .filter { case (valorNumerico, sufijo) => valorNumerico > 0 }
      // Reemplaza cada grupo de 3 dígitos con su nombre en centenas y su sufijo:
      // Seq(("ochocientos noventa y dos", ""), ("cuatrocientos veintisiete", "mil"), ("doce", "millones"))
      .map { case (valorNumerico, sufijo) =>
      s"${montoCentenas(valorNumerico)} ${sufijo(valorNumerico)}"
    }
      // Reversa grupos de 3 dígitos para presentar en el orden esperado
      // Resultado: Seq(("doce", "millones"), ("cuatrocientos veintisiete", "mil"), ("ochocientos noventa y dos", ""))
      .reverse
      // Convierte colección de grupos a string delimitado con ' '
      // Resultado: "doce millones cuatrocientos veintisiete mil ochocientos noventa y dos"
      .mkString(" ")

    // Descarados ajustes para compensar casos especiales
    val ajustes = Seq(
      ("\\s+", " "), // Colapsa múltiples blancos en uno solo
      ("^un mil", "mil"), // Transforma "un mil dólares" en simplemente "mil dólares"
      ("^millón ", "un millón de ")) // Transforma "millón pesos" en "un millón de pesos"

    // Retorna resultado de aplicar descarados trucos para compensar casos especiales
    ajustes.foldLeft(montoEscrito) { (texto, ajuste) =>
      val (regex, reemplazo) = ajuste
      texto.replaceAll(regex, reemplazo)
    }
  }

  // "Verdadero" monto escrito que opera sobre grupo de hasta 3 dígitos
  def montoCentenas(valor: Int): String = {

    require(valor > 0 && valor < 1000, "Valor no comprendido entre 1 inclusive y 1000 exclusive")

    valor match {
      // Caso #1: Valor menor que 30; buscar en mapa de unidades
      case v if v < 30 =>
        unidades(v)
      // Caso #2: Valor mayor que 30 e inferior a 100; buscar en mapa de decenas y concatenar recursivamente el remanente
      case v if v < 100 =>
        val valorDecenas = v / 10
        val textoDecenas = decenas(valorDecenas - 2) // Indice - 2 porque mapa empieza con "treinta"
        val remanente = v - 10 * valorDecenas
          if (remanente == 0) {
            textoDecenas
          } else {
            s"$textoDecenas y ${montoCentenas(remanente)}" // Llamado recursivo para remanente
          }
      // Caso #3: Valor mayor que 100 e inferior a 1000; buscar en mapa de centenas y concatenar recursivamente el remanente
      case v =>
        val valorCentenas = v / 100
        val textoCentenas = centenas(valorCentenas)
        val remanente = v - 100 * valorCentenas
        if (remanente == 0) {
          if (valorCentenas == 1) { // "cien" para valor 1 y "ciento" para valores mayores que 1
            "cien"
          } else {
            textoCentenas
          }
        } else {
          s"$textoCentenas ${montoCentenas(remanente)}" // Llamado recursivo para remanente
        }
    }
  }

  // Pluralizar dependiendo de última letra
  val vocales = Set('a', 'e', 'i', 'o', 'u')
  def esVocal(chr: Char) = vocales.contains(chr)
  def plural(valor: Int, termino: String) =
    valor match {
      case 1 => termino
      case _ if esVocal(termino.last) => s"${termino}s"
      case _ => s"${termino}es"
    }

  // Colección de funciones que retornan medida a partir de valor
  val sufijosMedida: Seq[Int => String] =
    Seq(textoUnidades, textoMiles, textoMillones)

  // Métodos colectados como funciones para añadir sufijo numeral
  def textoUnidades(valor: Int) = "" // Nada para valores entre 1 y 999
  def textoMiles(valor: Int) = "mil " // "mil" para valores entre 1000 y 999,999
  def textoMillones(valor: Int) = // "millón" para uno y "millones" para valores iguales o mayores que 1
    if (valor == 1) "millón " else "millones "

  // Truco para convertir secuencias en mapas con índice entero base 1
  implicit def seq2map[A](as: Seq[A]): Map[Int, A] =
    as.zipWithIndex.map { case (text, index) => index + 1 -> text }.toMap

  // Componentes de conversión de cifra a texto para centena, decena y unidad
  // Usa conversión implícita seq2map para construir mapa a partir de secuencia
  val centenas: Map[Int, String] = Seq(
    "ciento", "doscientos", "trescientos", "cuatrocientos", "quinientos",
    "seiscientos", "setecientos", "ochocientos", "novecientos"
  )
  val decenas: Map[Int, String] = Seq(
    "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta",
    "noventa"
  )
  val unidades: Map[Int, String] = Seq(
    "un", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho",
    "nueve", "diez", "once", "doce", "trece", "catorce", "quince",
    "dieciseis", "diecisiete", "dieciocho", "diecinueve", "veinte",
    "veintiún", "veintidos", "veintitres", "veinticuatro",
    "veinticinco", "veintiseis", "veintisiete", "veintiocho", "veintinueve"
  )

}
