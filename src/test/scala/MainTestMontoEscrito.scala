
object MainTestMontoEscrito {

  def main(args: Array[String]): Unit = {

    val monedas = Seq(
      ("peso", "centavo"),
      ("bolívar", "céntimo")
    )

    val valores = Seq(1, 1.23, 12.34, 30, 30.44, 99.99, 100, 101.23, 512.56, 1000, 1001.12, 100000, 1000000, 9999999.99)

    val resultadosEsperados = Seq(
      (1.0, "un peso"),
      (1.23, "un peso con veintitres centavos"),
      (12.34, "doce pesos con treinta y cuatro centavos"),
      (30.0, "treinta pesos"),
      (30.44, "treinta pesos con cuarenta y cuatro centavos"),
      (99.99, "noventa y nueve pesos con noventa y nueve centavos"),
      (100.0, "cien pesos"),
      (101.23, "ciento un pesos con veintitres centavos"),
      (512.56, "quinientos doce pesos con cincuenta y seis centavos"),
      (1000.0, "mil pesos"),
      (1001.12, "mil un pesos con doce centavos"),
      (100000.0, "cien mil pesos"),
      (1000000.0, "un millón de pesos"),
      (9999999.99, "nueve millones novecientos noventa y nueve mil novecientos noventa y nueve pesos con noventa y nueve centavos"),
      (1.0, "un bolívar"),
      (1.23, "un bolívar con veintitres céntimos"),
      (12.34, "doce bolívares con treinta y cuatro céntimos"),
      (30.0, "treinta bolívares"),
      (30.44, "treinta bolívares con cuarenta y cuatro céntimos"),
      (99.99, "noventa y nueve bolívares con noventa y nueve céntimos"),
      (100.0, "cien bolívares"),
      (101.23, "ciento un bolívares con veintitres céntimos"),
      (512.56, "quinientos doce bolívares con cincuenta y seis céntimos"),
      (1000.0, "mil bolívares"),
      (1001.12, "mil un bolívares con doce céntimos"),
      (100000.0, "cien mil bolívares"),
      (1000000.0, "un millón de bolívares"),
      (9999999.99, "nueve millones novecientos noventa y nueve mil novecientos noventa y nueve bolívares con noventa y nueve céntimos")
    )

    // Desplegar resultados esperados
    resultadosEsperados.foreach { case(valorEsperado, montoEscritoEsperado) =>
      println(s"$valorEsperado: $montoEscritoEsperado")
    }

    // Calcular montos escritos invocando objeto "MontoEscrito"
    val resultadosObtenidos = for ((moneda, centimo) <- monedas; valor <- valores)
      yield MontoEscrito(valor, moneda, centimo)

    // Comparar todos los esperados con todos los obtenidos. Deben ser iguales
    val resultadosIguales =
      resultadosEsperados
        // Empareja resultados esperados con montos escritos obtenidos
        .zip(resultadosObtenidos)
        // Se debe cumplir que para cada pareja esperado/obtenido...
        .forall { case((valorEsperado, montoEscritoEsperado), montoEscritoObtenido) =>
        // .. los montos escritos obtenidos sean iguales a los esperados
        montoEscritoEsperado == montoEscritoObtenido
      }

    assert(resultadosIguales, "Diferencias entre los montos escritos esperados y los obtenidos, fyah")
    println("Victoria! Resultados obtenidos iguales a los esperados, PTL, PTL, PTL!")
  }
}
