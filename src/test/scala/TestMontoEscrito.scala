
import utest._

object TestMontoEscrito extends TestSuite {

  val tests = this {
    'generaMontoEscritoUnidadEntera {
      val valor = 1
      assert("un peso" == MontoEscrito(valor, "peso", "centavo"))
      assert("un bolívar" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoUnidadDoble {
      val valor = 1.0
      assert("un peso" == MontoEscrito(valor, "peso", "centavo"))
      assert("un bolívar" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoEntreUnoYVeintinueveMonedaVocal {
      for (valor <- MontoEscrito.unidades.keySet if valor > 1)
        assert(s"${MontoEscrito.unidades(valor)} pesos" ==  MontoEscrito(valor, "peso", "centavo"))
    }
    'generaMontoEscritoEntreUnoYVeintinueveMonedaConsonante {
      for (valor <- MontoEscrito.unidades.keySet if valor > 1)
        assert(s"${MontoEscrito.unidades(valor)} bolívares" ==  MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoUnidadConDecimales {
      val valor = 1.23
      assert("un peso con veintitres centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("un bolívar con veintitres céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoConDecimales {
      val valor = 12.34
      assert("doce pesos con treinta y cuatro centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("doce bolívares con treinta y cuatro céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoConDecimalSingular {
      val valor = 12.01
      assert("doce pesos con un centavo" == MontoEscrito(valor, "peso", "centavo"))
      assert("doce bolívares con un céntimo" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoConDecimalSingular {
      val valor = 12.1
      assert("doce pesos con diez centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("doce bolívares con diez céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoCien {
      val valor = 100d
      assert("cien pesos" == MontoEscrito(valor, "peso", "centavo"))
      assert("cien bolívares" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoCienConDecimales {
      val valor = 100.69
      assert("cien pesos con sesenta y nueve centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("cien bolívares con sesenta y nueve céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoCentenas {
      val valor = 256.78
      assert("doscientos cincuenta y seis pesos con setenta y ocho centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("doscientos cincuenta y seis bolívares con setenta y ocho céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoMil {
      val valor = 1000d
      assert("mil pesos" == MontoEscrito(valor, "peso", "centavo"))
      assert("mil bolívares" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoMiles {
      val valor = 1001.12
      assert("mil un pesos con doce centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("mil un bolívares con doce céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoMillon {
      val valor = 1000000d
      assert("un millón de pesos" == MontoEscrito(valor, "peso", "centavo"))
      assert("un millón de bolívares" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoMillones {
      val valor = 7654321.99d
      assert("siete millones seiscientos cincuenta y cuatro mil trescientos veintiún pesos con noventa y nueve centavos" ==
        MontoEscrito(valor, "peso", "centavo"))
      assert("siete millones seiscientos cincuenta y cuatro mil trescientos veintiún bolívares con noventa y nueve céntimos" ==
        MontoEscrito(valor, "bolívar", "céntimo"))
    }
    'generaMontoEscritoMaximoValor {
      val valor = 999999999.99
      assert("novecientos noventa y nueve millones novecientos noventa y nueve mil novecientos noventa y nueve pesos con noventa y nueve centavos" == MontoEscrito(valor, "peso", "centavo"))
      assert("novecientos noventa y nueve millones novecientos noventa y nueve mil novecientos noventa y nueve bolívares con noventa y nueve céntimos" == MontoEscrito(valor, "bolívar", "céntimo"))
    }
  }
}
