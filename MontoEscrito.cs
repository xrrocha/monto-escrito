 class Dictionary
  {

    static void Main()
    {
      string entrada;
      int numero;
      bool esNumero;

      Console.WriteLine("\nDigite '0' para terminar la ejecución\n");

      do

      {
        Console.Write("Digite número : ");
        entrada = Console.ReadLine();
        esNumero = int.TryParse(entrada, out numero);
        if (!esNumero)
          Console.WriteLine("\n  No es un número, intente de nuevo\n");
        else
          Console.WriteLine("\n  {0}\n", MontoEscrito(numero));
      } while (!(esNumero && numero == 0));
      Console.WriteLine("\nPrograma finalizado");
    }

    public static string Unidades(int numero)
    {
      var keys = Enumerable.Range(1, 99);
      var values = new List<string> { "UN ", "DOS ", "TRES ", "CUATRO ", "CINCO ", "SEIS ", "SIETE ", "OCHO ", "NUEVE " , "DIEZ ", "ONCE ",
                      "DOCE ", "TRECE ", "CATORCE ", "QUINCE ", "DIECISEIS ", "DIECISIETE ", "DIECIOCHO ", "DIECINUEVE ", "VEINTE ",
                 "VEINTIUN ","VEINTIDOS ","VEINTITRES ","VEINTICUATRO ","VEINTICINCO ","VEINTISEIS ","VEINTISIETE ","VEINTIOCHO ","VEINTINUEVE "};


      var d = keys.Zip(values, (k, v) => new { Key = k, Value = v })
                 .ToDictionary(x => x.Key, x => x.Value);

      List<string> valueList = new List<string>();
      valueList.Add("TREINTA ");
      valueList.Add("CUARENTA ");
      valueList.Add("CINCUENTA ");
      valueList.Add("SESENTA ");
      valueList.Add("SETENTA ");
      valueList.Add("OCHENTA ");
      valueList.Add("NOVENTA ");

      for (int i = 1; i <= valueList.Count; i++)
      {

        d[(10 * (i + 2))] = valueList[i - 1];
      }

      for (int i = 1; i <= valueList.Count; i++)
      {
        int h = (10 * (i + 2));
        for (int j = 1; j < 10; j++)
        {
          d[h + j] = valueList[i - 1] + "Y " + d.Where(v => v.Key == j).FirstOrDefault().Value;
        }

      }

      return d.Where(v => v.Key == numero).FirstOrDefault().Value;
    }

    public static string Centenas(int numero)
    {
      var values = new List<string> { "CIENTO ", "DOSCIENTOS ", "TRESCIENTOS ", "CUATROCIENTOS ", "QUINIENTOS ", "SEISCIENTOS ", "SETECIENTOS ", "OCHOCIENTOS ", "NOVECIENTOS " };
      var keys = Enumerable.Range(1, values.Count);

      var d = keys.Zip(values, (k, v) => new { Key = k, Value = v })
                 .ToDictionary(x => x.Key, x => x.Value);
      return d.FirstOrDefault(v => v.Key == numero).Value;
    }
    public static string Miles(int numero)
    {
      int valor = 0;
      int cientos = 0;
      int unidades = 0;
      string texto = "";

      valor = numero;
      cientos = valor / 100;
      valor = (valor - (100 * cientos));
      unidades = valor;

      if (cientos > 0)
        texto = texto + Centenas(cientos);
      if (unidades > 0)
        texto = texto + Unidades(unidades);
      texto = texto + "MIL ";
      return texto;
    }
    public static string Millones(int numero)
    {
      int valor = 0;
      int miles = 0;
      int cientos = 0;
      int unidades = 0;
      string texto = "";

      valor = numero;
      miles = valor / 1000;
      valor = (valor - (1000 * miles));
      cientos = valor / 100;
      valor = (valor - (100 * cientos));
      unidades = valor;


      if (miles > 0)
        texto = texto + Miles(miles);
      if (cientos > 0)
        texto = texto + Centenas(cientos);
      if (unidades > 0)
        texto = texto + Unidades(unidades);
      texto = texto + "MILLON";
      if (miles > 0 || cientos > 0 || unidades > 1)
        texto = texto + "ES ";
      else
        texto = texto + " ";
      return texto;
    }
    public static string MontoEscrito(int numero)
    {
      int valor = 0;
      int miles = 0;
      int millones = 0;
      int cientos = 0;
      int unidades = 0;
      string texto = "";

      valor = numero;
      millones = valor / 1000000;
      valor = (valor - (1000000 * millones));
      miles = valor / 1000;
      valor = (valor - (1000 * miles));
      cientos = valor / 100;
      valor = (valor - (100 * cientos));
      unidades = valor;

      if (millones > 0)
        texto = texto + Millones(millones);
      if (miles > 0)
        texto = texto + Miles(miles);
      if (cientos > 0)
        texto = texto + Centenas(cientos);
      if (unidades > 0)
        texto = texto + Unidades(unidades);

      return texto;
    }
  }
