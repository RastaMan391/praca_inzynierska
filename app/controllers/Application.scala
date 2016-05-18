package controllers

import play.api._
import play.api.mvc._
import java.util._
import java.lang._
import java.io._
import scala.io.Source
import scala.runtime.ScalaRunTime._
import scala.Predef._

class Application extends Controller {

  // Określenie źródła pliku
  val source = Source.fromFile("C:\\activator-1.3.6-minimal\\pracainzynierska\\app\\piotr_1504_Ch1_1.txt", "UTF-8")
  val lineIterator = source.getLines
  val lines = source.getLines.toArray
  var rozmiar_danych = 0
  //Określenie rodzaju pliku poprzez pierwszy znak w 6 wierszu
  val rodzaj_pliku: String = lines(5) //pobranie całej linii 5
  val rodzaj_pliku1 = rodzaj_pliku(0) //zapisany pierwszy znak
  //jeżeli pierwszym znakiem jest 0 to jest to plik typu piotr_...
  //
  //Stworzenie zmiennej warunek zawierająca warunek wykonania ifa
  val warunek: String = rodzaj_pliku1.toString
  if (warunek == "0") rozmiar_danych = 5 else rozmiar_danych = 12 // wartości rozmiaru danych zależą od tego, że w jednym pliku dane rozpoczynają się w 6 linijce, a w drugim typie pliku w 13
  //zlokalizowanie ilości kolumn danych na podstawie tabów
  var zmienna = 0
  if(rozmiar_danych == 5) {
    //plik 0909123091209301293 z log
    zmienna += 1 // w tym wypadku powiększam o 1, aby wziąć pod uwagę dodatkową zmienną czas
    for (x <- 1 to rodzaj_pliku.length()){
      var pomocnicza = rodzaj_pliku(x-1).toChar
      if(pomocnicza == 9) zmienna+=1
    }
  } else {
    //plik piotr...
    val dane = lines(12)
    for (x <- 1 to dane.length()){
      var pomocnicza = dane(x-1).toChar
      if(pomocnicza == 9) zmienna += 1
    }
    zmienna = zmienna/2 // dziele przez 2, gdyż w pliku znajdują się podwójne wartości
    zmienna += 1
  }
  //tworze tablicę zawierającą wszystkie dane w kolumnach
  val dane = Array.ofDim[Double](zmienna, lines.length-rozmiar_danych)
  //stworzenie pliku przechowującego dane
  val dane_plik = new PrintWriter("public\\javascripts\\dane.csv")
  //
  //zmienna przechowująca czas początkowy tylko w przypadku drugiego rodzaju pliku
  //
  var czas_poczatkowy: Double = 0.0
  //
  //Główna pętla programu
  //
  for (x <- 1 to lines.length-rozmiar_danych){
    //zmienna przechowująca położenie przeglądania danych
    var polozenie = 1 //polozenie wynosi 1, gdyż używam go w forze, a później przy odczytywaniu wartości z tablicy wartość jest o 1 pomniejszana
    var polozenie_tablica = Array.ofDim[Int](zmienna+10) //tablica, do której będę wprowadzał znalezione polozenia
    //przy pobieraniu danych położenie będzie dodatkowo pomniejszane o 2, gdyż bity do pobrania z tablicy zaczynają numerację od 0
    //If sprawdzający, który z rodzajów plików jest przekazany do przetworzenia
    if (warunek == "0"){
      //Odczyt danych dla pliku 029183091283902810 log
      var wartosc: String = lines(x+4)//odczyt aktualnej linii danych
      var zmienna_tablica = 1 //zmienna potrzebna do wpisywania polozen w tablicy
      //petla przelatująca przez cały wiersz i zapisująca położenia TABów
      for (y <- polozenie to wartosc.length()){
        var pomocnicza_1 = wartosc(y-1).toChar
        if(pomocnicza_1 == 9){
          polozenie_tablica(zmienna_tablica-1) = y-2
          zmienna_tablica += 1
        }
      }
      //zmienne pomocnicze potrzzebne do lokalizowania pomiarów
      var zmienna_pomocnicza_1 = 0 // indeks dla polozenie_tablica
      //pętla odczytująca wartości dla całego wiersza i zapisująca je do tablicy z danymi
      for (i <- 1 to zmienna){
        var rozmiar = 0 //zmienna przechowująca rozmiar tablicy potrzebnej do przechowania danej
        if (i < zmienna) {
          //określenie rozmiarów pojedynczej danej w celu stworzenia tablicy
          if(i == 1) rozmiar = polozenie_tablica(zmienna_pomocnicza_1) else rozmiar = polozenie_tablica(zmienna_pomocnicza_1)-(polozenie_tablica(zmienna_pomocnicza_1-1)+2)
          zmienna_pomocnicza_1 += 1
        } else{
          rozmiar = wartosc.length() - (polozenie_tablica(zmienna_pomocnicza_1-1)+3)
        }
        var tablica = new Array[Char](rozmiar+1)//tablica przechowująca pojedynczą daną
        for (j <- 1 to rozmiar+1){
          //rozpisane przypadki pobierania danych
          if (i == 1) tablica(j-1) = wartosc(j-1)//pierwsza wartość pobierana jest z początku
          //wszystkie środkowe wartości pobierane są na podstawie danych z położenia tabów
          if (i > 1 && i < zmienna) tablica(j-1) = wartosc(polozenie_tablica(zmienna_pomocnicza_1-2)+j+1)
          //ostatnia wartość jest pobierana na podstawie ostatniego położenia taba
          if (i == zmienna) tablica(j-1) = wartosc(polozenie_tablica(zmienna_pomocnicza_1-1)+j+1)
        }
        var tablica_1 = tablica.mkString("")//zmieniam dane z tablicy w stringa
        var tablica_2 = new Array[Char](rozmiar+1)//druga tablica służąca do przechowania danych  
        //pętla zamieniająca przecinki w kropki i zapisująca do nowej tablicy
        for ( k <- 1 to rozmiar+1){
          if (tablica_1(k-1) == 44) tablica_2(k-1)=46 else tablica_2(k-1)=tablica_1(k-1)
        }
        var tablica_3 = tablica_2.mkString("")//zapisanie do stringa danych z kropkami
        var tablica_4 = tablica_3.toDouble//przerobienie danych na typ double
        dane(i-1)(x-1) = tablica_4//wprowadzenie danych do głównej tablicy
      }
      //łączenie wszystkich danych w jednego stringa
      var string_danych_1 = new Array[String](zmienna)
      for (i <- 1 to zmienna){
        string_danych_1(i-1)=dane(i-1)(x-1).toString
      }
      var string_danych = string_danych_1.mkString(",")
      //zapis danych do pliku
      dane_plik.println(string_danych)
    } else {
      var wartosc: String = lines(x+11)//odczyt aktualnej linii danych
      //zmienna przechowująca polożenie godziny
      var polozenie_1 = 1//położenie początku godziny
      var polozenie_2 = 1//położenie początku pierwszej danej
      //lokalizacja godziny i pierwszego pomiaru, lokalizacja pierwszego pomiaru zostanie wykorzystana dalej do lokalizacji następnych
      for (i <- 1 to wartosc.length()){
        var pomocnicza_1 = wartosc(i-1).toChar
        var pomocnicza_2 = wartosc(i-1).toChar
        if (pomocnicza_1 == 32) polozenie_1 = i//lokalizacja pierwszej spacji
        if (pomocnicza_2 == 9 && i <28) polozenie_2 = i//lokalizacja pierwszego taba
      }
      //
      //przeliczanie czasu na względny
      //
      //definicja zmiennych, które będą przechowywać poszczególne części czasu oraz ich polozenie
      var godziny = 0
      var godziny_string: String = ""
      var polozenie_godziny_1 = polozenie_1
      var polozenie_godziny_2 = 0
      var minuty = 0
      var minuty_string: String = ""
      var polozenie_minuty_1 = 0
      var polozenie_minuty_2 = 0
      var sekundy = 0
      var sekundy_string: String = ""
      var polozenie_sekundy_1 = 0
      var polozenie_sekundy_2 = 0
      var milisekundy = 0
      var milisekundy_string: String = ""
      var polozenie_milisekundy_1 = 0
      var polozenie_milisekundy_2 = 0
      //pętla lokalizująca i wydzielająca położenie godziny
      for(i <- 1 to polozenie_2-1-polozenie_1){
        if(wartosc(polozenie_1+i-1).toChar == 58 && i < 4) polozenie_godziny_2 = polozenie_1 + i - 2
      }
      //położenie minut, sekund i milisekund na podstawie położenia godziny
      polozenie_minuty_1 = polozenie_godziny_2 + 2
      polozenie_minuty_2 = polozenie_minuty_1 + 1
      polozenie_sekundy_1 = polozenie_minuty_2 + 2
      polozenie_sekundy_2 = polozenie_sekundy_1 + 1
      polozenie_milisekundy_1 = polozenie_sekundy_2 + 2
      polozenie_milisekundy_2 = polozenie_milisekundy_1 + 2
      //policzenie godziny
      var czas_pomocniczy_1 = new Array[Char](polozenie_godziny_2 - polozenie_godziny_1 + 1)
      for(i <- 1 to polozenie_godziny_2 - polozenie_godziny_1 + 1){
        czas_pomocniczy_1(i-1) = wartosc(polozenie_godziny_1 + i - 1)
      }
      godziny_string = czas_pomocniczy_1.mkString("")
      godziny = godziny_string.toInt
      //policzenie minuty
      var czas_pomocniczy_2 = new Array[Char](polozenie_minuty_2 - polozenie_minuty_1 + 1)
      for(i <- 1 to polozenie_minuty_2 - polozenie_minuty_1 + 1){
        czas_pomocniczy_2(i-1) = wartosc(polozenie_minuty_1 + i - 1)
      }
      minuty_string = czas_pomocniczy_2.mkString("")
      minuty = minuty_string.toInt
      //policzenie sekundy
      var czas_pomocniczy_3 = new Array[Char](polozenie_sekundy_2 - polozenie_sekundy_1 + 1)
      for(i <- 1 to polozenie_sekundy_2 - polozenie_sekundy_1 + 1){
        czas_pomocniczy_3(i-1) = wartosc(polozenie_sekundy_1 + i - 1)
      }
      sekundy_string = czas_pomocniczy_3.mkString("")
      sekundy = sekundy_string.toInt
      //policzenie milisekundy
      var czas_pomocniczy_4 = new Array[Char](polozenie_milisekundy_2 - polozenie_milisekundy_1 + 1)
      for(i <- 1 to polozenie_milisekundy_2 - polozenie_milisekundy_1 + 1){
        czas_pomocniczy_4(i-1) = wartosc(polozenie_milisekundy_1 + i - 1)
      }
      milisekundy_string = czas_pomocniczy_4.mkString("")
      milisekundy = milisekundy_string.toInt
      //przeliczenie całej godziny na milisekundy
      var czas_wzgledny_1 = 3600000*godziny+60000*minuty+1000*sekundy+milisekundy
      var czas_wzgledny_2 = czas_wzgledny_1.toDouble//zmiana formatu na double
      var czas_wzgledny = czas_wzgledny_2/1000.0
      if(x==1) czas_poczatkowy = czas_wzgledny//przy pierwszym przejściu pętli zapisywany jest czas początkowy w celu obliczenia czasu względnego
      var czas = czas_wzgledny - czas_poczatkowy//właściwa wartość upłyniętego czasu
      dane(0)(x-1) = czas
      var zmienna_tablica = 1//zmienna potrzebna do wpisywania polozen w tablicy
      //pętla lokalizująca taby w pliku, wszystkie i zapisująca ich położenie
      for(i <- 1 to wartosc.length()){
        if(wartosc(i-1).toChar == 9){
          polozenie_tablica(zmienna_tablica-1) = i
          zmienna_tablica += 1
        }
      }
      //zmienne pomocnicze
      var poczatek_pomiaru = 0
      var koniec_pomiaru = 0
      var zmienna_pomocnicza = 0//zmienna, którą posługujemy się do odczytania miejsc tabów
      for (i <- 1 to zmienna-1){
        poczatek_pomiaru = polozenie_tablica(zmienna_pomocnicza)
        koniec_pomiaru = polozenie_tablica(zmienna_pomocnicza+1)
        var rozmiar = koniec_pomiaru-poczatek_pomiaru//zmienna do określenia rozmiaru tablicy
        var tablica = new Array[Char](rozmiar-1)//tablica przechowująca pojedynczy pomiar
        //pętla zapisująca pojedynczy pomiar do tablicy
        for (j <- 1 to rozmiar-1){
          tablica(j-1) = wartosc(poczatek_pomiaru+j-1)
        }
        zmienna_pomocnicza += 2//zwiększamy o 2, gdyż chcemy przeskoczyć niepotrzebną wartość
        if (i == zmienna-1) zmienna_pomocnicza = 0//zerowanie jest potrzebne przy przejściu do następnej wartości
        //konwersja do double z chara poprzez string
        var tablica_1 = tablica.mkString
        var tablica_2 = tablica_1.toDouble
        dane(i)(x-1) = tablica_2//zapis danych do tablicy, rozpoczynami od i a nie od i - 1 gdyż na pierwszym miejscu jest czas, który został określony wcześniej
      }
      //łączenie wszystkich danych w jednego stringa
      var string_danych_1 = new Array[String](zmienna)
      for (i <- 1 to zmienna){
        string_danych_1(i-1)=dane(i-1)(x-1).toString
      }
      var string_danych = string_danych_1.mkString(",")
      //zapis danych do pliku
      dane_plik.println(string_danych)
    }
  }
  dane_plik.close()//zamykamy plik
  def index = Action {
      Ok(views.html.index("Wykonał Patryk Rudnicki"))

  }

}
