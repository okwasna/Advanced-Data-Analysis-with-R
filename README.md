
# Projekt Analizy Statystycznej w R

## Opis
Ten projekt R zawiera skrypty do przeprowadzania zaawansowanej analizy statystycznej danych. Skrypt wykonuje następujące zadania:
- Wczytywanie i przetwarzanie danych.
- Obsługa brakujących wartości.
- Wyszukiwanie wartości odstających.
- Generowanie wykresów boxplot.
- Podsumowanie statystyczne różnych grup.
- Testy statystyczne, w tym test Shapiro-Wilka, test Levene'a, test t-Studenta i test ANOVA.
- Analiza korelacji między zmiennymi.

## Wymagane Pakiety
Przed uruchomieniem skryptu, upewnij się, że zainstalowane są następujące pakiety R:

```R
install.packages("car")
install.packages("lawstat")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("multcomp")
install.packages("dplyr")
# install.packages("dunn.test") # Odkomentuj, jeśli potrzebny
```

## Ładowanie Pakietów
Po zainstalowaniu wymaganych pakietów, załaduj je, używając poniższego kodu:

```R
library(multcomp)
library(dplyr)
library(car)
library(lawstat)
library(ggplot2)
library(ggpubr)
# library(dunn.test) # Odkomentuj, jeśli używany
```

## Uruchamianie Skryptu
1. Upewnij się, że wszystkie wymagane pakiety są zainstalowane i załadowane.
2. Zmień ścieżkę pliku CSV w skrypcie, aby odpowiadała lokalizacji Twoich danych.
3. Uruchom skrypt w środowisku RStudio.

## Uwagi
- Skrypt zakłada, że dane są w formacie CSV z separatorami `;` i znakiem dziesiętnym `,`.
- W przypadku brakujących danych skrypt zastępuje braki medianą dla danej kolumny.


## Analiza Danych
Skrypt zawiera kilka funkcji do analizy danych:

### Obsługa Brakujących Wartości
- Wypełnienie brakujących wartości medianą.
- Wyświetlanie informacji o zmienionych brakach.

### Wyszukiwanie Wartości Odstających
- Identyfikacja i wyświetlanie wartości odstających w danych.
- Tworzenie wykresów boxplot z zaznaczonymi wartościami odstającymi.

### Podsumowanie Grupowe
- Podsumowanie statystyczne różnych grup, w tym liczba obserwacji, średnia, odchylenie standardowe, mediana.

### Testy Statystyczne
- Przeprowadzanie różnych testów statystycznych (Shapiro-Wilka, Levene'a, t-Studenta, ANOVA) w celu sprawdzenia rozkładów i równości wariancji.

### Analiza Korelacji
- Obliczanie i wyświetlanie korelacji między zmiennymi.
- Tworzenie wizualizacji macierzy korelacji.

### Wizualizacja Danych
- Tworzenie wykresów gęstości dla różnych grup.
- Generowanie wykresów korelacji między zmiennymi.

## Instrukcje Uruchamiania
1. Umieść skrypt w wybranym folderze.
2. Upewnij się, że plik danych CSV znajduje się w odpowiedniej lokalizacji i ma poprawny format.
3. Otwórz skrypt w RStudio i uruchom go.
4. Wyniki analizy, w tym wykresy, zostaną zapisane w określonych lokalizacjach.

## Wskazówki
- Przed uruchomieniem skryptu zaleca się sprawdzenie poprawności ścieżek do plików danych i wynikowych.
- Aby uzyskać najlepsze wyniki, upewnij się, że dane wejściowe są poprawnie sformatowane i czyste.

