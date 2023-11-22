install.packages("car")
install.packages("lawstat")
install.packages("ggplot2")
#install.packages("dunn.test")
install.packages("ggpubr")
install.packages("multcomp")
library(multcomp)
library(dplyr)
library(car)
library(lawstat)
library(ggplot2)
#library(dunn.test)
library (ggpubr)
library(ggplot2)

#1
dane <- read.csv("/Users/home/Desktop/folder bez nazwy/przykladoweDane-Projekt.csv", sep=";", dec=",")

zmienione_braki <- list()

for (kolumna in names(dane)) {
  if (is.numeric(dane[[kolumna]]) && any(is.na(dane[[kolumna]]))) {
    cat("Kolumna", kolumna, "ma braki numeryczne.\n")
    braki <- is.na(dane[[kolumna]])
    mediana <- median(dane[[kolumna]], na.rm=TRUE)
    dane[[kolumna]][braki] <- mediana
    zmienione_braki[[kolumna]] <- data.frame(Rząd = which(braki), Stara_Wartość = NA, Nowa_Wartość = mediana)
  }
}
for (kolumna in names(zmienione_braki)) {
  cat("Zmienione braki w kolumnie\n", kolumna, ":\n")
  print(zmienione_braki[[kolumna]])
}
find_outliers <- function(column) {
  q1 <- quantile(column, 0.25, na.rm=TRUE)
  q3 <- quantile(column, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  lower_threshold <- q1 - 1.5 * iqr
  upper_threshold <- q3 + 1.5 * iqr
  
  outliers <- column[column < lower_threshold | column > upper_threshold]
  
  return(outliers)
}
for (kolumna in names(dane)) {
  if (is.numeric(dane[[kolumna]])) {
    outliers <- find_outliers(dane[[kolumna]])
    
    if (length(outliers) > 0) {
      cat("Wartości odstające w kolumnie", kolumna, ":\n")
      print(outliers)
    }
  }
}

for (kolumna in names(dane)) {
  if (is.numeric(dane[[kolumna]])) {
    wykres <- ggplot(dane, aes(x = "", y = dane[[kolumna]], fill = dane$grupa)) +
      geom_boxplot(outlier.shape = NA) +
      #geom_jitter(aes(color = dane$grupa), width = 0.2) +
      geom_text(aes(label = ifelse(dane[[kolumna]] > quantile(dane[[kolumna]], 0.75) + 1.5 * IQR(dane[[kolumna]]) | dane[[kolumna]] < quantile(dane[[kolumna]], 0.25) - 1.5 * IQR(dane[[kolumna]]), round(dane[[kolumna]], 2), "")), vjust = -0.5) +
      labs(title = paste("Boxplot dla", kolumna, "z wartościami odstającymi")) +
      theme_minimal()
    nazwa_pliku <- file.path("/Users/home/Desktop/folder bez nazwy/boxploty", paste(kolumna, "_boxplot.png", sep = ""))
    
    #nazwa_pliku <- paste("boxploty", kolumna, "_boxplot.png", sep = "")
    ggsave(wykres, filename = nazwa_pliku, width = 6, height = 4)
  }
}

#2punkt
podsumowanie_kolumn <- list()

for (kolumna in names(dane)) {
  podsumowanie <- dane %>%
    group_by(grupa) %>%
    summarise(
      Kolumna=kolumna,
      count=n(),
      mean=ifelse(is.numeric(.data[[kolumna]]), format(round(mean(.data[[kolumna]], na.rm=TRUE), 2), nsmall=2), "N/A"),
      sd=ifelse(is.numeric(.data[[kolumna]]), format(round(sd(.data[[kolumna]], na.rm=TRUE), 2), nsmall=2), "N/A"),
      median=ifelse(is.numeric(.data[[kolumna]]), format(round(median(.data[[kolumna]], na.rm=TRUE), 2), nsmall=2), "N/A")
    )
  cat("Podsumowanie dla kolumny", kolumna, ":\n")
  print(podsumowanie)
  cat("\n")
}

#3
library(ggplot2)
library(patchwork)

# Przygotuj pustą listę do przechowywania wykresów
gg_density_plots <- list()

# Pobierz unikalne grupy na podstawie danych
unikalne_grupy <- unique(dane$grupa)

# Iteruj przez kolumny (pomijając pierwszą, która to jest grupa)
for (kolumna in colnames(dane)[-1]) {  # Usuwamy pierwszą kolumnę (grupa) z iteracji
  cat("Analiza dla kolumny:", kolumna, "\n")
  
  # Sprawdź, czy zmienna jest numeryczna
  if (is.numeric(dane[[kolumna]])) {
    
    # Przeprowadź test Shapiro-Wilka
    for (grupa in unikalne_grupy) {
      cat("Analiza dla grupy:", grupa, "\n")
      
      # Filtruj dane tylko dla danej grupy
      dane_grupa <- dane[dane$grupa == grupa, ]
      
      # Przeprowadź test Shapiro-Wilka
      shapiro_test_result <- shapiro.test(dane_grupa[[kolumna]])
      
      # Wyświetl wyniki
      if (shapiro_test_result$p.value < 0.05) {
        cat("P-wartość dla testu Shapiro-Wilka dla grupy", grupa, "i kolumny", kolumna, ":", shapiro_test_result$p.value, "< 0.05 - nie można założyć zgodności z rozkładem normalnym\n")
      } else {
        cat("P-wartość dla testu Shapiro-Wilka dla grupy", grupa, "i kolumny", kolumna, ":", shapiro_test_result$p.value, "> 0.05 - można założyć zgodność z rozkładem normalnym\n")
      }
      
      # Tworzenie wykresu gęstości
      gg_density_plot <- ggplot(dane_grupa, aes(x = !!sym(kolumna), fill = grupa)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Wykres gęstości dla", kolumna)) +
        theme(legend.position = "bottom")  # Przenoszenie legendy na dół
      
      # Dodaj wykres do listy
      gg_density_plots[[paste(kolumna, "_Grupa_", grupa)]] <- gg_density_plot
    }
  } else {
    cat("Zmienna", kolumna, "nie jest numeryczna\n")
  }
}

# Przekształć listę wykresów na obiekt typu grob
plots_as_grob <- patchwork::wrap_plots(gg_density_plots, nrow = 1)  # Wykresy na jednym wierszu

# Zapisz wszystkie wykresy do jednego pliku
ggsave("wykresy_gestosci.png", plots_as_grob, width = 12, height = 6)  # Zmniejsz wysokość, aby wykresy zmieściły się obok siebie

if (!requireNamespace("lawstat", quietly=TRUE)) {
  install.packages("lawstat")
}
library(lawstat)

dane_numeryczne <- dane[, sapply(dane, is.numeric)]

for (grupa in unique(dane$grupa)) {
  cat("Analiza dla grupy:", grupa, "\n")
  
  for (kolumna in colnames(dane_numeryczne)) {
    cat("Analiza dla kolumny:", kolumna, "\n")
    
    levene_test_result <- leveneTest(dane_numeryczne[[kolumna]] ~ dane$grupa, data=dane)
    cat("P-wartość dla testu Levene:", levene_test_result$"Pr(>F)"[1], "\n")
  }
}

for (kolumna in colnames(dane)[-1]) {
  cat("Analiza dla kolumny:", kolumna, "\n")
  
  if (is.numeric(dane[[kolumna]])) {
    liczba_grup <- length(unique(dane$grupa))
    
    if (liczba_grup == 2) {
      t_test_result <- t.test(dane[[kolumna]] ~ dane$grupa)
      
      cat("Wyniki testu t-Studenta dla kolumny:", kolumna, "\n")
      print(t_test_result)
    } else if (liczba_grup > 2) {
      cat("W danej kolumnie jest więcej niż 2 grupy. Wykonuję analizę wariancji (ANOVA) dla kolumny:", kolumna, "\n")
      anova_result <- aov(dane[[kolumna]] ~ dane$grupa)
      
      cat("Wyniki analizy wariancji (ANOVA) dla kolumny:", kolumna, "\n")
      print(summary(anova_result))
      
      if (!is.null(summary(anova_result)$`Pr(>F)`[1])) {
        if (summary(anova_result)$`Pr(>F)`[1] < 0.05) {
          cat("Przeprowadzam test Tukeya post hoc dla kolumny:", kolumna, "\n")
          tukey_result <- TukeyHSD(anova_result)
          print(tukey_result)
        } else {
          cat("Nie wykonuję testu Tukeya post hoc - brak istotnych różnic między grupami w kolumnie:", kolumna, "\n")
        }
      } 
    }
  } else {
    cat("Zmienna nie jest numeryczna\n")
  }
}

library(dunn.test)
for (kolumna in colnames(dane)[-1]) {
  cat("Analiza dla kolumny:", kolumna, "\n")
  
  if (is.numeric(dane[[kolumna]])) {
    liczba_grup <- length(unique(dane$grupa))
    
    if (liczba_grup == 2) {
      welch_test_result <- welchTwoSampleTest(dane[[kolumna]] ~ dane$grupa)
      
      cat("Wyniki testu Welcha dla kolumny:", kolumna, "\n")
      print(welch_test_result)
    } else if (liczba_grup > 2) {
      cat("W danej kolumnie jest więcej niż 2 porównywalne grupy. Wykonuję test Kruskala-Wallisa dla kolumny:", kolumna, "\n")
      kruskal_test_result <- kruskal.test(dane[[kolumna]] ~ dane$grupa)
      
      cat("Wyniki testu Kruskala-Wallisa dla kolumny:", kolumna, "\n")
      print(kruskal_test_result)
      
      # if (kruskal_test_result$p.value < 0.05) {
      # cat("P-wartość z testu Kruskala-Wallisa jest mniejsza niż 0,05. Przeprowadzam test post hoc Dunna.\n")
      
      # posthoc_dunnett_result <- dunnTest(dane[[kolumna]] ~ dane$grupa, method = "bonferroni")
      
      # cat("Wyniki testu post hoc Dunna dla kolumny:", kolumna, "\n")
      # print(posthoc_dunnett_result)
      # } else {
      #  cat("P-wartość z testu Kruskala-Wallisa nie jest mniejsza niż 0,05. Nie przeprowadzam testu post hoc Dunna.\n")
      # }
    }
  } else {
    cat("Zmienna nie jest numeryczna\n")
  }
}

if (!requireNamespace("dunn.test", quietly = TRUE)) {
  install.packages("dunn.test")
}
library(dunn.test)


#library(dunn.test)
for (kolumna in colnames(dane)[-1]) {
  cat("Analiza dla kolumny:", kolumna, "\n")
  
  if (is.numeric(dane[[kolumna]])) {
    liczba_grup <- length(unique(dane$grupa))
    
    if (liczba_grup == 2) {
      wilcox_test_result <- wilcox.test(dane[[kolumna]] ~ dane$grupa)
      
      cat("Wyniki testu Wilcoxona dla kolumny:", kolumna, "\n")
      print(wilcox_test_result)
    } else if (liczba_grup > 2) {
      cat("W danej kolumnie jest więcej niż 2 porównywalne grupy. Wykonuję test Kruskala-Wallisa dla kolumny:", kolumna, "\n")
      kruskal_test_result <- kruskal.test(dane[[kolumna]] ~ dane$grupa)
      
      cat("Wyniki testu Kruskala-Wallisa dla kolumny:", kolumna, "\n")
      print(kruskal_test_result)
      
      # if (kruskal_test_result$p.value < 0.05) {
      #cat("P-wartość z testu Kruskala-Wallisa jest mniejsza niż 0,05. Przeprowadzam test post hoc Dunna.\n")
      
      #posthoc_dunnett_result <- dunnTest(dane[[kolumna]] ~ dane$grupa, method = "bonferroni")
      
      #cat("Wyniki testu post hoc Dunna dla kolumny:", kolumna, "\n")
      #print(posthoc_dunnett_result)
      # } else {
      #cat("P-wartość z testu Kruskala-Wallisa nie jest mniejsza niż 0,05. Nie przeprowadzam testu post hoc Dunna.\n")
      # }
    }
  } else {
    cat("Zmienna nie jest numeryczna\n")
  }
}

#4
library(ggplot2)

unikalne_grupy <- unique(dane$grupa)
macierz_korelacji_global <- matrix(nrow = ncol(dane), ncol = ncol(dane))

for (grupa in unikalne_grupy) {
  cat("Korelacje dla grupy:", grupa, "\n")
  
  dane_grupa <- dane[dane$grupa == grupa, ]
  
  numeryczne_kolumny <- dane_grupa[, sapply(dane_grupa, is.numeric)]
  liczba_kolumn <- ncol(numeryczne_kolumny)
  # Definiowanie folder_path wewnątrz pętli
  folder_path <- "/Users/home/Desktop/folder bez nazwy"
  for (i in 1:(liczba_kolumn - 1)) {
    for (j in (i + 1):liczba_kolumn) {
      kolumna1 <- colnames(numeryczne_kolumny)[i]
      kolumna2 <- colnames(numeryczne_kolumny)[j]
      
      cor_test_result <- cor.test(numeryczne_kolumny[, i], numeryczne_kolumny[, j])
      
      cat("Korelacja między", kolumna1, "a", kolumna2, ":\n")
      cat("Współczynnik r:", cor_test_result$estimate, "\n")
      cat("P-wartość:", cor_test_result$p.value, "\n")
      
      if (cor_test_result$p.value < 0.05) {
        cat("Wyniki wskazują na istnienie korelacji. \n")
        
        if (cor_test_result$estimate > 0) {
          cat("Korelacja jest dodatnia. \n")
          
          if (abs(cor_test_result$estimate) > 0.7) {
            cat("Korelacja jest bardzo silna. \n")
          } else if (abs(cor_test_result$estimate) > 0.5) {
            cat("Korelacja jest silna. \n")
          } else if (abs(cor_test_result$estimate) > 0.3) {
            cat("Korelacja jest o średnim natężeniu. \n")
          } else if (abs(cor_test_result$estimate) > 0.2) {
            cat("Korelacja jest słaba. \n")
          } else {
            cat("Korelacja jest bardzo słaba. \n")
          }
        } else if (cor_test_result$estimate < 0) {
          cat("Korelacja jest ujemna. \n")
          
          if (abs(cor_test_result$estimate) > 0.7) {
            cat("Korelacja jest bardzo silna. \n")
          } else if (abs(cor_test_result$estimate) > 0.5) {
            cat("Korelacja jest silna. \n")
          } else if (abs(cor_test_result$estimate) > 0.3) {
            cat("Korelacja jest o średnim natężeniu. \n")
          } else if (abs(cor_test_result$estimate) > 0.2) {
            cat("Korelacja jest słaba. \n")
          } else {
            cat("Korelacja jest bardzo słaba. \n")
          }
        } else {
          cat("Brak liniowej zależności między zmiennymi. \n")
        }
        
        # Tworzenie wykresu macierzy korelacji
        cor_matrix <- cor(numeryczne_kolumny)
        cor_df <- as.data.frame(as.table(cor_matrix))
        colnames(cor_df) <- c("Zmienna1", "Zmienna2", "Korelacja")
        
        gg <- ggplot(cor_df, aes(x = Zmienna1, y = Zmienna2, fill = Korelacja)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
          labs(title = paste("Macierz korelacji - Grupa:", grupa)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Zapisywanie wykresu do pliku PNG
        ggsave(filename = file.path(folder_path, paste("macierz_korelacji_", grupa, ".png")), plot = gg, width = 8, height = 6)
      }
    }
  }
}
