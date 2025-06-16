# Wczytywanie danych
# filepath: ścieżka do pliku CSV
# sep: separator pól (domyślnie ";")
# dec: znak dziesiętny (domyślnie ".")
# Zwraca data.frame z wczytanymi danymi.
load_wine_data <- function(filepath,
                           sep = ";",
                           dec = ".") {
  if (!file.exists(filepath)) {
    stop(paste0("Plik nie istnieje: ", filepath))
  }
  df <- read.csv(filepath,
                 header = TRUE,
                 sep = sep,
                 dec = dec,
                 stringsAsFactors = FALSE)
  return(df)
}


# Szybka funkcja wyświetlająca strukturę i podsumowanie
# df: dowolny data.frame
# Wypisuje strukturę (str) i summary dla szybkiego podglądu danych.
inspect_data <- function(df) {
  cat("----- STRUKTURA DANYCH -----\n")
  print(str(df))
  cat("\n----- PODSUMOWANIE -----\n")
  print(summary(df))
}


# Zliczanie braków (NA) w każdej kolumnie
# df: data.frame
# Zwraca wektor liczby braków dla każdej kolumny.
count_na <- function(df) {
  nas <- sapply(df, function(col) sum(is.na(col)))
  return(nas)
}

# Prosty podział na zbiór treningowy i testowy
# df: data.frame z pełnym zestawem danych
# prop: proporcja danych przeznaczonych do trenowania (np. 0.75)
# seed: ziarno generatora (dla powtarzalności)
# Zwraca listę z elementami: $train (data.frame) i $test (data.frame).
train_test_split <- function(df,
                             prop = 0.75,
                             seed = 123) {
  if (!is.data.frame(df)) {
    stop("df musi być data.frame.")
  }
  if (prop <= 0 || prop >= 1) {
    stop("prop musi być w (0, 1).")
  }
  set.seed(seed)
  n <- nrow(df)
  n_train <- floor(prop * n)
  
  # losowy wybór indeksów
  train_idx <- sample(seq_len(n), size = n_train)
  train_df  <- df[train_idx, , drop = FALSE]
  test_df   <- df[-train_idx, , drop = FALSE]
  
  return(list(train = train_df, test = test_df))
}


# Wyświetlenie liczby obserwacji w zbiorach
# splits: wynik train_test_split (lista ze składnikami $train, $test)
# Po prostu catuje informację ile wierszy w train / test.
print_split_sizes <- function(splits) {
  if (!("train" %in% names(splits)) || !("test" %in% names(splits))) {
    stop("Lista splits musi zawierać elementy 'train' i 'test'.")
  }
  n_train <- nrow(splits$train)
  n_test  <- nrow(splits$test)
  cat("Liczba obserwacji w zbiorze treningowym: ", n_train, "\n")
  cat("Liczba obserwacji w zbiorze testowym:     ", n_test, "\n")
}


# Skalowanie cech: standard lub min–max
# df: data.frame
# columns: character vector z nazwami kolumn numerycznych do przeskalowania
# method: "standard" (domyślnie) albo "minmax"
# Zwraca listę: 
# $df_scaled = data.frame ze skalowanymi kolumnami (pozostałe bez zmian),
# $params = lista z parametrami skalowania dla każdej kolumny,
# $method = "standard" lub "minmax".
scale_numeric <- function(df, columns, method = "standard") {
  df_scaled <- df
  params <- list()
  
  for (col in columns) {
    x <- df[[col]]
    if (!is.numeric(x)) {
      stop(paste0("Kolumna ", col, " nie jest numeryczna."))
    }
    
    if (method == "standard") {
      mu <- mean(x, na.rm = TRUE)
      sigma <- sd(x, na.rm = TRUE)
      if (sigma == 0) sigma <- 1  # zabezpieczenie przed dzieleniem przez 0
      df_scaled[[col]] <- (x - mu) / sigma
      params[[col]] <- list(mean = mu, sd = sigma)
      
    } else if (method == "minmax") {
      min_val <- min(x, na.rm = TRUE)
      max_val <- max(x, na.rm = TRUE)
      range_val <- max_val - min_val
      if (range_val == 0) range_val <- 1
      df_scaled[[col]] <- (x - min_val) / range_val
      params[[col]] <- list(min = min_val, max = max_val)
      
    } else {
      stop("Nieznana metoda skalowania: użyj 'standard' lub 'minmax'.")
    }
  }
  
  return(list(df_scaled = df_scaled, params = params, method = method))
}

# Funkcja do zastosowania tych samych parametrów skalowania do zbioru testowego
# df_test: data.frame (zbiór testowy)
# params: lista zwrócona przez scale_numeric na zbiorze treningowym
# method: "standard" lub "minmax"
# Zwraca: data.frame testowy ze skalowanymi kolumnami zgodnie z parametrami.
apply_scaling <- function(df_test, params, method = "standard") {
  df_scaled <- df_test
  
  for (col in names(params)) {
    x <- df_test[[col]]
    if (method == "standard") {
      mu <- params[[col]]$mean
      sigma <- params[[col]]$sd
      if (sigma == 0) sigma <- 1
      df_scaled[[col]] <- (x - mu) / sigma
      
    } else if (method == "minmax") {
      min_val <- params[[col]]$min
      max_val <- params[[col]]$max
      range_val <- max_val - min_val
      if (range_val == 0) range_val <- 1
      df_scaled[[col]] <- (x - min_val) / range_val
      
    } else {
      stop("Nieznana metoda w apply_scaling: użyj 'standard' lub 'minmax'.")
    }
  }
  
  return(df_scaled)
}

# Funkcja do stworzenia histogramów i odparna na error "Error in plot.new() : figure margins too large"
plot_histograms <- function(df, cols, ncol = 2, mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1)) {
  n <- length(cols)
  nrow <- ceiling(n / ncol)
  op <- par(no.readonly = TRUE) # zapamiętaj poprzednie ustawienia
  on.exit(par(op)) # przy wyjściu przywróć je
  
  par(mfrow = c(nrow, ncol), mar = mar, oma = oma)
  for (col in cols) {
    hist(df[[col]],
         main  = paste("Histogram:", col),
         xlab  = col,
         breaks= 20)
  }
}
