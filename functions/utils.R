# Wczytywanie danych
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
inspect_data <- function(df) {
  cat("----- STRUKTURA DANYCH -----\n")
  print(str(df))
  cat("\n----- PODSUMOWANIE -----\n")
  print(summary(df))
}


# Zliczanie braków (NA) w każdej kolumnie
count_na <- function(df) {
  nas <- sapply(df, function(col) sum(is.na(col)))
  return(nas)
}

# Opcjonalnie stratyfikowany podział na train/test
train_test_split <- function(df,
                             prop   = 0.75,
                             seed   = 123,
                             strata = NULL) {
  if (!is.data.frame(df)) {
    stop("df musi być data.frame.")
  }
  if (prop <= 0 || prop >= 1) {
    stop("prop musi być w (0, 1).")
  }
  set.seed(seed)
  n <- nrow(df)
  if (is.null(strata)) {
    # klasyczny losowy split
    n_train   <- floor(prop * n)
    train_idx <- sample(seq_len(n), size = n_train)
  } else {
    # stratyfikowany split
    if (!strata %in% names(df)) {
      stop(paste0("Brak kolumny strata: ", strata))
    }
    # dla każdej grupy losujemy prop*liczba_w_grupie próbek
    idx_list <- split(seq_len(n), df[[strata]])
    train_idx <- unlist(lapply(idx_list, function(idxs) {
      n_i <- length(idxs)
      k   <- floor(prop * n_i)
      sample(idxs, size = k)
    }))
  }
  train_df <- df[train_idx, , drop = FALSE]
  test_df  <- df[-train_idx, , drop = FALSE]
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

# Funkcja pomocnicza: dodaje kolumnę intercept = 1 do regresji
add_intercept <- function(X) {
  # X – macierz (lub data.frame) cech
  intercept <- rep(1, nrow(X))
  X_int <- cbind(Intercept = intercept, as.matrix(X))
  return(X_int)
}

# Funkcja ekstrakcji macierzy X i wektora y
get_model_data <- function(df_scaled, df_orig, feature_cols, target_col) {
  X_raw <- df_scaled[, feature_cols, drop = FALSE]
  X <- add_intercept(X_raw)
  y <- df_orig[[target_col]]
  return(list(X = X, y = y))
}

# Funkcja normal equation
my_linear_regression_closedform <- function(X, y) {
  XtX   <- t(X) %*% X
  XtX_inv <- solve(XtX)
  theta <- XtX_inv %*% t(X) %*% y
  return(theta)
}

# Funkcja predict
predict_lr <- function(X, theta) {
  return(as.vector(X %*% theta))
}

# Funkcja licząca podstawowe metryki regresji
evaluate_regression <- function(y_true, y_pred) {
  n <- length(y_true)
  err <- y_pred - y_true
  
  mse <- mean(err^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(err))
  
  ss_res <- sum(err^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  r2 <- 1 - ss_res/ss_tot
  
  return(list(
    MSE  = mse,
    RMSE = rmse,
    MAE  = mae,
    R2   = r2
  ))
}

# Utworzenie klas dla klasyfikatora
make_classes <- function(y) {
  factor(
    ifelse(y >= 7, "dobra", "niedobra"),
    levels = c("niedobra", "dobra")
  )
}

# Macierz pomyłek
conf_matrix <- function(true, pred) {
  tbl <- table(True = true, Pred = pred)
  lv <- levels(true)
  tbl <- tbl[lv, lv, drop = FALSE]
  return(tbl)
}

accuracy <- function(cm) sum(diag(cm)) / sum(cm)

precision <- function(cm) {
  # TP / (TP + FP)
  prec <- diag(cm) / colSums(cm)
  return(prec)
}

recall<- function(cm) {
  # TP / (TP + FN)
  rec <- diag(cm) / rowSums(cm)
  return(rec)
}

f1_score<- function(cm) {
  p <- precision(cm)
  r <- recall(cm)
  f <- 2 * p * r / (p + r)
  return(f)
}

# Pomocnicza funkcja: zbiera miary z macierzy
get_metrics <- function(true, pred) {
  cm  <- conf_matrix(true, pred)
  acc <- accuracy(cm)
  prec <- precision(cm)
  rec <- recall(cm)
  f1 <- f1_score(cm)
  
  # Macro‐średnie
  macro_prec <- mean(prec, na.rm = TRUE)
  macro_rec <- mean(rec,  na.rm = TRUE)
  macro_f1 <- mean(f1,    na.rm = TRUE)
  
  return(list(
    accuracy = acc,
    precision = prec,
    recall = rec,
    f1 = f1,
    macro_precision = macro_prec,
    macro_recall = macro_rec,
    macro_f1 = macro_f1,
    cm = cm
  ))
}

# regresja liniowa za pomocą spadku gradientu

compute_cost_lr <- function(X, y, theta) {
  m <- length(y)
  predictions <- X %*% theta
  errors <- predictions - y
  cost <- (1 / (2 * m)) * sum(errors^2)
  return(cost)
}


gradient_descent_lr <- function(X, y, alpha, n_iters, initial_theta = NULL) {
  m <- length(y)
  n_features <- ncol(X)

  if (is.null(initial_theta)) {
    theta <- rep(0, n_features)
  } else {
    theta <- initial_theta
  }

  cost_history <- numeric(n_iters)

  for (i in 1:n_iters) {
    errors <- (X %*% theta) - y
    gradient <- (1 / m) * (t(X) %*% errors)
    theta <- theta - alpha * gradient
    cost_history[i] <- compute_cost_lr(X, y, theta)
  }

  return(list(
    theta = theta,
    cost_history = cost_history
  ))
}