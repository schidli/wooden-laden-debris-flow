# Laden der notwendigen Bibliothek
library(dplyr)

# Daten einlesen
data <- read.csv("dat/processed/data_all.csv", sep=",")

# Funktion zum Testen auf Normalverteilung und Durchführen des geeigneten Tests
perform_tests <- function(data, profile_value) {
  # Filtern der Daten nach Profil
  #data_profile <- data %>% filter(profile == profile_value)
  data_profile <- data
  # Test auf Normalverteilung für noWood=1 und noWood=0
  shapiro_noWood_1 <- shapiro.test(data_profile$h[data_profile$noWood == 1])
  shapiro_noWood_0 <- shapiro.test(data_profile$h[data_profile$noWood == 0])
  
  cat("\nShapiro-Wilk Test für profile =", profile_value, "und noWood=1:\n")
  print(shapiro_noWood_1)
  cat("\nShapiro-Wilk Test für profile =", profile_value, "und noWood=0:\n")
  print(shapiro_noWood_0)
  
  # Entscheidung basierend auf den Shapiro-Wilk Tests
  if (shapiro_noWood_1$p.value > 0.05 & shapiro_noWood_0$p.value > 0.05) {
    cat("\nBeide Gruppen sind normalverteilt. Durchführung des T-Tests für profile =", profile_value, ":\n")
    t_test_result <- t.test(h ~ noWood, data = data_profile)
    print(t_test_result)
    return(t_test_result)
  } else {
    cat("\nMindestens eine Gruppe ist nicht normalverteilt. Durchführung des Mann-Whitney U-Tests für profile =", profile_value, ":\n")
    mann_whitney_result <- wilcox.test(h ~ noWood, data = data_profile)
    print(mann_whitney_result)
    return(mann_whitney_result)
  }
}

# Tests für profile=1
test_result_profile_1 <- perform_tests(data, 1)

# Tests für profile=4
test_result_profile_4 <- perform_tests(data, 4)

# Funktion zur Ausgabe der Ergebnisse bei alpha=0.05
print_test_results <- function(test_result, profile_value) {
  if ("statistic" %in% names(test_result)) {
    cat("\nErgebnisse des Tests für profile =", profile_value, "bei alpha = 0.05:\n")
    cat("Test-Statistik:", test_result$statistic, "\n")
    cat("p-Wert:", test_result$p.value, "\n")
    if (test_result$p.value < 0.05) {
      cat("Ergebnis: Signifikant (p < 0.05) - Nullhypothese ablehnen, die Mittelwerte/Verteilungen sind unterschiedlich.\n")
    } else {
      cat("Ergebnis: Nicht signifikant (p >= 0.05) - Nullhypothese nicht ablehnen, die Mittelwerte/Verteilungen sind gleich.\n")
    }
  }
}

# Ergebnisse für profile=1
print_test_results(test_result_profile_1, 1)

# Ergebnisse für profile=4
print_test_results(test_result_profile_4, 4)
