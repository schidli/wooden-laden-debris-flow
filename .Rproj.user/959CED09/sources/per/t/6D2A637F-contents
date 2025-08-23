# Laden der notwendigen Bibliotheken
library(dplyr)
library(ggpubr)
library(rstatix)
# Installieren und Laden des PMCMRplus Pakets
if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
  install.packages("PMCMRplus")
}
library(PMCMRplus)

# Daten einlesen
data <- read.csv("dat/processed/data_all.csv", sep=",")
profile_value=1
data_profile <- data %>% filter(profile == profile_value)
conover_result <- PMCMRplus::kwAllPairsConoverTest(h ~ factor(Wood), data = data_profile, p.adjust.method = "bonferroni")
print(conover_result)
dune_result<-dunn_test(h~Wood, data=data_profile, p.adjust.method = "bonferroni")
print(dune_result)
wilcox_result <-pairwise.wilcox.test(data_profile$h,data_profile$Wood,p.adjust="bonferroni")
print(wilcox_result)
wilcox_effect_result<-data_profile %>%
  wilcox_effsize(h~Wood) %>%
  as.data.frame()
print(wilcox_effect_result)




# Funktion zum Testen auf Normalverteilung und Durchführen des geeigneten Tests
perform_tests <- function(data, profile_value) {
  # Filtern der Daten nach Profil
  data_profile <- data %>% filter(profile == profile_value)
  
  # Test auf Normalverteilung für jede Wood-Gruppe
  shapiro_results <- data_profile %>%
    group_by(Wood) %>%
    summarise(p_value = shapiro.test(Zzm)$p.value)
  
  print(shapiro_results)
  
  # Entscheidung basierend auf den Shapiro-Wilk Tests
  if (all(shapiro_results$p_value > 0.05)) {
    cat("\nAlle Gruppen sind normalverteilt. Durchführung der ANOVA für profile =", profile_value, ":\n")
    anova_result <- aov(Zzm ~ factor(Wood), data = data_profile)
    summary(anova_result)
    conover_result <- PMCMRplus::kwAllPairsConoverTest(Zzm ~ factor(Wood), data = data_profile, p.adjust.method = "bonferroni")
      print(conover_result)
      dune_result<-dunn_test(Zzm~Wood, data=data_profile, p.adjust.method = "bonferroni")
      print(dune_result)
      wilcox_result <-pairwise.wilcox.test(data_profile$Zzm,data_profile$Wood,p.adjust="bonferroni")
      print(wilcox_result)
      wilcox_effect_result<-data_profile %>%
        wilcox_effsize(Zzm~Wood) %>%
        as.data.frame()
      print(wilcox_effect_result)
    return(list(test_type = "ANOVA", result = anova_result))
  } else {
    cat("\nMindestens eine Gruppe ist nicht normalverteilt. Durchführung des Kruskal-Wallis-Tests für profile =", profile_value, ":\n")
    kruskal_result <- kruskal.test(Zzm ~ factor(Wood), data = data_profile)
    print(kruskal_result)
    
    if (kruskal_result$p.value < 0.05) {
      cat("\nKruskal-Wallis-Test ist signifikant. Durchführung des Conover-Iman-Post-Hoc-Tests für profile =", profile_value, ":\n")
      conover_result <- PMCMRplus::kwAllPairsConoverTest(Zzm ~ factor(Wood), data = data_profile, p.adjust.method = "bonferroni")
      print(conover_result)
      dune_result<-dunn_test(Zzm~Wood, data=data_profile, p.adjust.method = "bonferroni")
      print(dune_result)
      wilcox_result <-pairwise.wilcox.test(data_profile$Zzm,data_profile$Wood,p.adjust="bonferroni")
      print(wilcox_result)
      wilcox_effect_result<-data_profile %>%
        wilcox_effsize(Zzm~Wood) %>%
        as.data.frame()
      print(wilcox_effect_result)
      return(list(test_type = "Kruskal-Wallis", kruskal_result = kruskal_result, conover_result = conover_result))
    } else {
      return(list(test_type = "Kruskal-Wallis", kruskal_result = kruskal_result))
      
    }
  }
}

# Tests für profile=1
test_result_profile_1 <- perform_tests(data, 1)

# Tests für profile=4
test_result_profile_4 <- perform_tests(data, 4)
# 
# # Funktion zur Ausgabe der Ergebnisse bei alpha=0.05
# print_test_results <- function(test_result, profile_value) {
#   if (test_result$test_type == "Kruskal-Wallis") {
#     cat("\nErgebnisse des Kruskal-Wallis-Tests für profile =", profile_value, "bei alpha = 0.05:\n")
#     cat("Kruskal-Wallis-Test-Statistik:", test_result$kruskal_result$statistic, "\n")
#     cat("Kruskal-Wallis-Test p-Wert:", test_result$kruskal_result$p.value, "\n")
#     if (test_result$kruskal_result$p.value < 0.05) {
#       cat("Ergebnis: Signifikant (p < 0.05) - Nullhypothese ablehnen, die Verteilungen sind unterschiedlich.\n")
#       cat("\nErgebnisse des Conover-Iman-Post-Hoc-Tests:\n")
#       print(test_result$conover_result)
#     } else {
#       cat("Ergebnis: Nicht signifikant (p >= 0.05) - Nullhypothese nicht ablehnen, die Verteilungen sind gleich.\n")
#     }
#   } else if (test_result$test_type == "ANOVA") {
#     cat("\nErgebnisse der ANOVA für profile =", profile_value, "bei alpha = 0.05:\n")
#     anova_summary <- summary(test_result$result)
#     cat("ANOVA F-Wert:", anova_summary[[1]]$`F value`[1], "\n")
#     cat("ANOVA p-Wert:", anova_summary[[1]]$`Pr(>F)`[1], "\n")
#     if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
#       cat("Ergebnis: Signifikant (p < 0.05) - Nullhypothese ablehnen, die Mittelwerte sind unterschiedlich.\n")
#     } else {
#       cat("Ergebnis: Nicht signifikant (p >= 0.05) - Nullhypothese nicht ablehnen, die Mittelwerte sind gleich.\n")
#     }
#   }
# }
# 
# # Ergebnisse für profile=1
# print_test_results(test_result_profile_1, 1)
# 
# # Ergebnisse für profile=4
# print_test_results(test_result_profile_4, 4)
# 
# 
# pairwise.wilcox.test(data$Ruhepuls,data$Trainingsgruppe, 
#                      p.adjust="bonferroni")
# 
# # bzw. für den Dunn's Test:
# 
# library(rstatix)
# data_profile <- data %>% filter(profile == 1)
# dunn_test(Yy~Wood, data=data_profile, 
#           p.adjust.method = "bonferroni")
# 
# pairwise.wilcox.test(data_profile$Yy,data_profile$Wood, 
#                      p.adjust="bonferroni")
# 
# 
# data_profile %>%
#   wilcox_effsize(Zzm~Wood) %>%
#   as.data.frame()
# 
