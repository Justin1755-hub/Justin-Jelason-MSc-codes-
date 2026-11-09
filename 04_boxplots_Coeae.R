# Gene_expression_analysis_box plots for Coeae1f and Coeae6g
# same layout and colours as the box plots for Cyp6p3, Cyp6m2 and Cyp6aa1 (28/07/2024)

#load packages
library(readxl)
library(ggplot2)
library(ggsignif)
library(patchwork)

age_mosq <- read_excel("gene_expression_data_5genes.xlsx",
                       sheet = "Age of the mosquito condition")

blood_feeding <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Blood feeding status condition")

time_conditon <- read_excel("gene_expression_data_5genes.xlsx",
                            sheet = "Time of the day condition")

# groups and labels (as for the Cyp6 genes) ------------------------------------------
age_mosq$Sample_Time <- paste(age_mosq$`Sample Name`, trimws(age_mosq$`Time of the day`), sep = " ")
age_mosq$Sample_Time <- factor(age_mosq$Sample_Time, levels = c("Kisumu 2 Days", "Tiassale 2 Days", "Kisumu 5 Days", "Tiassale 5 Days"),
                               labels = c("Kis 2d", "Tia 2d", "Kis 5d", "Tia 5d"))

blood_feeding$Sample_Blood <- paste(blood_feeding$`Sample Name`, blood_feeding$`Blood-feeding status`, sep = " ")
blood_feeding$Sample_Blood <- factor(blood_feeding$Sample_Blood, levels = c("Kisumu Blood-fed", "Tiassale Blood-fed", "Kisumu Non-blood-fed", "Tiassale Non-blood-fed"),
                                     labels = c("Kis BF", "Tia BF", "Kis NBF", "Tia NBF"))

time_conditon$Sample_Time <- paste(time_conditon$`Sample Name`, time_conditon$`Time of the day`, sep = " ")
time_conditon$Sample_Time <- factor(time_conditon$Sample_Time, levels = c("Kisumu 6AM", "Tiassale 6AM", "Kisumu 6PM", "Tiassale 6PM"),
                                    labels = c("Kis AM", "Tia AM", "Kis PM", "Tia PM"))

# Function for one panel ----------------------------------------------------------------
# test = "t.test" so that the asterisks are the pairwise t-tests described in the figure legends
make_box <- function(data, xvar, gene, title, labs4, cols) {
  ggplot(data = data, aes(x = .data[[xvar]], y = .data[[gene]], fill = .data[[xvar]])) +
    geom_boxplot() +
    labs(x = "", y = paste(gene, "ΔCt"), title = title) +
    geom_signif(comparisons = list(
      c(labs4[1], labs4[2]),
      c(labs4[3], labs4[4]),
      c(labs4[2], labs4[3]),
      c(labs4[1], labs4[3]),
      c(labs4[2], labs4[4]),
      c(labs4[1], labs4[4])),
      test = "t.test", map_signif_level = TRUE, step_increase = 0.1) +
    scale_fill_manual(values = setNames(cols, labs4)) +
    theme_minimal() + theme(legend.position = "none")}

age_labels   <- c("Kis 2d", "Tia 2d", "Kis 5d", "Tia 5d")
blood_labels <- c("Kis BF", "Tia BF", "Kis NBF", "Tia NBF")
time_labels  <- c("Kis AM", "Tia AM", "Kis PM", "Tia PM")
age_cols   <- c("lightblue", "cornflowerblue", "lightblue", "cornflowerblue")
blood_cols <- c("red", "darkred", "red", "darkred")
time_cols  <- c("chartreuse1", "chartreuse4", "chartreuse1", "chartreuse4")

for (gene in c("Coeae1f", "Coeae6g")) {
  p_age   <- make_box(age_mosq, "Sample_Time", gene, "Age of mosquito", age_labels, age_cols)
  p_blood <- make_box(blood_feeding, "Sample_Blood", gene, "Blood feeding status", blood_labels, blood_cols)
  p_time  <- make_box(time_conditon, "Sample_Time", gene, "Time of the day", time_labels, time_cols)
  combined <- p_age + p_blood + p_time
  print(combined)
  ggsave(filename = paste0("boxplots_", gene, ".png"), plot = combined, device = "png", dpi = 300, width = 10, height = 5)}
