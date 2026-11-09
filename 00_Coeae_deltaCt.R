# Carboxylesterase genes (Coeae1f and Coeae6g): from raw Ct values to delta Ct
# Adds Coeae1f and Coeae6g to gene_expression_data.xlsx so that the same
# analyses as for Cyp6p3, Cyp6m2 and Cyp6aa1 can be run on all five genes.
# Delta Ct is calculated in the same direction as the Cyp6 data:
#   delta Ct = mean Ct of housekeeping genes (S7, Ef) - mean Ct of gene of interest
# so that higher delta Ct = higher expression.

#load packages
library(readxl)
library(data.table)
if (!require("writexl")) {
  install.packages("writexl")}
library(writexl)

#working directory (folder with gene_expression_data.xlsx and extra_qPCRs/)
#setwd("C:/Users/justi/OneDrive/Desktop/Re_ Research project results")


# Plate layouts (from Eric's prelim_data.r) ---------------------------------------
# Rows A-D and E-H: Coeae1f, Coeae6g, S7, Ef. Two plates per experiment = two technical replicates.
# NOTE: in prelim_data.r two Kisumu 5-day wells were typed '5d' instead of '5D',
# which dropped Kisumu 5-day pools 4 and 5 when merging. This is corrected here.

wells <- paste(rep(LETTERS[1:8], each = 12), 1:12, sep = '')
genes <- rep(rep(c('Coeae1f', 'Coeae6g', 'S7', 'Ef'), each = 12), 2)

age.sample.order <- data.table(Well = wells,
                               Colony = c(rep(c(rep('Kisumu', 8), rep('Tiassale', 4)), 4), rep(c(rep('Kisumu', 2), rep('Tiassale', 6), rep('', 4)), 4)),
                               Condition = c(rep(c(rep('2D', 5), rep('5D', 7)), 4), rep(c(rep('5D', 2), rep('2D', 5), '5D', 'NTC', rep('', 3)), 4)),
                               Replicate = c(rep(c(1:5, 1:3, 2:5), 4), rep(c(4:5, 1:5, 1, rep('', 4)), 4)),
                               Gene = genes)

bf.sample.order <- data.table(Well = wells,
                              Colony = c(rep(c(rep('Kisumu', 10), rep('', 2)), 4), rep(c(rep('Tiassale', 10), rep('', 2)), 4)),
                              Condition = rep(c(rep('NBF', 5), rep('BF', 5), 'NTC', ''), 8),
                              Replicate = rep(c(rep(1:5, 2), rep('', 2)), 8),
                              Gene = genes)

time.sample.order <- data.table(Well = wells,
                                Colony = c(rep(c(rep('Kisumu', 10), rep('', 2)), 4), rep(c(rep('Tiassale', 10), rep('', 2)), 4)),
                                Condition = rep(c(rep('AM', 5), rep('PM', 5), 'NTC', ''), 8),
                                Replicate = rep(c(rep(1:5, 2), rep('', 2)), 8),
                                Gene = genes)

age.sample.order[, ID := paste(substr(Colony, 1, 1), Condition, Replicate, sep = '')]
bf.sample.order[, ID := paste(substr(Colony, 1, 1), Condition, Replicate, sep = '')]
time.sample.order[, ID := paste(substr(Colony, 1, 1), Condition, Replicate, sep = '')]


# Read the AriaMx exports ("No Cq" becomes NA) --------------------------------------
read.plate <- function(file, layout){
  plate <- fread(file, header = TRUE)
  plate <- plate[, 1:2]
  setnames(plate, c('Well', 'Ct'))
  plate[, Well := trimws(Well)]
  plate[, Ct := suppressWarnings(as.numeric(Ct))]
  plate <- merge(plate, layout, by = 'Well')
  plate[Colony != '']}

combine.plates <- function(file1, file2, layout){
  p1 <- read.plate(file1, layout)
  p2 <- read.plate(file2, layout)
  merge(p1[, -c('Well')], p2[, -c('Well')],
        by = c('Colony', 'Condition', 'Replicate', 'Gene', 'ID'), suffixes = c('1', '2'))}

coeae.age  <- combine.plates('extra_qPCRs/eric1 - Tabular Results.txt', 'extra_qPCRs/eric2 - Tabular Results.txt', age.sample.order)
coeae.bf   <- combine.plates('extra_qPCRs/eric3-bf - Tabular Results.txt', 'extra_qPCRs/eric5-bf - Tabular Results.txt', bf.sample.order)
coeae.time <- combine.plates('extra_qPCRs/eric4-time - Tabular Results.txt', 'extra_qPCRs/eric6-time - Tabular Results.txt', time.sample.order)


# Delta Ct ---------------------------------------------------------------------------
# mean Ct of the two technical replicates (one Ef well on plate eric4-time did not
# amplify, so the other replicate is used), then housekeeping mean - gene mean
delta.Ct <- function(df){
  df[, mean.Ct := rowMeans(cbind(Ct1, Ct2), na.rm = TRUE)]
  hk <- df[Gene %in% c('S7', 'Ef'), .(mean.HK.Ct = mean(mean.Ct)), by = 'ID']
  df <- merge(df, hk, by = 'ID')
  df[, dCt := mean.HK.Ct - mean.Ct]
  df}

coeae.age  <- delta.Ct(coeae.age)
coeae.bf   <- delta.Ct(coeae.bf)
coeae.time <- delta.Ct(coeae.time)

# check: 20 samples per gene in each experiment
print(coeae.age[, .N, by = Gene]); print(coeae.bf[, .N, by = Gene]); print(coeae.time[, .N, by = Gene])


# Add the two genes to Justin's data ----------------------------------------------------
age_mosq <- read_excel("gene_expression_data.xlsx", sheet = "Age of the mosquito condition")
blood_feeding <- read_excel("gene_expression_data.xlsx", sheet = "Blood feeding status condition")
time_conditon <- read_excel("gene_expression_data.xlsx", sheet = "Time of the day condition")

# remove the trailing space in "5 Days " (two rows), so that all 5-day samples are one group
age_mosq$`Time of the day` <- trimws(age_mosq$`Time of the day`)

add.genes <- function(data, coeae, id.fun){
  id <- id.fun(data$`Sample Identity`)
  for (g in c('Coeae1f', 'Coeae6g')) {
    values <- coeae[Gene == g]
    data[[g]] <- values$dCt[match(id, values$ID)]}
  if (any(is.na(data$Coeae1f) | is.na(data$Coeae6g))) stop('Some samples were not matched')
  data}

# KD2-1 -> K2D1 ; KBF-1 -> KBF1 ; K6AM-1 -> KAM1
age_mosq      <- add.genes(age_mosq, coeae.age, function(x) sub('-', '', sub('D(\\d)', '\\1D', x)))
blood_feeding <- add.genes(blood_feeding, coeae.bf, function(x) sub('-', '', x))
time_conditon <- add.genes(time_conditon, coeae.time, function(x) sub('-', '', sub('6', '', x)))

head(age_mosq)

write_xlsx(list("Age of the mosquito condition" = age_mosq,
                "Blood feeding status condition" = blood_feeding,
                "Time of the day condition" = time_conditon),
           "gene_expression_data_5genes.xlsx")
