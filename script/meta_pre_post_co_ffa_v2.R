### Christopher Huebel
### 30-05-2018
### METALIPIDAN

## needed packages
# install.packages("metafor")
# install.packages("meta")
# install.packages("metasens")
library(metafor)
library(meta)
library(metasens)


###clear workspace
rm(list = ls())

# create output folders
mainDir <- "/Users/christopherhuebel/Library/Mobile Documents/com~apple~CloudDocs/Anorexia_nervosa/METALIPIDAN/data"
subDir <- "2018_08_01"
# response with FALSE if folder is already created
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
# set the working directory
setwd(dir = file.path(mainDir, subDir))


##### START HERE #####


### read in study characteristics
study_char <- read.table("Characteristics_reduced.tsv", header = T, sep = "\t")

dim(study_char)
# 56 35
summary(study_char)

#### merge meta_data
meta_data <- merge(study_char, phenotype)
dim(phenotype)
dim(meta_data)

setdiff(phenotype$Main_author, meta_data$Main_author)

### list of objects
ls ()

### summary statistics of data set
summary(meta_data)

### structure of the data set
str(meta_data)

### make sure every column is numeric 
#numeric_columns <- c("n_AN_pre", "mean_AN_pre", "se_AN_pre", "sd_AN_pre", "n_AN_post", "mean_AN_post", "se_AN_post", "sd_AN_post", "n_HC_pre", "mean_HC_pre", "se_HC_pre", "sd_HC_pre")

### change columns from numeric into factors
#meta_data[numeric_columns] <- lapply(meta_data[numeric_columns], as.numeric)

### displays no more than two digits
options(digits = 2)

### calculating whole n (pre)
meta_data$n_total <- c(meta_data$n_AN_pre+meta_data$n_HC_pre)

#### Unit conversions ####
### convert g/L to mg/dL
### convert mmol/L to mg/dL
for (variable in c("mean_AN_pre", "se_AN_pre", "sd_AN_pre", "mean_AN_post", "se_AN_post", "sd_AN_post", "mean_HC_pre", "se_HC_pre", "sd_HC_pre")) {
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "g/L", meta_data[, colnames(meta_data) %in% variable]*100, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "g/l", meta_data[, colnames(meta_data) %in% variable]*100, meta_data[, colnames(meta_data) %in% variable])
  
#  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "mg/L", meta_data[, colnames(meta_data) %in% variable]/10, meta_data[, colnames(meta_data) %in% variable])
  
#  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "mg/l", meta_data[, colnames(meta_data) %in% variable]/10, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "mmol/L", meta_data[, colnames(meta_data) %in% variable]*conversion_factor, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "mmol/l", meta_data[, colnames(meta_data) %in% variable]*conversion_factor, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "µmol/L", meta_data[, colnames(meta_data) %in% variable]* conversion_factor/1000 , meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "µmol/L", meta_data[, colnames(meta_data) %in% variable]*conversion_factor/1000 , meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "µmol/l", meta_data[, colnames(meta_data) %in% variable]*conversion_factor/1000, meta_data[, colnames(meta_data) %in% variable])
  
 meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "umol/L", meta_data[, colnames(meta_data) %in% variable]*conversion_factor/1000, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "umol/l", meta_data[, colnames(meta_data) %in% variable]*conversion_factor/1000, meta_data[, colnames(meta_data) %in% variable])
}

#### Spread conversions ####
### calculate SD from SE
meta_data$sd_AN_pre <- ifelse(is.na(meta_data$sd_AN_pre), meta_data$se_AN_pre * sqrt(meta_data$n_AN_pre), meta_data$sd_AN_pre)
meta_data$sd_AN_post <- ifelse(is.na(meta_data$sd_AN_post), meta_data$se_AN_post * sqrt(meta_data$n_AN_post), meta_data$sd_AN_post)
meta_data$sd_HC_pre <- ifelse(is.na(meta_data$sd_HC_pre), meta_data$se_HC_pre * sqrt(meta_data$n_HC_pre), meta_data$sd_HC_pre)


#### Calculate BMI differences ####
#### calculate difference between post and pre BMI
meta_data$bmi_diff_pre_post <- meta_data$bmi_mean_AN_post - meta_data$bmi_mean_AN_pre
summary(meta_data$bmi_diff_pre_post)

#### calculate difference between post and pre BMI
meta_data$bmi_diff_AN_pre_HC <- meta_data$bmi_mean_AN_pre - meta_data$bmi_mean_HC_pre
summary(meta_data$bmi_diff_AN_pre_HC)

#### calculate difference between post and pre BMI
meta_data$bmi_diff_AN_post_HC <- meta_data$bmi_mean_AN_post - meta_data$bmi_mean_HC_pre
summary(meta_data$bmi_diff_AN_post_HC)


#### Meta-analysis ####
#### pre-treatment ####
### calculate meta-analysis for continuous outcome: pre-treatment
meta_analysis <- metacont(mean.e = mean_AN_pre, 
                          sd.e = sd_AN_pre, 
                          n.e = n_AN_pre, 
                          mean.c = mean_HC_pre, 
                          sd.c = sd_HC_pre, 
                          n.c = n_HC_pre, 
                          studlab = paste(Main_author, Year), 
                          data = meta_data,
                          comb.fixed = F,
                          method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                          subset = !is.na(mean_AN_pre) & !is.na(mean_HC_pre) & !is.na(n_AN_pre) & !is.na(n_HC_pre)
)

#### Output results ####
### print results
sink(file = paste(phenotype_variable,"_meta_analysis_pretreatment.txt", sep = ""))
print(meta_analysis, 
      digits = 2,
      #prediction = T
      )
sink()

### write data table
write.table(meta_analysis, file = paste(phenotype_variable,"_meta_analysis_pretreatment_data_table.tsv", sep = ""), sep = "\t", col.names = T, quote = F)

#### Forest plot ####
### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pretreatment.png", sep = ""), height = 750, width = 800, units = "px")
forest(meta_analysis, 
       xlab =paste(phenotype_label, "Pre-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
       #prediction = T,
       lab.e = "Anorexia nervosa",
       lab.c = "Controls"
       )
dev.off()

#### Funnel plot ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_pretreatment.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis,
       studlab = T,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference", m_unit))
dev.off()

#### Funnel plot no labels ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_pretreatment_no_label.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis,
       studlab = F,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference", m_unit))
dev.off()

#### Asymmetry funnel plot ####
# Begg and Mazumdar Test: Rank Correlation Test
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
sink(file = paste(phenotype_variable,"_begg_and_mazumdar_pretreatment.txt", sep = ""))
metabias(meta_analysis, method="rank", k.min = 5)
sink()

#### Radial plot ####
# If there are no small-study effects, individual study results are expected to scatter randomly around the regression line
# Regression line is constraint to go through 0
png(paste(phenotype_variable,"_radial_plot_pretreatment.png", sep = ""), height = 850, width = 827)
radial(meta_analysis)
dev.off()

#### Egger's Test ####
### Testing for a non-zero intercept ß0 (related to radial plot)
# Points close to zero on the x-axis do not scatter randomly (1/SE -> studies with large SE closer to zero)
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
png(paste(phenotype_variable,"_eggers_test_pretreatment.png", sep = ""), height = 850, width = 827)
metabias(meta_analysis, method="linreg", k.min = 5, plotit = T)
dev.off()

sink(file = paste(phenotype_variable,"_eggers_test_pretreatment.txt", sep = ""))
metabias(meta_analysis, method="linreg", k.min = 5)
sink()

#### Thompson and Sharp
# Variant of Egger's tests allowing for between-study heterogeneity
# Weighted linear regression of the treatment effect on its standard error
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
sink(file = paste(phenotype_variable,"_thompson_and_sharp_pretreatment.txt", sep = ""))
metabias(meta_analysis, method = "mm", k.min = 5)
sink()


#### Small study effects #### 
### Copas model (sign threshold 0.1)
### gives adjusted MD
cl <- copas(meta_analysis)
png(paste(phenotype_variable,"_copas_model_pretreatment.png", sep = ""), height = 800, width = 800)
plot(cl)
dev.off()

sink(file = paste(phenotype_variable,"_copas_model_pretreatment.txt", sep = ""))
print(summary(cl), digits=2)
sink()

### Adjustment for small study effects by regression 
l1 <- limitmeta(meta_analysis)
sink(file = paste(phenotype_variable,"_adjustment_regression_pretreatment.txt", sep = ""))
print(l1, digits = 2)
sink()



#### Meta-regression without bubble plots ####

#### meta-regression of subtype (categorical) ####
meta_analysis_meta_regression <- metareg(meta_analysis, an_subtype)
sink(file = paste(phenotype_variable,"_meta_regression_pretreatment_by_subtype.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

#### meta-regression of fasting period (continuous) ####
meta_analysis_meta_regression <- metareg(meta_analysis, fasting)
sink(file = paste(phenotype_variable,"_meta_regression_pretreatment_fasting.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

#### meta-regression of BMI diff (continuous) ####
meta_analysis_meta_regression <- metareg(meta_analysis, bmi_diff_AN_pre_HC)
sink(file = paste(phenotype_variable,"_meta_regression_pretreatment_bmi_diff_AN_pre_HC.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

#### Subgroup analysis ####

### check if subgroup is factor
summary(meta_data$an_subtype)

### code unkown for NA in an_subtype
meta_data$an_subtype <- as.character(meta_data$an_subtype)
meta_data$an_subtype <- ifelse(is.na(meta_data$an_subtype), "unknown", print(meta_data$an_subtype))
meta_data$an_subtype <- as.factor(meta_data$an_subtype)
summary(as.factor(meta_data$an_subtype))

### calculate meta-analysis for continuous outcome by subtype
meta_analysis <- update(meta_analysis, 
                        byvar=meta_data$an_subtype,
                        print.byvar=F
)

### print results
sink(file = paste(phenotype_variable,"_meta_analysis_pretreatment_by_subtype.txt", sep = ""))
print(meta_analysis, 
      digits = 2,
#      prediction = T
      )
sink()

### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pretreatment_by_subtype.png", sep = ""), height = 850, width = 800)
forest(meta_analysis, 
       xlab =paste(phenotype_label, "Pre-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
 #      prediction = T
 )
dev.off()

#### Meta-regression ####
#### calculate meta-analysis for continuous outcome with BMI-only (necessary because bubble plot does otherwise not work)
meta_analysis_bmi_only <- metacont(mean.e = mean_AN_pre, 
                          sd.e = sd_AN_pre, 
                          n.e = n_AN_pre, 
                          mean.c = mean_HC_pre, 
                          sd.c = sd_HC_pre, 
                          n.c = n_HC_pre, 
                          studlab = paste(Main_author, Year), 
                          data = meta_data,
                          comb.fixed = F,
                          method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                          subset = !is.na(bmi_mean_AN_pre) & !is.na(mean_AN_pre) & !is.na(mean_HC_pre) & !is.na(n_AN_pre) & !is.na(n_HC_pre)
)

### print results
sink(file = paste(phenotype_variable,"_meta_analysis_pretreatment_bmi_only.txt", sep = ""))
print(meta_analysis_bmi_only, 
      digits = 2,
      prediction = T)
sink()

### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pretreatment_bmi_only.png", sep = ""), height = 750, width = 800)
forest(meta_analysis_bmi_only, 
       xlab =paste(phenotype_label, "Pre-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
#       prediction = T
)
dev.off()

#### meta-regression of BMI ####
meta_analysis_meta_regression <- metareg(meta_analysis_bmi_only, bmi_mean_AN_pre)

### print results
sink(file = paste(phenotype_variable,"_meta_regression_pretreatment_bmi.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

### create bubble plot
png(paste(phenotype_variable,"_bubble_plot_pretreatment_bmi.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Pre-Treatment", m_unit),
       xlab = "Mean BMI Anorexia Nervosa Cases Pre-treatment")
dev.off()


### meta-regression of illness duration
### calculate meta-analysis for continuous outcome with DOD-only (necessary because bubble plot does otherwise not work)
meta_analysis_dod_only <- metacont(mean.e = mean_AN_pre, 
                                   sd.e = sd_AN_pre, 
                                   n.e = n_AN_pre, 
                                   mean.c = mean_HC_pre, 
                                   sd.c = sd_HC_pre, 
                                   n.c = n_HC_pre, 
                                   studlab = paste(Main_author, Year), 
                                   data = meta_data,
                                   comb.fixed = F,
                                   method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                   subset = !is.na(mean_disease_duration) & !is.na(mean_AN_pre) & !is.na(mean_HC_pre) & !is.na(n_AN_pre) & !is.na(n_HC_pre)
)


meta_analysis_meta_regression <- metareg(meta_analysis_dod_only, mean_disease_duration)

sink(file = paste(phenotype_variable,"_meta_regression_pretreatment_dod.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

png(paste(phenotype_variable,"_bubble_plot_pretreatment_dod.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Pre-Treatment", m_unit),
       xlab = "Mean Duration of Anorexia Nervosa")
dev.off()




####
#### Post-treatment meta-analysis ####
####

### calculate meta-analysis for continuous outcome: posttreatment
meta_analysis_posttreatment <- metacont(mean.e = mean_AN_post, 
                                   sd.e = sd_AN_post, 
                                   n.e = n_AN_post, 
                                   mean.c = mean_HC_pre, 
                                   sd.c = sd_HC_pre, 
                                   n.c = n_HC_pre, 
                                   studlab = paste(Main_author, Year), 
                                   data = meta_data,
                                   comb.fixed = F,
                                   method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                   subset = !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre)
)


#### Output results ####
### print results
sink(file = paste(phenotype_variable,"_meta_analysis_posttreatment.txt", sep = ""))
print(meta_analysis_posttreatment, 
      digits = 2,
#      prediction = T
)
sink()

### write data table
write.table(meta_analysis_posttreatment, file = paste(phenotype_variable,"_meta_analysis_posttreatment_data_table.tsv", sep = ""), sep = "\t", col.names = T, quote = F)

#### Forest plot ####
### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_posttreatment.png", sep = ""), height = 750, width = 800)
forest(meta_analysis_posttreatment, 
       xlab =paste(phenotype_label, "Post-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
#       prediction = T,
       lab.e = "Anorexia nervosa",
       lab.c = "Controls")
dev.off()

#### Funnel plot ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_posttreatment.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis_posttreatment,
       studlab = T,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference", m_unit))
dev.off()

#### Funnel plot no labels ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_posttreatment_no_label.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis_posttreatment,
       studlab = F,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference", m_unit))
dev.off()

#### Asymmetry funnel plot ####
#Begg and Mazumdar Test: Rank Correlation Test
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
sink(file = paste(phenotype_variable,"_begg_and_mazumdar_posttreatment.txt", sep = ""))
metabias(meta_analysis_posttreatment, method="rank", k.min = 5)
sink()

#### Radial plot ####
# If there are no small-study effects, individual study results are expected to scatter randomly around the regression line
# Regression line is constraint to go through 0
png(paste(phenotype_variable,"_radial_plot_posttreatment.png", sep = ""), height = 850, width = 827)
radial(meta_analysis_posttreatment)
dev.off()

#### Egger's Test ####
### Testing for a non-zero intercept ß0 (related to radial plot)
# Points close to zero on the x-axis do not scatter randomly (1/SE -> studies with large SE closer to zero)
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
png(paste(phenotype_variable,"_eggers_test_posttreatment.png", sep = ""), height = 850, width = 827)
metabias(meta_analysis_posttreatment, method="linreg", k.min = 5, plotit = T)
dev.off()

sink(file = paste(phenotype_variable,"_eggers_test_posttreatment.txt", sep = ""))
metabias(meta_analysis_posttreatment, method="linreg", k.min = 5)
sink()

#### Thompson and Sharp
# Variant of Egger's tests allowing for between-study heterogeneity
# Weighted linear regression of the treatment effect on its standard error
sink(file = paste(phenotype_variable,"_thompson_and_sharp_posttreatment.txt", sep = ""))
metabias(meta_analysis_posttreatment, method = "mm", k.min = 5)
sink()


#### Small study effects #### 
### Copas model (sign threshold 0.1)
### gives adjusted MD
cl <- copas(meta_analysis_posttreatment)
png(paste(phenotype_variable,"_copas_model_posttreatment.png", sep = ""), height = 800, width = 800)
plot(cl)
dev.off()

sink(file = paste(phenotype_variable,"_copas_model_posttreatment.txt", sep = ""))
print(summary(cl), digits=2)
sink()

### Adjustment for small study effects by regression 
l1 <- limitmeta(meta_analysis_posttreatment)
sink(file = paste(phenotype_variable,"_adjustment_regression_posttreatment.txt", sep = ""))
print(l1, digits = 2)
sink()


#### Meta-regression without bubble plots ####

#### meta-regression of subtype (categorical) ####
meta_analysis_posttreatment_meta_regression <- metareg(meta_analysis_posttreatment, an_subtype)
sink(file = paste(phenotype_variable,"_meta_regression_posttreatment_by_subtype.txt", sep = ""))
print(meta_analysis_posttreatment_meta_regression, digits = 2)
sink()

#### meta-regression of fasting period (continuous) ####
meta_analysis_posttreatment_meta_regression <- metareg(meta_analysis_posttreatment, fasting)
sink(file = paste(phenotype_variable,"_meta_regression_posttreatment_fasting.txt", sep = ""))
print(meta_analysis_posttreatment_meta_regression, digits = 2)
sink()

#### meta-regression of BMI diff (continuous) ####
meta_analysis_posttreatment_meta_regression <- metareg(meta_analysis_posttreatment, bmi_diff_AN_post_HC)
sink(file = paste(phenotype_variable,"_meta_regression_posttreatment_bmi_diff_AN_post_HC.txt", sep = ""))
print(meta_analysis_posttreatment_meta_regression, digits = 2)
sink()


#### Subgroup analysis ####
### before regression 

### check if subgroup is factor
summary(meta_data$an_subtype)

### code unkown for NA in an_subtype
meta_data$an_subtype <- as.character(meta_data$an_subtype)
meta_data$an_subtype <- ifelse(is.na(meta_data$an_subtype), "unknown", print(meta_data$an_subtype))
meta_data$an_subtype <- as.factor(meta_data$an_subtype)
summary(as.factor(meta_data$an_subtype))

### calculate meta-analysis for continuous outcome by subtype
meta_analysis_posttreatment <- update(meta_analysis_posttreatment, 
                                      byvar=meta_data$an_subtype,
                                      print.byvar=F
)

### print results
sink(file = paste(phenotype_variable,"_meta_analysis_posttreatment_by_subtype.txt", sep = ""))
print(meta_analysis_posttreatment, 
      digits = 2,
      #      prediction = T
)
sink()

### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_posttreatment_by_subtype.png", sep = ""), height = 850, width = 800)
forest(meta_analysis_posttreatment, 
       xlab =paste(phenotype_label, "Post-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
       #      prediction = T
)
dev.off()

#### Meta-regression ####
#### Meta-regression BMI ####
#### calculate meta-analysis for continuous outcome with BMI-only (necessary because bubble plot does otherwise not work)
meta_analysis_posttreatment_bmi_only <- metacont(mean.e = mean_AN_post, 
                                                 sd.e = sd_AN_post, 
                                                 n.e = n_AN_post, 
                                                 mean.c = mean_HC_pre, 
                                                 sd.c = sd_HC_pre, 
                                                 n.c = n_HC_pre, 
                                                 studlab = paste(Main_author, Year), 
                                                 data = meta_data,
                                                 comb.fixed = F,
                                                 method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                                 subset = !is.na(bmi_mean_AN_post) & !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre)
)

### print results
sink(file = paste(phenotype_variable,"_meta_analysis_posttreatment_bmi_only.txt", sep = ""))
print(meta_analysis_posttreatment_bmi_only, 
      digits = 2,
      prediction = T)
sink()

### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_posttreatment_bmi_only.png", sep = ""), height = 750, width = 800)
forest(meta_analysis_posttreatment_bmi_only, 
       xlab =paste(phenotype_label, "Post-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
#       prediction = T
)
dev.off()

#### meta-regression of BMI ####
meta_analysis_posttreatment_meta_regression <- metareg(meta_analysis_posttreatment_bmi_only, bmi_mean_AN_post)

### print results
sink(file = paste(phenotype_variable,"_meta_regression_posttreatment_bmi.txt", sep = ""))
print(meta_analysis_posttreatment_meta_regression, digits = 2)
sink()

### create bubble plot
png(paste(phenotype_variable,"_bubble_plot_posttreatment_bmi.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_posttreatment_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Post-Treatment"),
       xlab = "Mean BMI Anorexia Nervosa Cases Post-Treatment")
dev.off()


### meta-regression of illness duration
### calculate meta-analysis for continuous outcome with DOD-only (necessary because bubble plot does otherwise not work)
meta_analysis_posttreatment_dod_only <- metacont(mean.e = mean_AN_post, 
                                                 sd.e = sd_AN_post, 
                                                 n.e = n_AN_post, 
                                                 mean.c = mean_HC_pre, 
                                                 sd.c = sd_HC_pre, 
                                                 n.c = n_HC_pre, 
                                                 studlab = paste(Main_author, Year), 
                                                 data = meta_data,
                                                 comb.fixed = F,
                                                 method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                                 subset = !is.na(mean_disease_duration) & !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre)
)


meta_analysis_posttreatment_meta_regression <- metareg(meta_analysis_posttreatment_dod_only, mean_disease_duration)

sink(file = paste(phenotype_variable,"_meta_regression_posttreatment_dod.txt", sep = ""))
print(meta_analysis_posttreatment_meta_regression, digits = 2)
sink()

png(paste(phenotype_variable,"_bubble_plot_posttreatment_dod.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_posttreatment_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Post-Treatment", m_unit),
       xlab = "Mean duration of anorexia nervosa")
dev.off()






####
#### Pre-Post meta-analysis ####
####
meta_analysis_pre_post <- metacont(mean.e = mean_AN_post, 
                                   sd.e = sd_AN_post, 
                                   n.e = n_AN_post, 
                                   mean.c = mean_AN_pre, 
                                   sd.c = sd_AN_pre, 
                                   n.c = n_AN_pre, 
                                   studlab = paste(Main_author, Year), 
                                   data = meta_data,
                                   comb.fixed = F,
                                   method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                   subset = !is.na(mean_AN_post) & !is.na(mean_AN_pre) & !is.na(n_AN_post) & !is.na(n_AN_pre)
)


#### Output results ####
### print results
sink(file = paste(phenotype_variable,"_meta_analysis_pre_post.txt", sep = ""))
print(meta_analysis_pre_post, 
      digits = 2,
      #      prediction = T
)
sink()

### write data table
write.table(meta_analysis_pre_post, file = paste(phenotype_variable,"_meta_analysis_pre_post_data_table.tsv", sep = ""), sep = "\t", col.names = T, quote = F)

#### Forest plot ####
### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pre_post.png", sep = ""), height = 750, width = 800)
forest(meta_analysis_pre_post, 
       xlab =paste(phenotype_label, "Pre-Post-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
       #       prediction = T,
       lab.e = "AN post-treatment",
       lab.c = "pre-treatment")
dev.off()

#### Funnel plot ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_pre_post.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis_pre_post,
       studlab = T,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference", m_unit))
dev.off()

#### Funnel plot no labels ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_pre_post_no_label.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis_pre_post,
       studlab = F,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference", m_unit))
dev.off()

#### Asymmetry funnel plot ####
#Begg and Mazumdar Test: Rank Correlation Test
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
sink(file = paste(phenotype_variable,"_begg_and_mazumdar_pre_post.txt", sep = ""))
metabias(meta_analysis_pre_post, method="rank", k.min = 5)
sink()

#### Radial plot ####
# If there are no small-study effects, individual study results are expected to scatter randomly around the regression line
# Regression line is constraint to go through 0
png(paste(phenotype_variable,"_radial_plot_pre_post.png", sep = ""), height = 850, width = 827)
radial(meta_analysis_pre_post)
dev.off()

#### Egger's Test ####
### Testing for a non-zero intercept ß0 (related to radial plot)
# Points close to zero on the x-axis do not scatter randomly (1/SE -> studies with large SE closer to zero)
# significant p value (p<0.05) rejection of the null hypothesis of symmetry in the funnel plot -> asymmetry in the funnel plot
png(paste(phenotype_variable,"_eggers_test_pre_post.png", sep = ""), height = 850, width = 827)
metabias(meta_analysis_pre_post, method="linreg", k.min = 5, plotit = T)
dev.off()

sink(file = paste(phenotype_variable,"_eggers_test_pre_post.txt", sep = ""))
metabias(meta_analysis_pre_post, method="linreg", k.min = 5)
sink()

#### Thompson and Sharp
# Variant of Egger's tests allowing for between-study heterogeneity
# Weighted linear regression of the treatment effect on its standard error
sink(file = paste(phenotype_variable,"_thompson_and_sharp_pre_post.txt", sep = ""))
metabias(meta_analysis_pre_post, method = "mm", k.min = 5)
sink()


#### Small study effects #### 
### Copas model (sign threshold 0.1)
### gives adjusted MD
cl <- copas(meta_analysis_pre_post)
png(paste(phenotype_variable,"_copas_model_pre_post.png", sep = ""), height = 800, width = 800)
plot(cl)
dev.off()

sink(file = paste(phenotype_variable,"_copas_model_pre_post.txt", sep = ""))
print(summary(cl), digits=2)
sink()

### Adjustment for small study effects by regression 
l1 <- limitmeta(meta_analysis_pre_post)
sink(file = paste(phenotype_variable,"_adjustment_regression_pre_post.txt", sep = ""))
print(l1, digits = 2)
sink()


#### Meta-regression without bubble plots ####

#### meta-regression of subtype (categorical) ####
meta_analysis_pre_post_meta_regression <- metareg(meta_analysis_pre_post, an_subtype)
sink(file = paste(phenotype_variable,"_meta_regression_pre_post_by_subtype.txt", sep = ""))
print(meta_analysis_pre_post_meta_regression, digits = 2)
sink()

#### meta-regression of fasting period (continuous) ####
meta_analysis_pre_post_meta_regression <- metareg(meta_analysis_pre_post, fasting)
sink(file = paste(phenotype_variable,"_meta_regression_pre_post_fasting.txt", sep = ""))
print(meta_analysis_pre_post_meta_regression, digits = 2)
sink()

#### meta-regression of BMI diff (continuous) ####
meta_analysis_pre_post_meta_regression <- metareg(meta_analysis_pre_post, bmi_diff_pre_post)
sink(file = paste(phenotype_variable,"_meta_regression_pre_post_bmi_diff_AN_pre_post.txt", sep = ""))
print(meta_analysis_pre_post_meta_regression, digits = 2)
sink()


#### Subgroup analysis ####

### check if subgroup is factor
summary(meta_data$an_subtype)

### code unkown for NA in an_subtype
meta_data$an_subtype <- as.character(meta_data$an_subtype)
meta_data$an_subtype <- ifelse(is.na(meta_data$an_subtype), "unknown", print(meta_data$an_subtype))
meta_data$an_subtype <- as.factor(meta_data$an_subtype)
summary(as.factor(meta_data$an_subtype))

### calculate meta-analysis for continuous outcome by subtype
meta_analysis_pre_post <- update(meta_analysis_pre_post, 
                                 byvar=meta_data$an_subtype,
                                 print.byvar=F
)

### print results
sink(file = paste(phenotype_variable,"_meta_analysis_pre_post_by_subtype.txt", sep = ""))
print(meta_analysis_pre_post, 
      digits = 2,
      #      prediction = T
)
sink()

### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pre_post_by_subtype.png", sep = ""), height = 850, width = 800)
forest(meta_analysis_pre_post, 
       xlab =paste(phenotype_label, "Pre-Post-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
       #      prediction = T,
       lab.e = "AN post-treatment",
       lab.c = "pre-treatment"
)
dev.off()

#### Meta-regression ####
#### Meta-regression BMI ####



#### calculate meta-analysis for continuous outcome with BMI-only (necessary because bubble plot does otherwise not work)
meta_analysis_pre_post_bmi_only <- metacont(mean.e = mean_AN_post, 
                                            sd.e = sd_AN_post, 
                                            n.e = n_AN_post, 
                                            mean.c = mean_AN_pre, 
                                            sd.c = sd_AN_pre, 
                                            n.c = n_AN_pre, 
                                            studlab = paste(Main_author, Year), 
                                            data = meta_data,
                                            comb.fixed = F,
                                            method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                            subset = !is.na(bmi_diff_pre_post) & !is.na(mean_AN_post) & !is.na(mean_AN_pre) & !is.na(n_AN_post) & !is.na(n_AN_pre)
)

### print results
sink(file = paste(phenotype_variable,"_meta_analysis_pre_post_bmi_only.txt", sep = ""))
print(meta_analysis_pre_post_bmi_only, 
      digits = 2,
      prediction = T)
sink()

### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pre_post_bmi_only.png", sep = ""), height = 750, width = 800)
forest(meta_analysis_pre_post_bmi_only, 
       xlab =paste(phenotype_label, "Pre-Post-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
#       prediction = T,
       lab.e = "AN Post-treatment",
       lab.c = "Pre-treatment"
)
dev.off()

#### meta-regression of BMI ####
meta_analysis_pre_post_meta_regression <- metareg(meta_analysis_pre_post_bmi_only, bmi_diff_pre_post)

### print results
sink(file = paste(phenotype_variable,"_meta_regression_pre_post_bmi.txt", sep = ""))
print(meta_analysis_pre_post_meta_regression, digits = 2)
sink()

### create bubble plot
png(paste(phenotype_variable,"_bubble_plot_pre_post_bmi.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_pre_post_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Pre-Post-Treatment", m_unit),
       xlab = "BMI Difference Anorexia Nervosa Cases Pre-Post-Treatment")
dev.off()


### meta-regression of illness duration
### calculate meta-analysis for continuous outcome with DOD-only (necessary because bubble plot does otherwise not work)
meta_analysis_pre_post_dod_only <- metacont(mean.e = mean_AN_post, 
                                            sd.e = sd_AN_post, 
                                            n.e = n_AN_post, 
                                            mean.c = mean_AN_pre, 
                                            sd.c = sd_AN_pre, 
                                            n.c = n_AN_pre, 
                                            studlab = paste(Main_author, Year), 
                                            data = meta_data,
                                            comb.fixed = F,
                                            method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                            subset = !is.na(mean_disease_duration) & !is.na(mean_AN_post) & !is.na(mean_AN_pre) & !is.na(n_AN_post) & !is.na(n_AN_pre)
)


meta_analysis_pre_post_meta_regression <- metareg(meta_analysis_pre_post_dod_only, mean_disease_duration)

sink(file = paste(phenotype_variable,"_meta_regression_pre_post_dod.txt", sep = ""))
print(meta_analysis_pre_post_meta_regression, digits = 2)
sink()

png(paste(phenotype_variable,"_bubble_plot_pre_post_dod.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_pre_post_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Pre-Post-Treatment", m_unit),
       xlab = "Mean duration of anorexia nervosa")
dev.off()


#### meta-regression of treatment duration on pre-post-AN ####
### calculate pre-post meta-analysis for AN with 
meta_analysis_pre_post_follow_up <- metacont(mean.e = mean_AN_post, 
                                             sd.e = sd_AN_post, 
                                             n.e = n_AN_post, 
                                             mean.c = mean_AN_pre, 
                                             sd.c = sd_AN_pre, 
                                             n.c = n_AN_pre, 
                                             studlab = paste(Main_author, Year), 
                                             data = meta_data,
                                             comb.fixed = F,
                                             method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                             subset = !is.na(duration_of_follow_up) & !is.na(mean_AN_post) & !is.na(mean_AN_pre) & !is.na(n_AN_post) & !is.na(n_AN_pre))


meta_analysis_meta_regression <- metareg(meta_analysis_pre_post_follow_up, duration_of_follow_up)

sink(file = paste(phenotype_variable,"_meta_regression_pre_post_follow_up.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

png(paste(phenotype_variable,"_bubble_plot_pre_post_follow_up.png", sep = ""), height = 750, width = 1050)
bubble(meta_analysis_meta_regression, 
       ylab = paste("Difference in", phenotype_label, "Pre-Post-Treatment", m_unit),
       xlab = "Mean Duration of Follow-up/Treatment")
dev.off()

