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
#rm(list = ls())


# create output folders
mainDir <- "/Users/christopherhuebel/Library/Mobile Documents/com~apple~CloudDocs/Anorexia_nervosa/METALIPIDAN/data"
subDir <- "2018_11_15"
# response with FALSE if folder is already created
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
# set the working directory
setwd(dir = file.path(mainDir, subDir))


##### START HERE #####


### read in study characteristics
study_char <- read.table("../data/071118_Characteristics_reduced.tsv", header = T, sep = "\t")

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
#options(digits = 2)

### calculating whole n (pre)
meta_data$n_total <- c(meta_data$n_AN_pre+meta_data$n_HC_pre)

#### Unit conversions ####
### convert g/L to mg/dL
### convert mmol/L to mg/dL
for (variable in c("mean_AN_pre", "se_AN_pre", "sd_AN_pre", "mean_AN_post", "se_AN_post", "sd_AN_post", "mean_HC_pre", "se_HC_pre", "sd_HC_pre")) {
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "g/L", meta_data[, colnames(meta_data) %in% variable]*100, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "g/l", meta_data[, colnames(meta_data) %in% variable]*100, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "mg/L", meta_data[, colnames(meta_data) %in% variable]/10, meta_data[, colnames(meta_data) %in% variable])
  
  meta_data[, colnames(meta_data) %in% variable] <- ifelse(meta_data$measurement_unit == "mg/l", meta_data[, colnames(meta_data) %in% variable]/10, meta_data[, colnames(meta_data) %in% variable])
  
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

#### calculate difference between pre BMI: cases and controls
meta_data$bmi_diff_AN_pre_HC <- meta_data$bmi_mean_AN_pre - meta_data$bmi_mean_HC_pre
summary(meta_data$bmi_diff_AN_pre_HC)

#### calculate difference between post BMI: cases and controls
meta_data$bmi_diff_AN_post_HC <- meta_data$bmi_mean_AN_post - meta_data$bmi_mean_HC_pre
summary(meta_data$bmi_diff_AN_post_HC)




#### Meta-analysis ####
#### PRE-TREATMENT ####
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
      digits = 2
      )
sink()

### write data table
write.table(meta_analysis, file = paste(phenotype_variable,"_meta_analysis_pretreatment_data_table.tsv", sep = ""), 
            sep = "\t", 
            col.names = T,
            quote = F)

#### Forest plot ####
### create forest plot with meta package
png(paste(phenotype_variable,"_forest_plot_pretreatment.png", sep = ""), 
    height = 750, 
    width = 800, units = "px")
forest(meta_analysis, 
       xlab =paste(phenotype_label, "Pre-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
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


#### Meta-regression of bmi_mean_AN_pre ####
  meta_reg_bmi_mean_AN_pre <- metareg(meta_analysis, bmi_mean_AN_pre, method.tau = "REML")
  sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pretreatment_bmi_mean_AN_pre.txt", sep = ""))
  print(meta_reg_bmi_mean_AN_pre, digits = 2)
  sink()
  
  #bubble_plot
  png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pretreatment_bmi_mean_AN_pre.png", sep = ""), width = 605, height = 505)
  bubble(meta_reg_bmi_mean_AN_pre, 
         xlim=c(min(meta_data$bmi_mean_AN_pre, na.rm = T), max(meta_data$bmi_mean_AN_pre, na.rm = T)),
         xlab ="Mean BMI AN pre-treatment")
  dev.off()

#### Meta-regression of age_mean_AN_pre ####
  meta_reg_age_mean_AN_pre <- metareg(meta_analysis, age_mean_AN_pre, method.tau = "REML")
  sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pretreatment_age_mean_AN_pre.txt", sep = ""))
  print(meta_reg_age_mean_AN_pre, digits = 2)
  sink()
  
  #bubble_plot
  png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pretreatment_age_mean_AN_pre.png", sep = ""), width = 605, height = 505)
  bubble(meta_reg_age_mean_AN_pre, 
         xlim=c(min(meta_data$age_mean_AN_pre, na.rm = T), max(meta_data$age_mean_AN_pre, na.rm = T)),
         xlab ="Mean age AN pre-treatment")
  dev.off()
  
  
  
  #### Meta-regression of fasting ####
  meta_reg_fasting <- metareg(meta_analysis, fasting, method.tau = "REML")
  sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pretreatment_fasting.txt", sep = ""))
  print(meta_reg_fasting, digits = 2)
  sink()
  
  #bubble_plot
  png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pretreatment_fasting.png", sep = ""), width = 605, height = 505)
  bubble(meta_reg_fasting, 
         xlim=c(min(meta_data$fasting, na.rm = T), max(meta_data$fasting, na.rm = T)),
         xlab ="Fasting period")
  dev.off()
  
  #### Meta-regression of mean_disease_duration ####
  meta_reg_mean_disease_duration <- metareg(meta_analysis, mean_disease_duration, method.tau = "REML")
  sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pretreatment_mean_disease_duration.txt", sep = ""))
  print(meta_reg_mean_disease_duration, digits = 3)
  sink()
  
  #bubble_plot
  png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pretreatment_mean_disease_duration.png", sep = ""), width = 605, height = 505)
  bubble(meta_reg_mean_disease_duration, 
         xlim=c(min(meta_data$mean_disease_duration, na.rm = T), max(meta_data$mean_disease_duration, na.rm = T)),
         xlab ="Mean disorder duration")
  dev.off()

  #### Meta-regression of bmi_diff_AN_pre_HC ####
  meta_reg_bmi_diff_AN_pre_HC <- metareg(meta_analysis, bmi_diff_AN_pre_HC, method.tau = "REML")
  sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pretreatment_bmi_diff_AN_pre_HC.txt", sep = ""))
  print(meta_reg_bmi_diff_AN_pre_HC, digits = 2)
  sink()
  
  #bubble_plot
  png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pretreatment_bmi_diff_AN_pre_HC.png", sep = ""), width = 605, height = 505)
  bubble(meta_reg_bmi_diff_AN_pre_HC, 
         xlim=c(min(meta_data$bmi_diff_AN_pre_HC, na.rm = T), max(meta_data$bmi_diff_AN_pre_HC, na.rm = T)),
         xlab ="Mean disorder duration")
  dev.off()


#### Subgroup analysis ####
### check if subgroup is factor
summary(meta_data$an_subtype)

### code unkown for NA in an_subtype
meta_data$an_subtype <- as.character(meta_data$an_subtype)
meta_data$an_subtype <- ifelse(is.na(meta_data$an_subtype), "unknown", print(meta_data$an_subtype))
meta_data$an_subtype <- as.factor(meta_data$an_subtype)
summary(as.factor(meta_data$an_subtype))

### calculate meta-analysis for continuous outcome: Pre-Treatment
meta_analysis <- metacont(mean.e = mean_AN_pre, 
                          sd.e = sd_AN_pre, 
                          n.e = n_AN_pre, 
                          mean.c = mean_HC_pre, 
                          sd.c = sd_HC_pre, 
                          n.c = n_HC_pre, 
                          studlab = Main_author, 
                          data = meta_data,
                          comb.fixed = F,
                          method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                          subset = !is.na(mean_AN_pre) & !is.na(mean_HC_pre) & !is.na(n_AN_pre) & !is.na(n_HC_pre),
                          byvar=meta_data$an_subtype,
                          print.byvar=F
)

### print results
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_analysis_pretreatment_subtype.txt", sep = ""))
print(meta_analysis, 
      digits = 2
)
sink()

### create forest plot with meta package
png(paste(mainDir,"/",subDir,"/",phenotype_variable,"_forest_plot_pretreatment_subtype.png", sep = ""), height = 850, width = 800)
forest(meta_analysis, 
       xlab =paste(phenotype_label, "Pre-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()


### check if subgroup is factor
summary(meta_data$diagnostic_manual)

### code unkown for NA in diagnostic_manual
meta_data$diagnostic_manual <- as.character(meta_data$diagnostic_manual)
meta_data$diagnostic_manual <- ifelse(is.na(meta_data$diagnostic_manual), "unknown", print(meta_data$diagnostic_manual))
meta_data$diagnostic_manual <- as.factor(meta_data$diagnostic_manual)
summary(as.factor(meta_data$diagnostic_manual))

### calculate meta-analysis for continuous outcome: Pre-Treatment by diagnostic manual
meta_analysis <- metacont(mean.e = mean_AN_pre, 
                          sd.e = sd_AN_pre, 
                          n.e = n_AN_pre, 
                          mean.c = mean_HC_pre, 
                          sd.c = sd_HC_pre, 
                          n.c = n_HC_pre, 
                          studlab = Main_author, 
                          data = meta_data,
                          comb.fixed = F,
                          method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                          subset = !is.na(mean_AN_pre) & !is.na(mean_HC_pre) & !is.na(n_AN_pre) & !is.na(n_HC_pre),
                          byvar=meta_data$diagnostic_manual,
                          print.byvar=F
)

### print results
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_analysis_pretreatment_diagnostic_manual.txt", sep = ""))
print(meta_analysis, 
      digits = 2
)
sink()

### create forest plot with meta package
png(paste(mainDir,"/",subDir,"/",phenotype_variable,"_forest_plot_pretreatment_diagnostic_manual.png", sep = ""),
    height = 1050,
    width = 800)
forest(meta_analysis, 
       xlab =paste(phenotype_label, "Pre-Treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()





####
#### POST-TREATMENT META-ANALYSIS ####
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
      digits = 2
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


#### Meta-regression of bmi_mean_AN_post ####
meta_reg_bmi_mean_AN_post <- metareg(meta_analysis_posttreatment, bmi_mean_AN_post, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_posttreatment_bmi_mean_AN_post.txt", sep = ""))
print(meta_reg_bmi_mean_AN_post, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_posttreatment_bmi_mean_AN_post.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_bmi_mean_AN_post, 
       xlim=c(min(meta_data$bmi_mean_AN_post, na.rm = T), max(meta_data$bmi_mean_AN_post, na.rm = T)),
       xlab ="Mean BMI AN post-treatment")
dev.off()

#### Meta-regression of age_mean_AN_post ####
meta_reg_age_mean_AN_pre <- metareg(meta_analysis_posttreatment, age_mean_AN_pre, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_posttreatment_age_mean_AN_pre.txt", sep = ""))
print(meta_reg_age_mean_AN_pre, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_posttreatment_age_mean_AN_pre.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_age_mean_AN_pre, 
       xlim=c(min(meta_data$age_mean_AN_pre, na.rm = T), max(meta_data$age_mean_AN_pre, na.rm = T)),
       xlab ="Mean age AN pre-treatment")
dev.off()



#### Meta-regression of fasting ####
meta_reg_fasting <- metareg(meta_analysis_posttreatment, fasting, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_posttreatment_fasting.txt", sep = ""))
print(meta_reg_fasting, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_posttreatment_fasting.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_fasting, 
       xlim=c(min(meta_data$fasting, na.rm = T), max(meta_data$fasting, na.rm = T)),
       xlab ="Fasting period")
dev.off()

#### Meta-regression of mean_disease_duration ####
meta_reg_mean_disease_duration <- metareg(meta_analysis_posttreatment, mean_disease_duration, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_posttreatment_mean_disease_duration.txt", sep = ""))
print(meta_reg_mean_disease_duration, digits = 3)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_posttreatment_mean_disease_duration.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_mean_disease_duration, 
       xlim=c(min(meta_data$mean_disease_duration, na.rm = T), max(meta_data$mean_disease_duration, na.rm = T)),
       xlab ="Mean disorder duration")
dev.off()

#### Meta-regression of bmi_diff_AN_post_HC ####
meta_reg_bmi_diff_AN_post_HC <- metareg(meta_analysis_posttreatment, bmi_diff_AN_post_HC, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_posttreatment_bmi_diff_AN_post_HC.txt", sep = ""))
print(meta_reg_bmi_diff_AN_post_HC, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_posttreatment_bmi_diff_AN_post_HC.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_bmi_diff_AN_post_HC, 
       xlim=c(min(meta_data$bmi_diff_AN_post_HC, na.rm = T), max(meta_data$bmi_diff_AN_post_HC, na.rm = T)),
       xlab ="Mean disorder duration")
dev.off()

#### Meta-regression of duration_of_follow_up ####
meta_reg_duration_of_follow_up <- metareg(meta_analysis_posttreatment, duration_of_follow_up, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_posttreatment_duration_of_follow_up.txt", sep = ""))
print(meta_reg_duration_of_follow_up, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_posttreatment_duration_of_follow_up.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_duration_of_follow_up, 
       xlim=c(min(meta_data$duration_of_follow_up, na.rm = T), max(meta_data$duration_of_follow_up, na.rm = T)),
       xlab ="Duration of follow-up")
dev.off()


#### Subgroup analysis ####
### check if subgroup is factor
summary(meta_data$an_subtype)

### code unkown for NA in an_subtype
meta_data$an_subtype <- as.character(meta_data$an_subtype)
meta_data$an_subtype <- ifelse(is.na(meta_data$an_subtype), "unknown", print(meta_data$an_subtype))
meta_data$an_subtype <- as.factor(meta_data$an_subtype)
summary(as.factor(meta_data$an_subtype))

### calculate meta-analysis for continuous outcome: post-treatment
meta_analysis_posttreatment <- metacont(mean.e = mean_AN_post, 
                          sd.e = sd_AN_post, 
                          n.e = n_AN_post, 
                          mean.c = mean_HC_pre, 
                          sd.c = sd_HC_pre, 
                          n.c = n_HC_pre, 
                          studlab = Main_author, 
                          data = meta_data,
                          comb.fixed = F,
                          method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                          subset = !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre),
                          byvar=meta_data$an_subtype,
                          print.byvar=F
)

### print results
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_analysis_posttreatment_subtype.txt", sep = ""))
print(meta_analysis_posttreatment, 
      digits = 2
)
sink()

### create forest plot with meta package
png(paste(mainDir,"/",subDir,"/",phenotype_variable,"_forest_plot_posttreatment_subtype.png", sep = ""), height = 850, width = 800)
forest(meta_analysis_posttreatment, 
       xlab =paste(phenotype_label, "post-treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()


### check if subgroup is factor
summary(meta_data$diagnostic_manual)

### code unkown for NA in diagnostic_manual
meta_data$diagnostic_manual <- as.character(meta_data$diagnostic_manual)
meta_data$diagnostic_manual <- ifelse(is.na(meta_data$diagnostic_manual), "unknown", print(meta_data$diagnostic_manual))
meta_data$diagnostic_manual <- as.factor(meta_data$diagnostic_manual)
summary(as.factor(meta_data$diagnostic_manual))

### calculate meta-analysis for continuous outcome: post-treatment by diagnostic manual
meta_analysis_posttreatment <- metacont(mean.e = mean_AN_post, 
                          sd.e = sd_AN_post, 
                          n.e = n_AN_post, 
                          mean.c = mean_HC_pre, 
                          sd.c = sd_HC_pre, 
                          n.c = n_HC_pre, 
                          studlab = Main_author, 
                          data = meta_data,
                          comb.fixed = F,
                          method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                          subset = !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre),
                          byvar=meta_data$diagnostic_manual,
                          print.byvar=F
)

### print results
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_analysis_posttreatment_diagnostic_manual.txt", sep = ""))
print(meta_analysis_posttreatment, 
      digits = 2
)
sink()

### create forest plot with meta package
png(paste(mainDir,"/",subDir,"/",phenotype_variable,"_forest_plot_posttreatment_diagnostic_manual.png", sep = ""),
    height = 1050,
    width = 800)
forest(meta_analysis_posttreatment, 
       xlab =paste(phenotype_label, "post-treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()





####
#### PRE-POST META-ANALYSIS ####
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
      digits = 2
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



#### Meta-regression of bmi_mean_AN_pre ####
meta_reg_bmi_mean_AN_pre <- metareg(meta_analysis_pre_post, bmi_mean_AN_pre, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pre_post_bmi_mean_AN_pre.txt", sep = ""))
print(meta_reg_bmi_mean_AN_pre, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pre_post_bmi_mean_AN_pre.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_bmi_mean_AN_pre, 
       xlim=c(min(meta_data$bmi_mean_AN_pre, na.rm = T), max(meta_data$bmi_mean_AN_pre, na.rm = T)),
       xlab ="Mean BMI AN pre-treatment")
dev.off()

#### Meta-regression of age_mean_AN_post ####
meta_reg_age_mean_AN_pre <- metareg(meta_analysis_pre_post, age_mean_AN_pre, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pre_post_age_mean_AN_pre.txt", sep = ""))
print(meta_reg_age_mean_AN_pre, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pre_post_age_mean_AN_pre.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_age_mean_AN_pre, 
       xlim=c(min(meta_data$age_mean_AN_pre, na.rm = T), max(meta_data$age_mean_AN_pre, na.rm = T)),
       xlab ="Mean age AN pre-treatment")
dev.off()



#### Meta-regression of fasting ####
meta_reg_fasting <- metareg(meta_analysis_pre_post, fasting, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pre_post_fasting.txt", sep = ""))
print(meta_reg_fasting, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pre_post_fasting.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_fasting, 
       xlim=c(min(meta_data$fasting, na.rm = T), max(meta_data$fasting, na.rm = T)),
       xlab ="Fasting period")
dev.off()

#### Meta-regression of mean_disease_duration ####
meta_reg_mean_disease_duration <- metareg(meta_analysis_pre_post, mean_disease_duration, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pre_post_mean_disease_duration.txt", sep = ""))
print(meta_reg_mean_disease_duration, digits = 4)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pre_post_mean_disease_duration.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_mean_disease_duration, 
       xlim=c(min(meta_data$mean_disease_duration, na.rm = T), max(meta_data$mean_disease_duration, na.rm = T)),
       xlab ="Mean disorder duration")
dev.off()

#### Meta-regression of bmi_diff_pre_post ####
meta_reg_bmi_diff_pre_post <- metareg(meta_analysis_pre_post, bmi_diff_pre_post, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pre_post_bmi_diff_pre_post.txt", sep = ""))
print(meta_reg_bmi_diff_pre_post, digits = 2)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pre_post_bmi_diff_pre_post.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_bmi_diff_pre_post, 
       xlim=c(min(meta_data$bmi_diff_pre_post, na.rm = T), max(meta_data$bmi_diff_pre_post, na.rm = T)),
       xlab ="Mean disorder duration")
dev.off()



#### Meta-regression of duration_of_follow_up ####
meta_reg_duration_of_follow_up <- metareg(meta_analysis_pre_post, duration_of_follow_up, method.tau = "REML")
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pre_post_duration_of_follow_up.txt", sep = ""))
print(meta_reg_duration_of_follow_up, digits = 3)
sink()

#bubble_plot
png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pre_post_duration_of_follow_up.png", sep = ""), width = 605, height = 505)
bubble(meta_reg_duration_of_follow_up, 
       xlim=c(min(meta_data$duration_of_follow_up, na.rm = T), max(meta_data$duration_of_follow_up, na.rm = T)),
       xlab ="Duration of follow-up")
dev.off()


#### Subgroup analysis ####
### check if subgroup is factor
summary(meta_data$an_subtype)

### code unkown for NA in an_subtype
meta_data$an_subtype <- as.character(meta_data$an_subtype)
meta_data$an_subtype <- ifelse(is.na(meta_data$an_subtype), "unknown", print(meta_data$an_subtype))
meta_data$an_subtype <- as.factor(meta_data$an_subtype)
summary(as.factor(meta_data$an_subtype))

### calculate meta-analysis for continuous outcome: post-treatment
meta_analysis_pre_post <- metacont(mean.e = mean_AN_post, 
                                   sd.e = sd_AN_post, 
                                   n.e = n_AN_post, 
                                   mean.c = mean_HC_pre, 
                                   sd.c = sd_HC_pre, 
                                   n.c = n_HC_pre, 
                                   studlab = Main_author, 
                                   data = meta_data,
                                   comb.fixed = F,
                                   method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                   subset = !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre),
                                   byvar=meta_data$an_subtype,
                                   print.byvar=F
)

### print results
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_analysis_pre_post_subtype.txt", sep = ""))
print(meta_analysis_pre_post, 
      digits = 2
)
sink()

### create forest plot with meta package
png(paste(mainDir,"/",subDir,"/",phenotype_variable,"_forest_plot_pre_post_subtype.png", sep = ""), height = 850, width = 800)
forest(meta_analysis_pre_post, 
       xlab =paste(phenotype_label, "post-treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()


### check if subgroup is factor
summary(meta_data$diagnostic_manual)

### code unkown for NA in diagnostic_manual
meta_data$diagnostic_manual <- as.character(meta_data$diagnostic_manual)
meta_data$diagnostic_manual <- ifelse(is.na(meta_data$diagnostic_manual), "unknown", print(meta_data$diagnostic_manual))
meta_data$diagnostic_manual <- as.factor(meta_data$diagnostic_manual)
summary(as.factor(meta_data$diagnostic_manual))

### calculate meta-analysis for continuous outcome: post-treatment by diagnostic manual
meta_analysis_pre_post <- metacont(mean.e = mean_AN_post, 
                                   sd.e = sd_AN_post, 
                                   n.e = n_AN_post, 
                                   mean.c = mean_HC_pre, 
                                   sd.c = sd_HC_pre, 
                                   n.c = n_HC_pre, 
                                   studlab = Main_author, 
                                   data = meta_data,
                                   comb.fixed = F,
                                   method.tau = "REML", # restricted maximum-likelihood estimator to calculat heterogeneity
                                   subset = !is.na(mean_AN_post) & !is.na(mean_HC_pre) & !is.na(n_AN_post) & !is.na(n_HC_pre),
                                   byvar=meta_data$diagnostic_manual,
                                   print.byvar=F
)

### print results
sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_analysis_pre_post_diagnostic_manual.txt", sep = ""))
print(meta_analysis_pre_post, 
      digits = 2
)
sink()

### create forest plot with meta package
png(paste(mainDir,"/",subDir,"/",phenotype_variable,"_forest_plot_pre_post_diagnostic_manual.png", sep = ""),
    height = 1050,
    width = 800)
forest(meta_analysis_pre_post, 
       xlab =paste(phenotype_label, "pre-post-treatment", m_unit), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()

