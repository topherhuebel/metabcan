


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
       xlab =paste(phenotype_label, "Pre-Treatment [mg/dL]"), 
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
       xlab = paste(phenotype_label, "Mean Difference"))
dev.off()

#### Funnel plot no labels ####
### assess risk of bias with funnel plot (figure caption page 111 in book)
png(paste(phenotype_variable,"_funnel_plot_pretreatment_no_label.png", sep = ""), height = 850, width = 827)
funnel(meta_analysis,
       studlab = F,
       cex.studlab = 0.8,
       xlab = paste(phenotype_label, "Mean Difference"))
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


#### Subgroup analysis ####
### before regression 
#### meta-regression of subtype (categorical) ####

meta_analysis_meta_regression <- metareg(meta_analysis, an_subtype)

sink(file = paste(phenotype_variable,"_meta_regression_pretreatment_by_subtype.txt", sep = ""))
print(meta_analysis_meta_regression, digits = 2)
sink()

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
       xlab =paste(phenotype_label, "pre-treatment"), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F
)
dev.off()


#### Meta-regression of BMI_mean_AN_pre ####
if (sum(!is.na(meta_analysis$data$BMI_mean_AN_pre))>=4) {
  meta_reg_BMI_mean_AN_pre <- metareg(meta_analysis, BMI_mean_AN_pre, method.tau = "REML")
  sink(file = paste(mainDir,"/",subDir,"/",phenotype_variable,"_meta_regression_pretreatment_BMI_mean_AN_pre.txt", sep = ""))
  print(meta_reg_BMI_mean_AN_pre, digits = 2)
  sink()
  
  #bubble_plot
  png(filename = paste(mainDir,"/",subDir,"/",phenotype_variable,"_results_metareg_pretreatment_BMI_mean_AN_pre.png", sep = ""), width = 605, height = 505)
  bubble(meta_reg_BMI_mean_AN_pre, 
         xlim=c(min(meta_data$BMI_mean_AN_pre, na.rm = T), max(meta_data$BMI_mean_AN_pre, na.rm = T)),
         xlab ="Mean BMI AN pre-treatment")
  dev.off()












#### Meta-regression ####
#### Meta-regression BMI ####
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
       xlab =paste(phenotype_label, "pre-treatment"), 
       digits.mean = 1, 
       digits.sd = 1, 
       comb.fixed = F, 
       prediction = T
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
       ylab = paste("Difference in", phenotype_label, "Pre-Treatment"),
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
       ylab = ylab = paste("Difference in", phenotype_label, "pre-treatment"),
       xlab = "Mean duration of anorexia nervosa")
dev.off()