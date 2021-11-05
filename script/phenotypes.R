## Meta phenotypes

### read in data
#cholesterol (pre-treatment / post-treatmet)
rm(list = ls())
phenotype <- read.table("../data/Total_cholesterol.tsv", header = T, sep = "\t")
dim(phenotype)
phenotype_label <- "Total Cholesterol"
conversion_factor = 38.67
phenotype_variable <- "cholesterol"
m_unit <- "[mg/dL]"

#cholesterol (without Matzkin (all): pre-post)
rm(list = ls())
phenotype <- read.table("../data/Total_cholesterol_without_matzkin.tsv", header = T, sep = "\t")
dim(phenotype)
phenotype_label <- "Total Cholesterol"
conversion_factor = 38.67
phenotype_variable <- "cholesterol"
m_unit <- "[mg/dL]"

#HDL (pre-treatment / post-treatmet)
rm(list = ls())
phenotype <- read.table("../data/HDL.tsv", header = T, sep = "\t")
phenotype_label <- "High-Density Lipoprotein Cholesterol"
conversion_factor = 38.67
phenotype_variable <- "HDL"
m_unit <- "[mg/dL]"

#HDL (without Matzkin (all): pre-post)
rm(list = ls())
phenotype <- read.table("../data/HDL_without_matzkin.tsv", header = T, sep = "\t")
phenotype_label <- "High-Density Lipoprotein Cholesterol"
conversion_factor = 38.67
phenotype_variable <- "HDL"
m_unit <- "[mg/dL]"

#LDL (pre-treatment / post-treatmet)
rm(list = ls())
phenotype <- read.table("../data/LDL_without_matzkin.tsv", header = T, sep = "\t")
phenotype_label <- "Low-Density Lipoprotein Cholesterol"
phenotype_variable <- "LDL"
conversion_factor = 38.67
m_unit <- "[mg/dL]"

#LDL (without Matzkin (all): pre-post)
rm(list = ls())
phenotype <- read.table("../data/LDL_without_matzkin.tsv", header = T, sep = "\t")
phenotype_label <- "Low-Density Lipoprotein Cholesterol"
phenotype_variable <- "LDL"
conversion_factor = 38.67
m_unit <- "[mg/dL]"

#LDL (assays)
rm(list = ls())
phenotype <- read.table("../data/LDL_assays.tsv", header = T, sep = "\t")
phenotype_label <- "Low-Density Lipoprotein Cholesterol"
phenotype_variable <- "LDL"
conversion_factor = 38.67
m_unit <- "[mg/dL]"

#LDL (without Mordasini: pre-treatment / post-treatment)
rm(list = ls())
phenotype <- read.table("../data/LDL_wo_Mordasini.tsv", header = T, sep = "\t")
phenotype_label <- "Low-Density Lipoprotein Cholesterol"
phenotype_variable <- "LDL"
conversion_factor = 38.67
m_unit <- "[mg/dL]"

#LDL (without Mordasini & without Matzkin (all): pre-post-treatment)
rm(list = ls())
phenotype <- read.table("../data/LDL_without_Matzkin_Mordasini.tsv", header = T, sep = "\t")
phenotype_label <- "Low-Density Lipoprotein Cholesterol"
phenotype_variable <- "LDL"
conversion_factor = 38.67
m_unit <- "[mg/dL]"


#VLDL
rm(list = ls())
phenotype <- read.table("../data/VLDL.tsv", header = T, sep = "\t")
phenotype_label <- "Very Low-Density Lipoprotein Cholesterol"
phenotype_variable <- "VLDL"
conversion_factor = 38.67
m_unit <- "[mg/dL]"

#TG
rm(list = ls())
phenotype <- read.table("../data/TG_without_matzkin.tsv", header = T, sep = "\t")
phenotype_label <- "Triglycerides"
phenotype_variable <- "TG"
conversion_factor = 88.57
m_unit <- "[mg/dL]"

#Apo_a1
rm(list = ls())
phenotype <- read.table("../data/Apo_a1.tsv", header = T, sep = "\t")
phenotype_label <- "Apolipoprotein a1"
phenotype_variable <- "Apo_a1"
conversion_factor = 0
m_unit <- "[mg/dL]"

#Apo_b
rm(list = ls())
phenotype <- read.table("../data/Apo_B.tsv", header = T, sep = "\t")
phenotype_label <- "Apolipoprotein B"
phenotype_variable <- "Apo_B"
conversion_factor = 0
m_unit <- "[mg/dL]"

#FFA
#other script because of conversion

#FFA
rm(list = ls())
phenotype <- read.table("../data/FFA2.tsv", header = T, sep = "\t", stringsAsFactors = F,  na.strings = "NA")
phenotype_label <- "Free Fatty Acids"
phenotype_variable <- "FFA"
conversion_factor = 28.25
m_unit <- "[mg/dL]"

