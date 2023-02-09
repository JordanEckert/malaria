#### META ANALYSIS ####

# Need to check over and clean out code to match organizational structure of paper

#### Libraries ####
library(meta)
library(metafor)
library(readxl)
library(PublicationBias)
library(ggplot2)
library(tidyverse)


if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")

library(dmetar)

#### Importing Dataset ####
datum = read_excel("~/Documents/Projects/Malaria/MalariaMetaData.xlsx")
str(datum)

#### Cleaning Dataset ####
# Necessary factor variables
datum$Author = as.factor(datum$Author)
datum$Study = as.factor(datum$Study)
datum$Year = as.factor(datum$Year)
datum$Country = as.factor(datum$Country)
datum$Africa = as.factor(datum$Africa)
datum$Category = as.factor(datum$Category)
datum$Subcategory = as.factor(datum$Subcategory)
datum$Type = as.factor(datum$Type)
datum$Species = as.factor(datum$Species)

# Releveling - helps with some interpretations later in analysis
datum$Category = relevel(factor(datum$Category), ref="Physical")
datum$Species = relevel(factor(datum$Species), ref="Anopheles Spp")
datum$Type = relevel(factor(datum$Type), ref = "Tent")

# Clean off the single electrocuting group
datum.cleaned = datum[-c(37),] # 37 is only electrocuting trap
str(datum.cleaned)


#################### POWER ANALYSIS ########################
power.analysis(d = .1,
               k = 51,
               n1 = 59,
               n2 = 59,
               heterogeneity = "high")

#################### MODELING ##############################
#### Random Effects Modeling ####
m.malaria <- metagen(TE,
                     seTE,
                     data = datum,
                     studlab = datum$Author,
                     comb.fixed = F,
                     comb.random = T,
                     method.tau = "REML",
                     hakn = T,
                     prediction = T,
                     sm = "SMD")
m.malaria

#### Forest Plot for RMA ####
png(filename = "~/Desktop/RMAForest.png", width=5000,height=7000,res=400)
forest(m.malaria)
dev.off()

####################### OUTLIER ANALYSIS ##############################
#### Random Effects Model without Outliers ####
o.malaria = find.outliers(m.malaria)
o.malaria

# Forest plot without outliers
png(filename = "~/Desktop/OutlierForest.png", width=5000,height=7000,res=400)
forest(o.malaria)
dev.off()

#### Influential Analysis ####
inf.malaria = InfluenceAnalysis(m.malaria, random = TRUE)
inf.malaria

# Baujat plots
png(filename = "~/Desktop/Baujat.png", width=7000,height=7000,res=400)
plot(inf.malaria, "baujat", max.overlaps = 100)
dev.off()

# Influence plots
png(filename = "~/Desktop/Influence.png", width=7000,height=7000,res=400)
plot(inf.malaria, "influence")
dev.off()

# Effect size plots
png(filename = "~/Desktop/EffectSize,png", width=7000,height=7000,res=400)
plot(inf.malaria, "es")
dev.off()

# I^2 plots
png(filename = "~/Desktop/I2.png", width=7000,height=7000,res=400)
plot(inf.malaria, "i2")
dev.off()

# GOSH
m.rma <- rma(yi = m.malaria$TE,
             sei = m.malaria$seTE,
             method = m.malaria$method.tau,
             test = "knha")
m.rma

res.gosh = gosh(m.rma)
     
res.gosh.diag <- gosh.diagnostics(res.gosh)
res.gosh.diag
        
plot(res.gosh.diag)

################### SUBGROUP ANALYSIS #############################

# Removing the single electrocuting observation for Type analysis
m.malaria2 <- metagen(TE,
                     seTE,
                     data = datum.cleaned,
                     studlab = datum.cleaned$Author,
                     comb.fixed = F,
                     comb.random = T,
                     method.tau = "REML",
                     hakn = T,
                     prediction = T,
                     sm = "SMD")
m.malaria2

#### Subgroup Analysis by Type ####
sub.type = update.meta(m.malaria2, 
                        byvar = Type, 
                        tau.common = FALSE)
sub.type

# Robustness check
sub.type.2 = update.meta(m.malaria2, 
                        byvar = Type, 
                        tau.common = TRUE)
sub.type.2


#### Forest Plot for Group ####
png(filename = "~/Desktop/GroupForest.png", width=5000,height=8000,res=400)
forest(sub.type)
dev.off()

#### Subgroup Analysis by Africa ####
sub.africa = update.meta(m.malaria, 
                         byvar = Africa, 
                         tau.common = FALSE)
sub.africa

sub.africa2 = update.meta(m.malaria, 
                         byvar = Africa, 
                         tau.common = TRUE)
sub.africa2


#### Forest Plot for Africa ####
png(filename = "~/Desktop/AfricaForest.png", width=5000,height=8000,res=400)
forest(sub.africa)
dev.off()

#### Subgroup Analysis by Species ####
sub.species = update.meta(m.malaria, 
                        byvar = Species, 
                        tau.common = FALSE)
sub.species

sub.species2 = update.meta(m.malaria, 
                          byvar = Species, 
                          tau.common = T)
sub.species2

#### Forest Plot for Species ####
png(filename = "~/Desktop/SpeciesForest.png", width=5000,height=8000,res=400)
forest(sub.species)
dev.off()


##################### META - REGRESSION #########################

#### Model Selection for Multiple Meta - Regression ####

multimodel.inference(TE = "TE", 
                     seTE = "seTE",
                     data = datum.cleaned,
                     predictors = c("Type", "Species", "Africa"),
                     interaction = T) 
 
#### Selected Meta-Regression Model ####
malaria.reg <- rma(yi = TE, 
                sei = seTE, 
                data = datum.cleaned, 
                method = "REML", 
                mods = ~ Type + Species + Africa + Type*Species, 
                test = "knha")

malaria.reg

permutest(malaria.reg)

#### Appendix ####
#### Subgroup Analysis for Category ####
sub.category = update.meta(m.malaria, 
                           byvar = Category, 
                           tau.common = FALSE)
sub.category

# Since it's mentioned in appendix only, no tests were run for tau.common = TRUE to check robustness

#### Forest Plot for Category ####
png(filename = "~/Desktop/TypeForest.png", width=5000,height=7800,res=400)
forest(sub.type)
dev.off()

# Including Category - No gain in amount of heterogeneity accounted for; results excluded from formal paper
## Category and Type are probably very correlated, meaning that including both in meta-regression is not necessary

multimodel.inference(TE = "TE", 
                     seTE = "seTE",
                     data = datum.cleaned,
                     predictors = c("Type", "Species", "Africa", "Category"),
                     interaction = T) 

malaria.reg.2 <- rma(yi = TE, 
                   sei = seTE, 
                   data = datum.cleaned, 
                   method = "REML", 
                   mods = ~ Type + Species + Africa + Type*Species + Category + Category*Species, 
                   test = "knha")

malaria.reg.2

permutest(malaria.reg.2)

######################## PUBLICATION BIAS #########################

#### Funnel Plot for RMA ####
funnel(m.malaria, xlab = "Hedges' g", main = "Funnel Plot")
eggers.test(m.malaria)

metabias(m.malaria, method.bias = "linreg")

trimfill(m.malaria)
######################## SUBDIVIDED GAMBAIE DATASETS ######################

# Necessary to combine other levels to increase power for analysis
gambiae = datum.cleaned[datum.cleaned$Species == "Anopheles gambiae",]
gambiae$Type2 = gambiae$Type
levels(gambiae$Type2)[levels(gambiae$Type2)=="Other - Mechanical" |levels(gambiae$Type2)=="Other - Passive" ] <-"Other"


#### Random Effects Modeling ####
gam.malaria <- metagen(TE,
                     seTE,
                     data = gambiae,
                     studlab = gambiae$Author,
                     comb.fixed = F,
                     comb.random = T,
                     method.tau = "REML",
                     hakn = T,
                     prediction = T,
                     sm = "SMD")
gam.malaria

#### Subgroup Analysis by Type ####
gam.sub.type = update.meta(gam.malaria, 
                        byvar = Type2, 
                        tau.common = FALSE)
gam.sub.type

#### Subgroup Analysis by Category ####
gam.category = update.meta(gam.malaria, 
                       byvar = Category, 
                       tau.common = FALSE)
gam.category

######################## SUBDIVIDED FUNESTOS DATASETS ######################
funestus = datum.cleaned[datum.cleaned$Species == "Anopheles funestus",]
funestus$Type2 = funestus$Type
levels(funestus$Type2)[levels(funestus$Type2)=="Other - Mechanical" |levels(funestus$Type2)=="Other - Passive" ] <-"Other"

#### Random Effects Modeling ####
fun.malaria <- metagen(TE,
                       seTE,
                       data = funestus,
                       studlab = funestus$Author,
                       comb.fixed = F,
                       comb.random = T,
                       method.tau = "REML",
                       hakn = T,
                       prediction = T,
                       sm = "SMD")
fun.malaria

#### Subgroup Analysis by Type ####
fun.sub.group = update.meta(fun.malaria, 
                            byvar = Type2, 
                            tau.common = FALSE)
fun.sub.group

  #### Subgroup Analysis by Category ####
fun.category = update.meta(fun.malaria, 
                       byvar = Category, 
                       tau.common = FALSE)
fun.category

