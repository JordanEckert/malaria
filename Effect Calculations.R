# LIBRARIES 
library(esc) #Effect Size Calculator package

# EFFECT SIZE CALCULATIONS 
# All effect sizes are calculated against outdoor HLC (so group 2 is HLC). 

# Below is information on the
# functions used to calculate effect sizes and standard errors
?esc

?esc_mean_se
?esc_t
?esc_chisq

#### Abong'o et. al #### 
## Kisian had no Funestos

# HDT - Cow
abongo_se1 = (69.8-43.2)/1.96 # CI conversion to recover SE
abongo_se2 = (8.2-5.8)/1.96 # CI conversion to recover SE

esc_mean_se(grp1m = 43.2, grp1se = abongo_se1, grp1n = 24, grp2n = 24, grp2m = 5.8, grp2se = abongo_se2, es.type = "g")

# HDT - Human
abongo_se3 = (2.1-.97)/1.96 # CI conversion to recover SE

esc_mean_se(grp1m = 0.97, grp1se = abongo_se3, grp1n = 24, grp2n = 24, grp2m = 5.8, grp2se = abongo_se2, es.type = "g")


#### Vezenegho et. al ####

# MMOct
esc_t(6.291, grp1n = 21, grp2n = 21, es.type = "g")

# MMLure
esc_t(3.861, grp1n = 21, grp2n = 21, es.type = "g")


#### Batista et. al ####

# BGS Gambiae
esc_mean_se(grp1m = 11.28, grp1se = 1.5, grp1n = 96, grp2n = 96, grp2m = 66.34, grp2se = 6.5, es.type = "g")

# BGM Gambiae
esc_mean_se(grp1m = 7.20, grp1se = 2, grp1n = 96, grp2n = 96, grp2m = 66.34, grp2se = 6.5, es.type = "g")

# BGS Funestos
esc_mean_se(grp1m = .86, grp1se = .2, grp1n = 96, grp2n = 96, grp2m = .74, grp2se = .2, es.type = "g")

# BGM Funestos
esc_mean_se(grp1m = .54, grp1se = .1, grp1n = 96, grp2n = 96, grp2m = .74, grp2se = .2, es.type = "g")


#### Chaki et. al ####

chaki_se1 = (.085 - .057)/1.96 # CI to SE
chaki_se2 = (.815 - .560)/1.96 # CI to SE

esc_mean_se(grp1m = .057, grp1se = chaki_se1, grp1n = 931, grp2n = 335, grp2m = .560, grp2se = chaki_se2, es.type = "g")

#### Davidson et. al ####

## Using experiment 2 data since it was included in experiment 1
# KT versus HLC
esc_mean_se(grp1m = 158.2, grp1se = 36.64, grp1n = 12, grp2n = 12, grp2m = 97.83, grp2se = 22.73, es.type = "g")

# BS versus HLC
esc_mean_se(grp1m = 54.92, grp1se = 12.84, grp1n = 12, grp2n = 12, grp2m = 97.83, grp2se = 22.73, es.type = "g")

# BSE versus HLC
esc_mean_se(grp1m = 73.83, grp1se = 17.2, grp1n = 12, grp2n = 12, grp2m = 97.83, grp2se = 22.73, es.type = "g")


#### Dia et. al ####

## Barkedji (all Gambiae)
# Outdoor OBET versus Outdoor HLC
esc_mean_se(grp1m = 12.3, grp1se = 2.9, grp1n = 32, grp2n = 32, grp2m = 13.5, grp2se = 3.2, es.type = "g")

# Indoor OBET versus Outdoor HLC
esc_mean_se(grp1m = 1.2, grp1se = .30, grp1n = 32, grp2n = 32, grp2m = 13.5, grp2se = 3.2, es.type = "g")

## Ngari - Funestos
# Outdoor OBET versus Outdoor HLC
esc_mean_se(grp1m = 12.7, grp1se = 2.0, grp1n = 20, grp2n = 20, grp2m = 6.0, grp2se = .9, es.type = "g")

# Indoor OBET versus Outdoor HLC
esc_mean_se(grp1m = .8, grp1se = .4, grp1n = 20, grp2n = 20, grp2m = 6.0, grp2se = .9, es.type = "g")

## Ngari - Gambiae
# Outdoor OBET versus Outdoor HLC
esc_mean_se(grp1m = 18.6, grp1se = 2.5, grp1n = 20, grp2n = 20, grp2m = 9.1, grp2se = 1.6, es.type = "g")

# Indoor OBET versus Outdoor HLC
esc_mean_se(grp1m = .1, grp1se = .005, grp1n = 20, grp2n = 20, grp2m = 9.1, grp2se = 1.6, es.type = "g")

## Ngari - Spp
# Outdoor OBET versus Outdoor HLC
esc_mean_se(grp1m = 12.5, grp1se = 2.8, grp1n = 20, grp2n = 20, grp2m = 14.1, grp2se = 4.6, es.type = "g")

# Indoor OBET versus Outdoor HLC
esc_mean_se(grp1m = .4, grp1se = .3, grp1n = 20, grp2n = 20, grp2m = 14.1, grp2se = 4.6, es.type = "g")

#### Duo-quan et. al ####

# CDC LT
esc_rpb(.82, grp1n = 256, grp2n = 512)

# Electric Motor Trap
esc_rpb(.43, grp1n = 512, grp2n = 512)

#### Gama et. al ####
#Specified as per hour, not nights - so N would be hour
esc_mean_se(grp1m = 8.08, grp1se = 1.8, grp1n = 24, grp2n = 24, grp2m = 33.02, grp2se = 5.8, es.type = "g")


#### Hiwat et. al ####

## Experiment 1
# Light Trap with Person
esc_mean_se(grp1m = .9, grp1se = 1.26, grp1n = 10, grp2n = 11, grp2m = 7.82, grp2se = 8.42, es.type = "g")

# Light Trap with CO2
esc_mean_se(grp1m = 1.11, grp1se = 1.36, grp1n = 9, grp2n = 11, grp2m = 7.82, grp2se = 8.42, es.type = "g")

# BGS with CO2
esc_mean_se(grp1m = 2.56, grp1se = 4, grp1n = 9, grp2n = 11, grp2m = 7.82, grp2se = 8.42, es.type = "g")

## Experiment 2 
# Light Trap with Person
esc_mean_se(grp1m = 1.33, grp1se = 2.06, grp1n = 12, grp2n = 12, grp2m = 15.08, grp2se = 10.6, es.type = "g")

# Light Trap with CO2
esc_mean_se(grp1m = .73, grp1se = .65, grp1n = 11, grp2n = 12, grp2m = 15.08, grp2se = 10.6, es.type = "g")

# BGS with CO2
esc_mean_se(grp1m = 6.50, grp1se = 8.27, grp1n = 12, grp2n = 12, grp2m = 15.08, grp2se = 10.6, es.type = "g")

# MM
esc_mean_se(grp1m = 3.42, grp1se = 4.85, grp1n = 12, grp2n = 12, grp2m = 15.08, grp2se = 10.6, es.type = "g")

#### Hiwat et. al (2nd included paper) ####

# MM
esc_mean_se(grp1m = 10.22, grp1se = 14.48, grp1n = 9, grp2n = 12, grp2m = 35.58, grp2se = 19.84, es.type = "g")

# Light Trap
esc_mean_se(grp1m = 3.17, grp1se = 4.63, grp1n = 12, grp2n = 12, grp2m = 35.58, grp2se = 19.84, es.type = "g")

# BGS 
esc_mean_se(grp1m = 3.50, grp1se = 3.63, grp1n = 10, grp2n = 12, grp2m = 35.58, grp2se = 19.84, es.type = "g")

#### Krajacich et. al ####

# Bolahun
krajacich_se1 = (15.29 - 11.3)/1.96
krajacich_se2 = (42.20 - 29.71)/1.96
  
esc_mean_se(grp1m = 11.30, grp1se = krajacich_se1, grp1n = 14, grp2n = 14, grp2m = 29.71, grp2se = krajacich_se2, es.type = "g")

# Bougouriba
krajacich_se3 = (83.52 - 52.38) / 1.96
krajacich_se4 = (142 - 99) / 1.96

esc_mean_se(grp1m = 52.38, grp1se = krajacich_se3, grp1n = 8, grp2n = 8, grp2m = 99, grp2se = krajacich_se4, es.type = "g")

# Diarkadougou
krajacich_se5 = (99.58 - 57.76) / 1.96
krajacich_se6 = (130 - 80.44) / 1.96

esc_mean_se(grp1m = 57.76, grp1se = krajacich_se5, grp1n = 9, grp2n = 9, grp2m = 80.44, grp2se = krajacich_se6, es.type = "g")
 
#### Sanou et. al ####
esc_t(t = -5.42, totaln = 324, es.type = "g")

#### Sikaala et. al ####

# Spp 
sikaala_se1 = (1.86 - 1.687) / 1.96
sikaala_se2 = (1.139 - 1.004) / 1.96
sikaala_se3 = (3.504 - 3.267) / 1.96
sikaala_se4 = (.134 - .088) / 1.96

esc_mean_se(grp1m = 3.267, grp1se = sikaala_se3, grp1n = 240, grp2n = 240, grp2m = 1.004, grp2se = sikaala_se2, es.type = "g")
esc_mean_se(grp1m = .088, grp1se = sikaala_se4, grp1n = 240, grp2n = 240, grp2m = 1.004, grp2se = sikaala_se2, es.type = "g")

# Funestos
sikaala_se5 = (7.637 - 7.287) / 1.96
sikaala_se6 = (7.121 - 6.784) / 1.96
sikaala_se7 = (11.385 - 10.958) / 1.96
sikaala_se8 = (6.190 - 5.875) / 1.96
sikaala_se9 = (.373 - .296) / 1.96

esc_mean_se(grp1m = 7.287, grp1se = sikaala_se5, grp1n = 240, grp2n = 240, grp2m = 6.784, grp2se = sikaala_se6, es.type = "g")
esc_mean_se(grp1m = 10.958, grp1se = sikaala_se7, grp1n = 240, grp2n = 240, grp2m = 6.784, grp2se = sikaala_se6, es.type = "g")
esc_mean_se(grp1m = 5.875, grp1se = sikaala_se8, grp1n = 240, grp2n = 240, grp2m = 6.784, grp2se = sikaala_se6, es.type = "g")
esc_mean_se(grp1m = .296, grp1se = sikaala_se9, grp1n = 240, grp2n = 240, grp2m = 6.784, grp2se = sikaala_se6, es.type = "g")

#### Kweka et. al####

#BBCO
esc_mean_se(grp1m = 50.9090909, grp1se = 7.962963, grp1n = 30
          , grp2m = 79.45454545, grp2se = 13.09090909, grp2n = 30,  es.type = "g")

#BBU
esc_mean_se(grp1m = 135.0909091, grp1se = 9.696970, grp1n = 30
            , grp2m = 79.45454545, grp2se = 13.09090909, grp2n = 30,  es.type = "g")

#### Kweka and Mahande ####

# Odor Baited Entry Trap - Cow
esc_mean_se(grp1m = 46.82432432, grp1se = 11.14864865, grp1n = 30
            , grp2m = 13.37837838, grp2se = 3.716216216, grp2n = 30,  es.type = "g")

# Odor Baited Entry Trap - Human
esc_mean_se(grp1m = 12.36486486, grp1se = 5.743243243, grp1n = 30
            , grp2m = 13.37837838, grp2se = 3.716216216, grp2n = 30,  es.type = "g")

#### Kenea et. al ####
CI_to_seHLC = 0.285428857 / 1.96
CI_to_seCDC = 0.356786071 / 1.96
CI_to_seBaited = 0.285428857 / 1.96

# CDC LT - Funestos
esc_mean_se(grp1m = 5.104895105, grp1se = CI_to_seCDC, grp1n = 30, 
            grp2m = 0.629370629, grp2se = CI_to_seHLC, grp2n = 30,  es.type = "g")

# CDC LT with CO2 - Funestos
esc_mean_se(grp1m = 5.104895105, grp1se = CI_to_seBaited, grp1n = 30
            , grp2m = 0.629370629, grp2se = CI_to_seHLC, grp2n = 30,  es.type = "g")

CI_to_seHLC2 = 0.356786071 / 1.96
CI_to_seCDC2 = 0.214071643 / 1.96
CI_to_seBaited2 = 0.285428857 / 1.96

# CDC LT - Arabiensis
esc_mean_se(grp1m = 1.608391608, grp1se = CI_to_seCDC2, grp1n = 30, 
            grp2m = 12.7972028, grp2se = CI_to_seHLC2, grp2n = 30,  es.type = "g")

# CDC LT with CO2 - Arabiensis
esc_mean_se(grp1m = 1.608391608, grp1se = CI_to_seBaited2, grp1n = 30, 
            grp2m = 12.7972028, grp2se = CI_to_seHLC2, grp2n = 30,  es.type = "g")

CI_to_seHLC3 = 0.927643785 / 1.96
CI_to_seCDC3 = 0.428143285 / 1.96
CI_to_seBaited3 = 0.4995005 / 1.96

# CDC LT - Ziemanni
esc_mean_se(grp1m = 18.53146853, grp1se = CI_to_seCDC3, grp1n = 30, 
            grp2m = 35.17482517, grp2se = CI_to_seHLC3, grp2n = 30,  es.type = "g")

# CDC LT with CO2 - Ziemanni
esc_mean_se(grp1m = 1.608391608, grp1se = CI_to_seBaited3, grp1n = 30, 
            grp2m = 35.17482517, grp2se = CI_to_seHLC3, grp2n = 30,  es.type = "g")

CI_to_seHLC4 = 0.356786071 / 1.96
CI_to_seCDC4 = 0.142714428 / 1.96
CI_to_seBaited4 = 0.071357214 / 1.96

# CDC LT - Pharoensis
esc_mean_se(grp1m = 0.909090909, grp1se = CI_to_seCDC4, grp1n = 30, 
            grp2m = 3.286713287, grp2se = CI_to_seHLC4, grp2n = 30,  es.type = "g")

# CDC LT with CO2 - Pharoensis
esc_mean_se(grp1m = 0.629370629, grp1se = CI_to_seBaited4, grp1n = 30, 
            grp2m = 3.286713287, grp2se = CI_to_seHLC4, grp2n = 30,  es.type = "g")

#### Davis et. al ####
# CDC LT - Gambaie
esc_f(f = .22, totaln = 77, es.type = "g")

# CDC LT - Funestos
esc_f(f = .96, totaln = 33, es.type = "g")


#### Missawa et. al ####
esc_beta(beta = .422, sdy = .0195, grp1n = 48, grp2n = 48, es.type = "g")
esc_beta(beta = .348, sdy = .0155, grp1n = 48, grp2n = 48, es.type = "g")


#### Sikaala et. al ####
esc_mean_se(grp1m = 1.498, grp2m = 3.860, grp1n = 20, grp2n = 20,
            grp1se = .9194, grp2se = 2.2367, es.type = "g")

esc_mean_se(grp1m = 1.047, grp2m = 3.860, grp1n = 20, grp2n = 20,
            grp1se = .6612, grp2se = 2.2367, es.type = "g")
