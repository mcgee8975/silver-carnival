

library(MPsychoR)
library(matrixStats)
library(ggplot2)          #plotting
library(plotly)           #possibly not needed
library(maps)             #working with map data
library(mapdata)          #usa, states, etc
library(gridExtra)        #display multiple ggplots
library(wesanderson)      #fun color schemes
library(jtools)           #apa theme
library(ggrepel)          #map labels
library(ggalt)            #dumbbell plot
library(GGally)           #scatter plot matrix
library(lavaan)           #SEM
library(mediation)      #path models
library(devtools)
library(sem)
library(tidygraph)
library(ggraph)
library(OpenMx)
library(semPaths)
library(MASS)
library(car)
library(semTools)
library(Amelia)
library(mice)
library(Hmisc)
library(psych)
library(MASS)
library(rcompanion)
library(BaylorEdPsych)
library(ltm)
library(GMCM)
library(mvnmle)
library(MissMech)
library(dplyr)
library(tidyverse)
library(semPlot)
library(boot)

library(lme4) # load library
library(arm)  # convenience functions for regression in R
library(lavaan)

library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(tidygraph)
library(ggraph)
library(semTools)
library(modelr)
library(pwr)
library(simsem)
library(compute.es)
library(lavaan)
library(VIM)
library(naniar)
library(missMDA)
library(missForest)
library(FactoMineR)
library(forcats)
library(distances)
library(outliers)
library(xtable)
library(RMediation)
library(haven)
library(rockchalk)


#################################### Packages added in this sytax #

##### Loaded
library(tidyverse)
library(psych)
library(Hmisc)
library(lavaan)
library(QuantPsyc)
library(ppcor)
library(sem)
library(semTools)
library(semPlot)

######################################################################
##### Load data
######################################################################

### Co-Use and Dems data sets

##### Alcoholic beverages (beer, wine, spirits, etc.)

data.etoh <- read.csv("ASSIST_ETOH_Filtered_06.18.20.csv")
options(scipen = 999)


##### Tobacco products (cigarettes, chewing tobacco, cigars, etc.)

data.tob <- read.csv("ASSIST_TOB_Filtered_06.18.20.csv")
options(scipen = 999)


##### Cannabis (marijuana, pot, grass, hash, etc.)

data.thc <- read.csv("ASSIST_THC_Filtered_06.18.20.csv")
options(scipen = 999)


###### Opioids (heroin, morphine, methadone, buprenorphine, codeine, etc.)
data.ops <- read.csv("ASSIST_OPS_Filtered_06.18.20.csv")
options(scipen = 999)


##### Cocaine (coke, crack, etc.)

data.coc <- read.csv("ASSIST_COC_Filtered_06.18.20.csv")
options(scipen = 999)


##### Amphetamine-type stimulants (speed, meth, ecstasy, etc.)

data.amp <- read.csv("ASSIST_AMP_Filtered_06.18.20.csv")
options(scipen = 999)


##### Inhalants (nitrous, glue, petrol, paint thinner, etc.)
data.inh <- read.csv("ASSIST_INH_Filtered_06.18.20.csv")
options(scipen = 999)


###### Sedatives or sleeping pills (diazepam, alprazolam, flunitrazepam, midazolam, etc.)
data.sed <- read.csv("ASSIST_SED_Filtered_06.18.20.csv")
options(scipen = 999)


###### Hallucinogens(LSD,acid,mushrooms,trips,ketamine,etc.)
data.hal <- read.csv("ASSIST_HAL_Filtered_06.18.20.csv")
options(scipen = 999)


##### Demographics
data.dem <- read.csv("AST_DEM.csv")
options(scipen = 999)


### Convert data sets to dataframes

data.frame(data.etoh)

data.frame(data.tob)

data.frame(data.thc)

data.frame(data.ops)

data.frame(data.coc)

data.frame(data.amp)

data.frame(data.hal)

data.frame(data.sed)

data.frame(data.inh)

data.frame(data.dem)


head(data.thc)
head(data.ops)
head(data.tob)
head(data.amp)
head(data.hal)
head(data.sed)
head(data.inh)


### Provide colnames to dataframes

colnames(data.etoh) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY")

colnames(data.tob) <- c("PROJID", "TOB_TOT", "ATTOB3M", "ATTOBURG", "ATTOBPRB", "ATTOBWOR", "ATTOBTRY")

colnames(data.thc) <- c("PROJID", "THC_TOT", "ATTHC3M", "ATTHCURG", "ATTHCPRB", "ATTHCEXP", "ATTHCWOR", "ATTHCTRY")

colnames(data.ops) <- c("PROJID", "OPS_TOT", "ATOPS3M", "ATOPSURG", "ATOPSPRB", "ATOPSEXP", "ATOPSWOR", "ATOPSTRY", "ATDRGINJ", "ATINJFRQ")

colnames(data.coc) <- c("PROJID", "COC_TOT", "ATCOC3M", "ATCOCURG", "ATCOCPRB", "ATCOCEXP", "ATCOCWOR", "ATCOCTRY")

colnames(data.amp) <- c("PROJID", "AMP_TOT", "ATAMP3M", "ATAMPURG", "ATAMPPRB", "ATAMPEXP", "ATAMPWOR", "ATAMPTRY") 
                        
colnames(data.hal) <- c("PROJID", "HAL_TOT", "ATHAL3M", "ATHALURG", "ATHALPRB", "ATHALEXP", "ATHALWOR", "ATHALTRY")

colnames(data.sed) <- c("PROJID", "SED_TOT", "ATSED3M", "ATSEDURG", "ATSEDPRB", "ATSEDEXP", "ATSEDWOR", "ATSEDTRY")

colnames(data.inh) <- c("PROJID", "INH_TOT", "ATINH3M", "ATINHURG", "ATINHPRB", "ATINHEXP", "ATINHWOR", "ATINHTRY")

colnames(data.dem) <- c("PROJID", "GENDER", "AGE", "RACE", "EDU", "JOB", "MARTL")


##################### Merge AMP & DEM Data Sets ###############################

data.etoh_tob.dems <- merge.data.frame(data.tob, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                  by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                  suffixes = c(".x", ".y"), incomparables = NULL)
# > data.etoh_tob.dems <- merge.data.frame(data.tob, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
#                                          +                                   by.y = c("PROJID"), all = FALSE, all.x = TRUE, all.y = FALSE, sort = TRUE,
#                                          +                                   suffixes = c(".x", ".y"), incomparables = NULL)
# >

help(merge.data.frame)

data.etoh_tob.dems <- merge.data.frame(data.etoh, data.etoh_tob.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

head(data.etoh_tob.dems)

### Convert to data.frame
data.frame(data.etoh_tob.dems)


### Provide colnames to dataframes
colnames(data.etoh_tob.dems) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG",
                                  "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY",
                                  "TOB_TOT", "ATTOB3M", "ATTOBURG", "ATTOBPRB",
                                  "ATTOBWOR", "ATTOBTRY", "GENDER",
                                  "AGE", "RACE", "EDU", "JOB", "MARTL")


##################### DESCRIPTIVES ###############################

psych::describe(data.etoh_tob.dems)
# psych::describe(data.etoh_tob.dems)
# vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1433 4868986.18 2883468.94 4734816 4844011.48 3745043.15 10594 9994871 9984277  0.08
# ALC_TOT     2 1433       6.23       7.77       3       4.59       4.45     0      39      39  2.01
# ATALC3M     3 1433       2.01       1.89       2       1.84       2.97     0       6       6  0.42
# ATALCURG    4 1433       1.36       2.15       0       0.99       0.00     0       6       6  1.11
# ATALCPRB    5 1433       0.48       1.56       0       0.00       0.00     0       7       7  3.10
# ATALCEXP    6 1433       0.46       1.64       0       0.00       0.00     0       8       8  3.41
# ATALCWOR    7 1433       1.05       1.90       0       0.62       0.00     0       6       6  1.59
# ATALCTRY    8 1433       0.86       1.79       0       0.42       0.00     0       6       6  1.93
# TOB_TOT     9 1433      10.91      10.11       8      10.06      11.86     0      31      31  0.43
# ATTOB3M    10 1433       2.92       2.78       3       2.90       4.45     0       6       6  0.06
# ATTOBURG   11 1433       2.68       2.84       0       2.59       0.00     0       6       6  0.18
# ATTOBPRB   12 1433       1.00       2.25       0       0.41       0.00     0       7       7  1.91
# ATTOBWOR   13 1433       2.12       2.53       0       1.91       0.00     0       6       6  0.59
# ATTOBTRY   14 1433       2.18       2.41       3       1.98       4.45     0       6       6  0.53
# GENDER     15 1433       1.70       3.62       2       1.59       0.00     1      98      97 25.93
# AGE        16 1433      45.88      14.73      48      45.86      16.31    18      94      76 -0.02
# RACE       17 1433       2.85       1.18       3       2.68       0.00     1       7       6  1.31
# EDU        18 1433      14.29       2.96      15      14.45       2.97     0      21      21 -0.45
# JOB        19 1433       5.08      13.64       3       3.05       2.97     1      99      98  6.60
# MARTL      20 1433       3.61       1.68       5       3.71       1.48     1       6       5 -0.50
# kurtosis       se
# PROJID      -1.21 76171.44
# ALC_TOT      4.12     0.21
# ATALC3M     -0.84     0.05
# ATALCURG    -0.47     0.06
# ATALCPRB     8.10     0.04
# ATALCEXP    10.19     0.04
# ATALCWOR     1.24     0.05
# ATALCTRY     2.44     0.05
# TOB_TOT     -1.22     0.27
# ATTOB3M     -1.85     0.07
# ATTOBURG    -1.89     0.08
# ATTOBPRB     1.92     0.06
# ATTOBWOR    -1.34     0.07
# ATTOBTRY    -1.26     0.06
# GENDER     684.07     0.10
# AGE         -0.65     0.39
# RACE         2.12     0.03
# EDU          0.41     0.08
# JOB         42.54     0.36
# MARTL       -1.30     0.04
# > 


##### Create data sets for the ESEM and invariance testing

(data.etoh_tob.dems  <- subset(data.etoh_tob.dems, select = c(ATALC3M, ATALCURG, ATALCPRB, ATALCEXP, ATALCWOR,
                                                          ATALCTRY, ATTOB3M, ATTOBURG, ATTOBPRB, ATTOBWOR, ATTOBTRY,
                                                          GENDER, AGE, RACE, EDU, JOB, MARTL)))

##### Export the data 

write.table(data.etoh_tob.dems, file = "data.etoh_tob.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)
# > write.table(data.etoh_tob.dems, file = "data.etoh_tob.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)
# > 



##################################################################################################
##################################################################################################
##################################################################################################

############### CFA

############### MODEL.ETOH_TOB

AST.Etoh_Tob.model <- 'ETOH =~ NA*ATALC3M + ATALC3M + ATALCURG + ATALCPRB + ATALCEXP + ATALCWOR + ATALCTRY
                       TOB =~ NA*ATTOB3M + ATTOB3M + ATTOBURG + ATTOBPRB + ATTOBWOR + ATTOBTRY
                       ETOH_TOB =~ NA*ATALC3M + ATALC3M + ATALCWOR + ATALCTRY + ATTOBWOR + ATTOBTRY
                       ATTOBPRB ~~ ATALCPRB;
          		         ATALCURG ~~ ATALC3M;
                        
                        ETOH ~~ 1*ETOH
                        TOB ~~ 1*TOB
                        ETOH_TOB ~~ 1*ETOH_TOB'

############### MODEL.fit


data.etoh_tob.dems.fit <- lavaan::cfa(AST.Etoh_Tob.model, 
                                 data.etoh_tob.dems, 
                                 std.lv=TRUE, 
                                 estimator = "WLSMV", 
                                 ordered = c("ATALC3M", "ATALCURG",
                                             "ATALCPRB", "ATALCEXP",
                                             "ATALCWOR", "ATALCTRY",
                                             "ATTOB3M", "ATTOBURG",
                                             "ATTOBPRB", "ATTOBWOR",
                                             "ATTOBTRY"))
# > data.etoh_tob.dems.fit <- lavaan::cfa(AST.Etoh_Tob.model, 
#                                         +                                  data.etoh_tob.dems, 
#                                         +                                  std.lv=TRUE, 
#                                         +                                  estimator = "WLSMV", 
#                                         +                                  ordered = c("ATALC3M", "ATALCURG",
#                                                                                        +                                              "ATALCPRB", "ATALCEXP",
#                                                                                        +                                              "ATALCWOR", "ATALCTRY",
#                                                                                        +                                              "ATTOB3M", "ATTOBURG",
#                                                                                        +                                              "ATTOBPRB", "ATTOBWOR",
#                                                                                        +                                              "ATTOBTRY"))

  
############### MODEL.summary

summary(data.etoh_tob.dems.fit, standardized = TRUE, fit.measures = TRUE)
# > summary(data.etoh_tob.dems.fit, standardized = TRUE, fit.measures = TRUE)
# lavaan 0.6-6 ended normally after 31 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         57
# 
# Number of observations                          1433
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                96.786     143.399
# Degrees of freedom                                34          34
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.723
# Shift parameter                                            9.585
# simple second-order correction                             
# 
# Model Test Baseline Model:
#   
#   Test statistic                            100914.796   55740.829
# Degrees of freedom                                55          55
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.811
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.999       0.998
# Tucker-Lewis Index (TLI)                       0.999       0.997
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.036       0.047
# 90 Percent confidence interval - lower         0.028       0.040
# 90 Percent confidence interval - upper         0.044       0.056
# P-value RMSEA <= 0.05                          0.997       0.689
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.049       0.049
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M           0.663    0.039   17.194    0.000    0.663    0.663
# ATALCURG          0.759    0.023   32.917    0.000    0.759    0.759
# ATALCPRB          0.908    0.021   44.286    0.000    0.908    0.908
# ATALCEXP          0.974    0.020   48.079    0.000    0.974    0.974
# ATALCWOR          0.689    0.038   18.122    0.000    0.689    0.689
# ATALCTRY          0.684    0.039   17.420    0.000    0.684    0.684
# TOB =~                                                                
#   ATTOB3M           0.986    0.011   91.500    0.000    0.986    0.986
# ATTOBURG          0.987    0.011   93.092    0.000    0.987    0.987
# ATTOBPRB          0.784    0.023   34.382    0.000    0.784    0.784
# ATTOBWOR          0.622    0.029   21.170    0.000    0.622    0.622
# ATTOBTRY          0.664    0.029   22.781    0.000    0.664    0.664
# ETOH_TOB =~                                                           
#   ATALC3M          -0.405    0.040  -10.096    0.000   -0.405   -0.405
# ATALCWOR          0.535    0.036   14.796    0.000    0.535    0.535
# ATALCTRY          0.540    0.036   14.966    0.000    0.540    0.540
# ATTOBWOR          0.444    0.031   14.205    0.000    0.444    0.444
# ATTOBTRY          0.433    0.031   14.123    0.000    0.433    0.433
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.337    0.035    9.721    0.000    0.337    1.299
# .ATALC3M ~~                                                            
#   .ATALCURG          0.286    0.028   10.123    0.000    0.286    0.737
# ETOH ~~                                                               
#   TOB               0.376    0.034   11.094    0.000    0.376    0.376
# ETOH_TOB         -0.074    0.073   -1.012    0.312   -0.074   -0.074
# TOB ~~                                                                
#   ETOH_TOB          0.089    0.054    1.663    0.096    0.089    0.089
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# .ATTOB3M           0.000                               0.000    0.000
# .ATTOBURG          0.000                               0.000    0.000
# .ATTOBPRB          0.000                               0.000    0.000
# .ATTOBWOR          0.000                               0.000    0.000
# .ATTOBTRY          0.000                               0.000    0.000
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.274    0.034   -8.150    0.000   -0.274   -0.274
# ATALC3M|t2        0.207    0.033    6.202    0.000    0.207    0.207
# ATALC3M|t3        0.724    0.036   19.853    0.000    0.724    0.724
# ATALC3M|t4        1.457    0.050   29.334    0.000    1.457    1.457
# ATALCURG|t1       0.510    0.035   14.688    0.000    0.510    0.510
# ATALCURG|t2       0.770    0.037   20.850    0.000    0.770    0.770
# ATALCURG|t3       1.018    0.040   25.345    0.000    1.018    1.018
# ATALCURG|t4       1.472    0.050   29.379    0.000    1.472    1.472
# ATALCPRB|t1       1.345    0.047   28.816    0.000    1.345    1.345
# ATALCPRB|t2       1.509    0.051   29.461    0.000    1.509    1.509
# ATALCPRB|t3       1.778    0.061   29.007    0.000    1.778    1.778
# ATALCPRB|t4       2.110    0.080   26.282    0.000    2.110    2.110
# ATALCEXP|t1       1.432    0.049   29.249    0.000    1.432    1.432
# ATALCEXP|t2       1.729    0.059   29.219    0.000    1.729    1.729
# ATALCEXP|t3       1.923    0.069   28.068    0.000    1.923    1.923
# ATALCEXP|t4       2.240    0.091   24.727    0.000    2.240    2.240
# ATALCWOR|t1       0.630    0.036   17.676    0.000    0.630    0.630
# ATALCWOR|t2       1.362    0.047   28.919    0.000    1.362    1.362
# ATALCTRY|t1       0.797    0.037   21.393    0.000    0.797    0.797
# ATALCTRY|t2       1.442    0.049   29.285    0.000    1.442    1.442
# ATTOB3M|t1       -0.168    0.033   -5.042    0.000   -0.168   -0.168
# ATTOB3M|t2       -0.006    0.033   -0.185    0.853   -0.006   -0.006
# ATTOB3M|t3        0.094    0.033    2.825    0.005    0.094    0.094
# ATTOB3M|t4        0.225    0.033    6.728    0.000    0.225    0.225
# ATTOBURG|t1       0.045    0.033    1.347    0.178    0.045    0.045
# ATTOBURG|t2       0.143    0.033    4.303    0.000    0.143    0.143
# ATTOBURG|t3       0.198    0.033    5.938    0.000    0.198    0.198
# ATTOBURG|t4       0.347    0.034   10.252    0.000    0.347    0.347
# ATTOBPRB|t1       0.939    0.039   24.076    0.000    0.939    0.939
# ATTOBPRB|t2       1.114    0.042   26.662    0.000    1.114    1.114
# ATTOBPRB|t3       1.283    0.045   28.379    0.000    1.283    1.283
# ATTOBPRB|t4       1.422    0.049   29.211    0.000    1.422    1.422
# ATTOBWOR|t1       0.108    0.033    3.248    0.001    0.108    0.108
# ATTOBWOR|t2       0.671    0.036   18.643    0.000    0.671    0.671
# ATTOBTRY|t1      -0.010    0.033   -0.290    0.771   -0.010   -0.010
# ATTOBTRY|t2       0.759    0.037   20.602    0.000    0.759    0.759
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           0.356                               0.356    0.356
# .ATALCURG          0.424                               0.424    0.424
# .ATALCPRB          0.175                               0.175    0.175
# .ATALCEXP          0.051                               0.051    0.051
# .ATALCWOR          0.293                               0.293    0.293
# .ATALCTRY          0.295                               0.295    0.295
# .ATTOB3M           0.028                               0.028    0.028
# .ATTOBURG          0.025                               0.025    0.025
# .ATTOBPRB          0.385                               0.385    0.385
# .ATTOBWOR          0.367                               0.367    0.367
# .ATTOBTRY          0.320                               0.320    0.320
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# ATTOB3M           1.000                               1.000    1.000
# ATTOBURG          1.000                               1.000    1.000
# ATTOBPRB          1.000                               1.000    1.000
# ATTOBWOR          1.000                               1.000    1.000
# ATTOBTRY          1.000                               1.000    1.000
# 
# >
  
modindices(data.etoh_tob.dems.fit)
# > modindices(data.etoh_tob.dems.fit)
# lhs op      rhs     mi    epc sepc.lv sepc.all sepc.nox
# 97      ETOH =~  ATTOB3M  1.254 -0.039  -0.039   -0.039   -0.039
# 98      ETOH =~ ATTOBURG  2.005 -0.050  -0.050   -0.050   -0.050
# 99      ETOH =~ ATTOBPRB 23.491  0.167   0.167    0.167    0.167
# 100     ETOH =~ ATTOBWOR  6.958 -0.096  -0.096   -0.096   -0.096
# 101     ETOH =~ ATTOBTRY  6.958  0.094   0.094    0.094    0.094
# 102      TOB =~  ATALC3M 22.762 -0.187  -0.187   -0.187   -0.187
# 103      TOB =~ ATALCURG  1.239  0.032   0.032    0.032    0.032
# 104      TOB =~ ATALCPRB  9.021  0.120   0.120    0.120    0.120
# 105      TOB =~ ATALCEXP 12.071  0.134   0.134    0.134    0.134
# 106      TOB =~ ATALCWOR  1.955 -0.063  -0.063   -0.063   -0.063
# 107      TOB =~ ATALCTRY  7.699 -0.127  -0.127   -0.127   -0.127
# 108 ETOH_TOB =~ ATALCURG  7.728 -0.131  -0.131   -0.131   -0.131
# 109 ETOH_TOB =~ ATALCPRB  0.397  0.030   0.030    0.030    0.030
# 110 ETOH_TOB =~ ATALCEXP  3.450  0.092   0.092    0.092    0.092
# 111 ETOH_TOB =~  ATTOB3M  4.516 -0.102  -0.102   -0.102   -0.102
# 112 ETOH_TOB =~ ATTOBURG  0.040 -0.010  -0.010   -0.010   -0.010
# 113 ETOH_TOB =~ ATTOBPRB  6.017  0.106   0.106    0.106    0.106
# 114  ATALC3M ~~ ATALCPRB  3.923  0.097   0.097    0.390    0.390
# 115  ATALC3M ~~ ATALCEXP  0.007  0.004   0.004    0.032    0.032
# 116  ATALC3M ~~ ATALCWOR  2.990  0.082   0.082    0.253    0.253
# 117  ATALC3M ~~ ATALCTRY  0.316 -0.027  -0.027   -0.084   -0.084
# 118  ATALC3M ~~  ATTOB3M  1.805 -0.050  -0.050   -0.503   -0.503
# 119  ATALC3M ~~ ATTOBURG  9.338 -0.117  -0.117   -1.244   -1.244
# 120  ATALC3M ~~ ATTOBPRB  3.386 -0.074  -0.074   -0.199   -0.199
# 121  ATALC3M ~~ ATTOBWOR  0.440 -0.032  -0.032   -0.088   -0.088
# 122  ATALC3M ~~ ATTOBTRY  1.135  0.050   0.050    0.149    0.149
# 123 ATALCURG ~~ ATALCPRB  1.207  0.047   0.047    0.173    0.173
# 124 ATALCURG ~~ ATALCEXP  0.021  0.007   0.007    0.046    0.046
# 125 ATALCURG ~~ ATALCWOR  1.394 -0.046  -0.046   -0.130   -0.130
# 126 ATALCURG ~~ ATALCTRY  3.425 -0.076  -0.076   -0.214   -0.214
# 127 ATALCURG ~~  ATTOB3M  0.975  0.039   0.039    0.354    0.354
# 128 ATALCURG ~~ ATTOBURG  4.168  0.079   0.079    0.767    0.767
# 129 ATALCURG ~~ ATTOBPRB  0.596  0.034   0.034    0.083    0.083
# 130 ATALCURG ~~ ATTOBWOR  3.075 -0.071  -0.071   -0.179   -0.179
# 131 ATALCURG ~~ ATTOBTRY  0.290 -0.021  -0.021   -0.057   -0.057
# 132 ATALCPRB ~~ ATALCEXP  9.049 -0.149  -0.149   -1.578   -1.578
# 133 ATALCPRB ~~ ATALCWOR  0.016 -0.006  -0.006   -0.025   -0.025
# 134 ATALCPRB ~~ ATALCTRY  0.050  0.011   0.011    0.046    0.046
# 135 ATALCPRB ~~  ATTOB3M  0.002  0.003   0.003    0.039    0.039
# 136 ATALCPRB ~~ ATTOBURG  0.002 -0.002  -0.002   -0.033   -0.033
# 137 ATALCPRB ~~ ATTOBWOR  0.001  0.001   0.001    0.005    0.005
# 138 ATALCPRB ~~ ATTOBTRY  5.651  0.120   0.120    0.506    0.506
# 139 ATALCEXP ~~ ATALCWOR  0.412 -0.029  -0.029   -0.234   -0.234
# 140 ATALCEXP ~~ ATALCTRY  2.753  0.075   0.075    0.610    0.610
# 141 ATALCEXP ~~  ATTOB3M  0.282  0.031   0.031    0.809    0.809
# 142 ATALCEXP ~~ ATTOBURG  0.002 -0.002  -0.002   -0.067   -0.067
# 143 ATALCEXP ~~ ATTOBPRB 22.016  0.242   0.242    1.723    1.723
# 144 ATALCEXP ~~ ATTOBWOR  0.094 -0.017  -0.017   -0.124   -0.124
# 145 ATALCEXP ~~ ATTOBTRY  4.160  0.108   0.108    0.846    0.846
# 146 ATALCWOR ~~ ATALCTRY  6.923  0.162   0.162    0.549    0.549
# 147 ATALCWOR ~~  ATTOB3M  1.840 -0.059  -0.059   -0.651   -0.651
# 148 ATALCWOR ~~ ATTOBURG  0.620 -0.035  -0.035   -0.404   -0.404
# 149 ATALCWOR ~~ ATTOBPRB  1.372  0.054   0.054    0.161    0.161
# 150 ATALCWOR ~~ ATTOBWOR  2.814  0.081   0.081    0.248    0.248
# 151 ATALCWOR ~~ ATTOBTRY  4.752 -0.107  -0.107   -0.349   -0.349
# 152 ATALCTRY ~~  ATTOB3M  3.337 -0.084  -0.084   -0.926   -0.926
# 153 ATALCTRY ~~ ATTOBURG  2.135 -0.068  -0.068   -0.794   -0.794
# 154 ATALCTRY ~~ ATTOBPRB  1.863  0.065   0.065    0.193    0.193
# 155 ATALCTRY ~~ ATTOBWOR 23.141 -0.254  -0.254   -0.773   -0.773
# 156 ATALCTRY ~~ ATTOBTRY  5.759  0.118   0.118    0.383    0.383
# 157  ATTOB3M ~~ ATTOBURG 14.847  0.175   0.175    6.598    6.598
# 158  ATTOB3M ~~ ATTOBPRB  1.892 -0.061  -0.061   -0.582   -0.582
# 159  ATTOB3M ~~ ATTOBWOR  0.261 -0.019  -0.019   -0.185   -0.185
# 160  ATTOB3M ~~ ATTOBTRY  2.791 -0.062  -0.062   -0.650   -0.650
# 161 ATTOBURG ~~ ATTOBPRB  5.005 -0.093  -0.093   -0.950   -0.950
# 162 ATTOBURG ~~ ATTOBWOR  0.120 -0.013  -0.013   -0.133   -0.133
# 163 ATTOBURG ~~ ATTOBTRY  0.853 -0.034  -0.034   -0.379   -0.379
# 164 ATTOBPRB ~~ ATTOBWOR  2.208  0.054   0.054    0.144    0.144
# 165 ATTOBPRB ~~ ATTOBTRY  0.367  0.022   0.022    0.062    0.062
# 166 ATTOBWOR ~~ ATTOBTRY  2.407  0.066   0.066    0.194    0.194
# Warning message:
#   In lav_start_check_cov(lavpartable = lavpartable, start = START) :
#   lavaan WARNING: starting values imply a correlation larger than 1;
# variables involved are:  ATALCPRB   ATTOBPRB 
# > 


##################### ##################### ##################### ##################### 
##################### ##################### ##################### ##################### 


##################### INVARIANCE TESTS

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.Etoh_Tob.model.RACE.fit <- lavaan::cfa(AST.Etoh_Tob.model, data.etoh_tob.dems, std.lv=TRUE, estimator = "WLSMV", group = "RACE")
                         


##################  ##################  Adjustment to RACE  ################## ##################
################## ################## ################## ################## ##################

table(data.etoh_tob.dems$RACE)
# > table(data.etoh_tob.dems$RACE)
# 
# 1   2   3   4   5   6   7 
# 109 403 771   7  30 109   4 
# >


### Filter the data for RACE to only include Latinx, Whites, AA, Milt 
### Multiple arguments are equivalent to and

### Filter out Native Americans/Indigenious for RACE = RACE != 4
data.etoh_tob.dems.RACE <- dplyr::filter(data.etoh_tob.dems, RACE != 4) #Extract rows that meet logical criteria.

### Filter out Asians for RACE = RACE != 5

data.etoh_tob.dems.RACE.02 <- dplyr::filter(data.etoh_tob.dems.RACE, RACE != 5) #Extract rows that meet logical criteria.


### Filter out not specified for RACE = RACE != 7

data.etoh_tob.dems.RACE.03 <- dplyr::filter(data.etoh_tob.dems.RACE.02, RACE != 7) #Extract rows that meet logical criteria.

### Filter out Latinx for RACE = RACE != 1

data.etoh_tob.dems.RACE.04 <- dplyr::filter(data.etoh_tob.dems.RACE.03, RACE != 1) #Extract rows that meet logical criteria.

### Filter out multi-racial for RACE = RACE != 6

data.etoh_tob.dems.RACE.05 <- dplyr::filter(data.etoh_tob.dems.RACE.04, RACE != 6) #Extract rows that meet logical criteria.


### RACE is Whites(2) & AAs(3)
table(data.etoh_tob.dems.RACE.05$RACE)
# > table(data.etoh_tob.dems.RACE.05 $RACE)
# 
# 2   3 
# 403 771 
# >

##################### INVARIANCE TESTS_RACE

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.Etoh_Tob.model.RACE.fit <- lavaan::cfa(AST.Etoh_Tob.model, data.etoh_tob.dems.RACE.05, std.lv=TRUE, estimator = "WLSMV", group = "RACE")


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.RACE.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 135 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# 
# Number of observations per group:                   
#   2                                              403
# 3                                              771
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                91.256     181.600
# Degrees of freedom                                68          68
# P-value (Chi-square)                           0.031       0.000
# Scaling correction factor                                  0.578
# Shift parameter for each group:                                 
#   2                                                      8.176
# 3                                                     15.642
# simple second-order correction                             
# Test statistic for each group:
#   2                                           45.685      87.165
# 3                                           45.571      94.435
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8535.069    3450.925
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.522
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.966
# Tucker-Lewis Index (TLI)                       0.996       0.945
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.024       0.053
# 90 Percent confidence interval - lower         0.008       0.044
# 90 Percent confidence interval - upper         0.036       0.063
# P-value RMSEA <= 0.05                          1.000       0.264
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.041       0.041
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#                  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M           1.071    0.115    9.318    0.000    1.071    0.562
# ATALCURG          1.480    0.112   13.187    0.000    1.480    0.667
# ATALCPRB          1.073    0.150    7.162    0.000    1.073    0.648
# ATALCEXP          1.077    0.172    6.279    0.000    1.077    0.653
# ATALCWOR          1.101    0.141    7.822    0.000    1.101    0.547
# ATALCTRY          1.099    0.144    7.654    0.000    1.099    0.559
# TOB =~                                                                
#   ATTOB3M           2.595    0.054   48.029    0.000    2.595    0.942
# ATTOBURG          2.679    0.049   54.490    0.000    2.679    0.943
# ATTOBPRB          1.192    0.122    9.777    0.000    1.192    0.524
# ATTOBWOR          1.397    0.134   10.445    0.000    1.397    0.535
# ATTOBTRY          1.427    0.147    9.687    0.000    1.427    0.574

# ETOH_TOB =~                                                           
#   ATALC3M          -0.657    0.121   -5.421    0.000   -0.657   -0.345
# ATALCWOR          0.432    0.153    2.828    0.005    0.432    0.214
# ATALCTRY          0.485    0.129    3.751    0.000    0.485    0.247
# ATTOBWOR          0.887    0.152    5.843    0.000    0.887    0.340
# ATTOBTRY          1.203    0.163    7.384    0.000    1.203    0.484
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.994    0.257    3.873    0.000    0.994    0.407
# .ATALC3M ~~                                                            
#   .ATALCURG          1.416    0.223    6.344    0.000    1.416    0.580
# ETOH ~~                                                               
#   TOB               0.334    0.050    6.659    0.000    0.334    0.334
# ETOH_TOB          0.092    0.094    0.975    0.330    0.092    0.092
# TOB ~~                                                                
#   ETOH_TOB          0.097    0.110    0.881    0.378    0.097    0.097
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           2.144    0.095   22.595    0.000    2.144    1.125
# .ATALCURG          1.548    0.111   14.012    0.000    1.548    0.698
# .ATALCPRB          0.531    0.082    6.440    0.000    0.531    0.321
# .ATALCEXP          0.452    0.082    5.498    0.000    0.452    0.274
# .ATALCWOR          1.206    0.100   12.035    0.000    1.206    0.599
# .ATALCTRY          1.072    0.098   10.953    0.000    1.072    0.545
# .ATTOB3M           2.960    0.137   21.572    0.000    2.960    1.074
# .ATTOBURG          2.772    0.141   19.604    0.000    2.772    0.976
# .ATTOBPRB          1.022    0.113    9.029    0.000    1.022    0.450
# .ATTOBWOR          2.323    0.130   17.866    0.000    2.323    0.890
# .ATTOBTRY          2.308    0.124   18.660    0.000    2.308    0.929
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.182    0.252    8.643    0.000    2.182    0.601
# .ATALCURG          2.734    0.314    8.698    0.000    2.734    0.555
# .ATALCPRB          1.592    0.234    6.793    0.000    1.592    0.580
# .ATALCEXP          1.560    0.237    6.587    0.000    1.560    0.573
# .ATALCWOR          2.565    0.283    9.078    0.000    2.565    0.633
# .ATALCTRY          2.322    0.259    8.954    0.000    2.322    0.601
# .ATTOB3M           0.860    0.218    3.953    0.000    0.860    0.113
# .ATTOBURG          0.886    0.214    4.142    0.000    0.886    0.110
# .ATTOBPRB          3.749    0.285   13.138    0.000    3.749    0.725
# .ATTOBWOR          3.838    0.351   10.935    0.000    3.838    0.563
# .ATTOBTRY          2.354    0.409    5.762    0.000    2.354    0.382
# 
# 
# Group 2 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M           0.805    0.077   10.490    0.000    0.805    0.431
# ATALCURG          1.218    0.091   13.317    0.000    1.218    0.575
# ATALCPRB          1.103    0.110   10.021    0.000    1.103    0.741
# ATALCEXP          1.227    0.131    9.352    0.000    1.227    0.768
# ATALCWOR          1.124    0.096   11.674    0.000    1.124    0.626
# ATALCTRY          0.915    0.096    9.513    0.000    0.915    0.557
# TOB =~                                                                
#   ATTOB3M           2.521    0.045   56.072    0.000    2.521    0.909
# ATTOBURG          2.719    0.038   71.397    0.000    2.719    0.956
# ATTOBPRB          1.140    0.084   13.514    0.000    1.140    0.505
# ATTOBWOR          1.333    0.108   12.394    0.000    1.333    0.531
# ATTOBTRY          1.329    0.102   13.003    0.000    1.329    0.557
# ETOH_TOB =~                                                           
#   ATALC3M          -0.444    0.081   -5.499    0.000   -0.444   -0.238
# ATALCWOR          0.749    0.096    7.804    0.000    0.749    0.417
# ATALCTRY          0.557    0.085    6.588    0.000    0.557    0.339
# ATTOBWOR          1.181    0.111   10.619    0.000    1.181    0.470
# ATTOBTRY          1.087    0.109    9.984    0.000    1.087    0.455
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.868    0.167    5.207    0.000    0.868    0.445
# .ATALC3M ~~                                                            
#   .ATALCURG          1.496    0.155    9.672    0.000    1.496    0.536
# ETOH ~~                                                               
#   TOB               0.260    0.039    6.618    0.000    0.260    0.260
# ETOH_TOB         -0.071    0.082   -0.863    0.388   -0.071   -0.071
# TOB ~~                                                                
#   ETOH_TOB          0.077    0.076    1.007    0.314    0.077    0.077
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           1.983    0.067   29.462    0.000    1.983    1.061
# .ATALCURG          1.336    0.076   17.506    0.000    1.336    0.631
# .ATALCPRB          0.428    0.054    7.981    0.000    0.428    0.287
# .ATALCEXP          0.442    0.058    7.692    0.000    0.442    0.277
# .ATALCWOR          0.942    0.065   14.553    0.000    0.942    0.524
# .ATALCTRY          0.735    0.059   12.434    0.000    0.735    0.448
# .ATTOB3M           2.969    0.100   29.711    0.000    2.969    1.070
# .ATTOBURG          2.681    0.102   26.166    0.000    2.681    0.943
# .ATTOBPRB          1.012    0.081   12.440    0.000    1.012    0.448
# .ATTOBWOR          2.023    0.090   22.373    0.000    2.023    0.806
# .ATTOBTRY          2.105    0.086   24.484    0.000    2.105    0.882
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.596    0.148   17.486    0.000    2.596    0.743
# .ATALCURG          3.005    0.230   13.044    0.000    3.005    0.670
# .ATALCPRB          1.001    0.142    7.058    0.000    1.001    0.451
# .ATALCEXP          1.044    0.167    6.252    0.000    1.044    0.410
# .ATALCWOR          1.520    0.172    8.863    0.000    1.520    0.471
# .ATALCTRY          1.621    0.151   10.706    0.000    1.621    0.601
# .ATTOB3M           1.341    0.185    7.253    0.000    1.341    0.174
# .ATTOBURG          0.699    0.180    3.884    0.000    0.699    0.086
# .ATTOBPRB          3.798    0.225   16.912    0.000    3.798    0.745
# .ATTOBWOR          2.892    0.270   10.714    0.000    2.892    0.459
# .ATTOBTRY          2.527    0.248   10.194    0.000    2.527    0.444
# 
# >


############### Metric invariance

AST.Etoh_Tob.model.RACE.METRIC.fit <- lavaan::cfa(AST.Etoh_Tob.model, data.etoh_tob.dems.RACE.05, std.lv=TRUE, estimator = "WLSMV", group = "RACE", group.equal=c("loadings"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.METRIC.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.RACE.METRIC.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 114 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# Number of equality constraints                    16
# 
# Number of observations per group:                   
#   2                                              403
# 3                                              771
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               108.687     143.289
# Degrees of freedom                                84          84
# P-value (Chi-square)                           0.036       0.000
# Scaling correction factor                                  1.002
# Shift parameter for each group:                                 
#   2                                                     11.967
# 3                                                     22.894
# simple second-order correction                             
# Test statistic for each group:
#   2                                           57.644      69.473
# 3                                           51.043      73.816
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8535.069    3450.925
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.522
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.982
# Tucker-Lewis Index (TLI)                       0.996       0.977
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.022       0.035
# 90 Percent confidence interval - lower         0.006       0.025
# 90 Percent confidence interval - upper         0.034       0.044
# P-value RMSEA <= 0.05                          1.000       0.997
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.044       0.044
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.883    0.063   14.107    0.000    0.883    0.463
# ATALCUR (.p2.)    1.322    0.072   18.402    0.000    1.322    0.596
# ATALCPR (.p3.)    1.094    0.089   12.249    0.000    1.094    0.661
# ATALCEX (.p4.)    1.169    0.105   11.131    0.000    1.169    0.708
# ATALCWO (.p5.)    1.095    0.081   13.517    0.000    1.095    0.544
# ATALCTR (.p6.)    0.964    0.080   11.991    0.000    0.964    0.490
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.567    0.000    2.549    0.925
# ATTOBUR (.p8.)    2.703    0.030   89.874    0.000    2.703    0.952
# ATTOBPR (.p9.)    1.158    0.069   16.676    0.000    1.158    0.509
# ATTOBWO (.10.)    1.351    0.087   15.544    0.000    1.351    0.518
# ATTOBTR (.11.)    1.369    0.085   16.196    0.000    1.369    0.551
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.470    0.067   -6.999    0.000   -0.470   -0.247
# ATALCWO (.13.)    0.686    0.081    8.507    0.000    0.686    0.341
# ATALCTR (.14.)    0.558    0.071    7.865    0.000    0.558    0.284
# ATTOBWO (.15.)    1.113    0.090   12.303    0.000    1.113    0.426
# ATTOBTR (.16.)    1.097    0.090   12.196    0.000    1.097    0.442
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.991    0.280    3.533    0.000    0.991    0.407
# .ATALC3M ~~                                                            
#   .ATALCURG          1.775    0.223    7.955    0.000    1.775    0.609
# ETOH ~~                                                               
#   TOB               0.340    0.060    5.654    0.000    0.340    0.340
# ETOH_TOB          0.050    0.091    0.545    0.586    0.050    0.050
# TOB ~~                                                                
#   ETOH_TOB          0.108    0.088    1.226    0.220    0.108    0.108
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           2.144    0.095   22.595    0.000    2.144    1.125
# .ATALCURG          1.548    0.111   14.012    0.000    1.548    0.698
# .ATALCPRB          0.531    0.082    6.440    0.000    0.531    0.321
# .ATALCEXP          0.452    0.082    5.498    0.000    0.452    0.274
# .ATALCWOR          1.206    0.100   12.035    0.000    1.206    0.599
# .ATALCTRY          1.072    0.098   10.953    0.000    1.072    0.545
# .ATTOB3M           2.960    0.137   21.572    0.000    2.960    1.074
# .ATTOBURG          2.772    0.141   19.604    0.000    2.772    0.976
# .ATTOBPRB          1.022    0.113    9.029    0.000    1.022    0.450
# .ATTOBWOR          2.323    0.130   17.866    0.000    2.323    0.890
# .ATTOBTRY          2.308    0.124   18.660    0.000    2.308    0.929
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.672    0.220   12.138    0.000    2.672    0.736
# .ATALCURG          3.178    0.275   11.540    0.000    3.178    0.645
# .ATALCPRB          1.546    0.355    4.351    0.000    1.546    0.564
# .ATALCEXP          1.355    0.398    3.404    0.001    1.355    0.498
# .ATALCWOR          2.305    0.293    7.879    0.000    2.305    0.569
# .ATALCTRY          2.569    0.288    8.922    0.000    2.569    0.665
# .ATTOB3M           1.098    0.189    5.798    0.000    1.098    0.145
# .ATTOBURG          0.758    0.169    4.485    0.000    0.758    0.094
# .ATTOBPRB          3.830    0.439    8.719    0.000    3.830    0.741
# .ATTOBWOR          3.426    0.299   11.462    0.000    3.426    0.503
# .ATTOBTRY          2.766    0.287    9.651    0.000    2.766    0.448
# 
# 
# Group 2 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.883    0.063   14.107    0.000    0.883    0.472
# ATALCUR (.p2.)    1.322    0.072   18.402    0.000    1.322    0.624
# ATALCPR (.p3.)    1.094    0.089   12.249    0.000    1.094    0.735
# ATALCEX (.p4.)    1.169    0.105   11.131    0.000    1.169    0.732
# ATALCWO (.p5.)    1.095    0.081   13.517    0.000    1.095    0.610
# ATALCTR (.p6.)    0.964    0.080   11.991    0.000    0.964    0.587
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.567    0.000    2.549    0.919
# ATTOBUR (.p8.)    2.703    0.030   89.874    0.000    2.703    0.950
# ATTOBPR (.p9.)    1.158    0.069   16.676    0.000    1.158    0.513
# ATTOBWO (.10.)    1.351    0.087   15.544    0.000    1.351    0.538
# ATTOBTR (.11.)    1.369    0.085   16.196    0.000    1.369    0.573
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.470    0.067   -6.999    0.000   -0.470   -0.251
# ATALCWO (.13.)    0.686    0.081    8.507    0.000    0.686    0.382
# ATALCTR (.14.)    0.558    0.071    7.865    0.000    0.558    0.340
# ATTOBWO (.15.)    1.113    0.090   12.303    0.000    1.113    0.443
# ATTOBTR (.16.)    1.097    0.090   12.196    0.000    1.097    0.460
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.866    0.173    4.993    0.000    0.866    0.442
# .ATALC3M ~~                                                            
#   .ATALCURG          1.304    0.161    8.098    0.000    1.304    0.505
# ETOH ~~                                                               
#   TOB               0.260    0.040    6.494    0.000    0.260    0.260
# ETOH_TOB         -0.070    0.081   -0.866    0.387   -0.070   -0.070
# TOB ~~                                                                
#   ETOH_TOB          0.067    0.070    0.967    0.334    0.067    0.067
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           1.983    0.067   29.462    0.000    1.983    1.061
# .ATALCURG          1.336    0.076   17.506    0.000    1.336    0.631
# .ATALCPRB          0.428    0.054    7.981    0.000    0.428    0.287
# .ATALCEXP          0.442    0.058    7.692    0.000    0.442    0.277
# .ATALCWOR          0.942    0.065   14.553    0.000    0.942    0.524
# .ATALCTRY          0.735    0.059   12.434    0.000    0.735    0.448
# .ATTOB3M           2.969    0.100   29.711    0.000    2.969    1.070
# .ATTOBURG          2.681    0.102   26.166    0.000    2.681    0.943
# .ATTOBPRB          1.012    0.081   12.440    0.000    1.012    0.448
# .ATTOBWOR          2.023    0.090   22.373    0.000    2.023    0.806
# .ATTOBTRY          2.105    0.086   24.484    0.000    2.105    0.882
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.433    0.150   16.241    0.000    2.433    0.697
# .ATALCURG          2.742    0.228   12.050    0.000    2.742    0.611
# .ATALCPRB          1.020    0.198    5.139    0.000    1.020    0.460
# .ATALCEXP          1.183    0.246    4.801    0.000    1.183    0.464
# .ATALCWOR          1.662    0.184    9.050    0.000    1.662    0.515
# .ATALCTRY          1.531    0.175    8.750    0.000    1.531    0.568
# .ATTOB3M           1.198    0.156    7.670    0.000    1.198    0.156
# .ATTOBURG          0.786    0.152    5.168    0.000    0.786    0.097
# .ATTOBPRB          3.757    0.280   13.397    0.000    3.757    0.737
# .ATTOBWOR          3.036    0.237   12.810    0.000    3.036    0.482
# .ATTOBTRY          2.417    0.228   10.586    0.000    2.417    0.424
# 
# >

################# Chi-square difference test between Configural invariance and metric invariance. 

anova(AST.Etoh_Tob.model.RACE.fit, AST.Etoh_Tob.model.RACE.METRIC.fit)
# > anova(AST.Etoh_Tob.model.RACE.fit, AST.Etoh_Tob.model.RACE.METRIC.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC   Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.RACE.fit        68          91.256                              
# AST.Etoh_Tob.model.RACE.METRIC.fit 84         108.687     13.775      16     0.6154
# > 


################### scalar invariance

AST.Etoh_Tob.model.RACE.SCALAR.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                   data.etoh_tob.dems.RACE.05,
                                                   std.lv=TRUE,
                                                   estimator = "WLSMV",
                                                   group = "RACE",
                                                   group.equal=c("loadings", "intercepts"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.RACE.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 125 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         89
# Number of equality constraints                    27
# 
# Number of observations per group:                   
#   2                                              403
# 3                                              771
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               113.446     152.813
# Degrees of freedom                                92          92
# P-value (Chi-square)                           0.064       0.000
# Scaling correction factor                                  0.993
# Shift parameter for each group:                                 
#   2                                                     13.233
# 3                                                     25.317
# simple second-order correction                             
# Test statistic for each group:
#   2                                           60.639      74.309
# 3                                           52.807      78.504
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8535.069    3450.925
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.522
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.982
# Tucker-Lewis Index (TLI)                       0.997       0.978
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.020       0.034
# 90 Percent confidence interval - lower         0.000       0.024
# 90 Percent confidence interval - upper         0.031       0.043
# P-value RMSEA <= 0.05                          1.000       0.999
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.045       0.045
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.883    0.062   14.159    0.000    0.883    0.463
# ATALCUR (.p2.)    1.327    0.072   18.437    0.000    1.327    0.598
# ATALCPR (.p3.)    1.093    0.089   12.234    0.000    1.093    0.660
# ATALCEX (.p4.)    1.163    0.105   11.064    0.000    1.163    0.705
# ATALCWO (.p5.)    1.095    0.081   13.448    0.000    1.095    0.544
# ATALCTR (.p6.)    0.966    0.081   11.978    0.000    0.966    0.492
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.564    0.000    2.549    0.925
# ATTOBUR (.p8.)    2.703    0.030   89.853    0.000    2.703    0.952
# ATTOBPR (.p9.)    1.158    0.069   16.673    0.000    1.158    0.509
# ATTOBWO (.10.)    1.353    0.088   15.462    0.000    1.353    0.518
# ATTOBTR (.11.)    1.372    0.085   16.233    0.000    1.372    0.552
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.457    0.066   -6.881    0.000   -0.457   -0.240
# ATALCWO (.13.)    0.695    0.081    8.624    0.000    0.695    0.345
# ATALCTR (.14.)    0.573    0.071    8.051    0.000    0.573    0.291
# ATTOBWO (.15.)    1.118    0.090   12.397    0.000    1.118    0.428
# ATTOBTR (.16.)    1.090    0.089   12.226    0.000    1.090    0.439
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.992    0.280    3.538    0.000    0.992    0.407
# .ATALC3M ~~                                                            
#   .ATALCURG          1.766    0.222    7.940    0.000    1.766    0.607
# ETOH ~~                                                               
#   TOB               0.339    0.060    5.655    0.000    0.339    0.339
# ETOH_TOB          0.045    0.092    0.487    0.626    0.045    0.045
# TOB ~~                                                                
#   ETOH_TOB          0.106    0.088    1.194    0.233    0.106    0.106
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.050    0.077   26.577    0.000    2.050    1.076
# .ATALCUR (.37.)    1.517    0.091   16.712    0.000    1.517    0.683
# .ATALCPR (.38.)    0.554    0.072    7.669    0.000    0.554    0.335
# .ATALCEX (.39.)    0.543    0.075    7.203    0.000    0.543    0.329
# .ATALCWO (.40.)    1.213    0.085   14.209    0.000    1.213    0.603
# .ATALCTR (.41.)    0.997    0.079   12.586    0.000    0.997    0.507
# .ATTOB3M (.42.)    2.993    0.136   22.077    0.000    2.993    1.086
# .ATTOBUR (.43.)    2.741    0.142   19.312    0.000    2.741    0.965
# .ATTOBPR (.44.)    1.028    0.083   12.339    0.000    1.028    0.452
# .ATTOBWO (.45.)    2.285    0.114   20.048    0.000    2.285    0.875
# .ATTOBTR (.46.)    2.332    0.112   20.901    0.000    2.332    0.939
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.679    0.220   12.173    0.000    2.679    0.738
# .ATALCURG          3.164    0.272   11.628    0.000    3.164    0.642
# .ATALCPRB          1.548    0.352    4.399    0.000    1.548    0.564
# .ATALCEXP          1.369    0.395    3.469    0.001    1.369    0.503
# .ATALCWOR          2.298    0.284    8.097    0.000    2.298    0.568
# .ATALCTRY          2.552    0.280    9.108    0.000    2.552    0.661
# .ATTOB3M           1.099    0.189    5.804    0.000    1.099    0.145
# .ATTOBURG          0.757    0.169    4.481    0.000    0.757    0.094
# .ATTOBPRB          3.831    0.439    8.726    0.000    3.831    0.741
# .ATTOBWOR          3.415    0.293   11.664    0.000    3.415    0.501
# .ATTOBTRY          2.782    0.278   10.016    0.000    2.782    0.451
# 
# 
# Group 2 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.883    0.062   14.159    0.000    0.883    0.473
# ATALCUR (.p2.)    1.327    0.072   18.437    0.000    1.327    0.626
# ATALCPR (.p3.)    1.093    0.089   12.234    0.000    1.093    0.734
# ATALCEX (.p4.)    1.163    0.105   11.064    0.000    1.163    0.728
# ATALCWO (.p5.)    1.095    0.081   13.448    0.000    1.095    0.610
# ATALCTR (.p6.)    0.966    0.081   11.978    0.000    0.966    0.589
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.564    0.000    2.549    0.919
# ATTOBUR (.p8.)    2.703    0.030   89.853    0.000    2.703    0.950
# ATTOBPR (.p9.)    1.158    0.069   16.673    0.000    1.158    0.513
# ATTOBWO (.10.)    1.353    0.088   15.462    0.000    1.353    0.539
# ATTOBTR (.11.)    1.372    0.085   16.233    0.000    1.372    0.575
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.457    0.066   -6.881    0.000   -0.457   -0.244
# ATALCWO (.13.)    0.695    0.081    8.624    0.000    0.695    0.387
# ATALCTR (.14.)    0.573    0.071    8.051    0.000    0.573    0.349
# ATTOBWO (.15.)    1.118    0.090   12.397    0.000    1.118    0.445
# ATTOBTR (.16.)    1.090    0.089   12.226    0.000    1.090    0.457
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.866    0.174    4.987    0.000    0.866    0.442
# .ATALC3M ~~                                                            
#   .ATALCURG          1.296    0.162    7.993    0.000    1.296    0.502
# ETOH ~~                                                               
#   TOB               0.260    0.040    6.489    0.000    0.260    0.260
# ETOH_TOB         -0.077    0.082   -0.938    0.348   -0.077   -0.077
# TOB ~~                                                                
#   ETOH_TOB          0.065    0.070    0.936    0.349    0.065    0.065
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.050    0.077   26.577    0.000    2.050    1.097
# .ATALCUR (.37.)    1.517    0.091   16.712    0.000    1.517    0.716
# .ATALCPR (.38.)    0.554    0.072    7.669    0.000    0.554    0.372
# .ATALCEX (.39.)    0.543    0.075    7.203    0.000    0.543    0.340
# .ATALCWO (.40.)    1.213    0.085   14.209    0.000    1.213    0.675
# .ATALCTR (.41.)    0.997    0.079   12.586    0.000    0.997    0.607
# .ATTOB3M (.42.)    2.993    0.136   22.077    0.000    2.993    1.079
# .ATTOBUR (.43.)    2.741    0.142   19.312    0.000    2.741    0.964
# .ATTOBPR (.44.)    1.028    0.083   12.339    0.000    1.028    0.455
# .ATTOBWO (.45.)    2.285    0.114   20.048    0.000    2.285    0.910
# .ATTOBTR (.46.)    2.332    0.112   20.901    0.000    2.332    0.977
# ETOH             -0.125    0.073   -1.715    0.086   -0.125   -0.125
# TOB              -0.016    0.065   -0.253    0.800   -0.016   -0.016
# ETOH_TO          -0.198    0.085   -2.328    0.020   -0.198   -0.198
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.442    0.148   16.495    0.000    2.442    0.699
# .ATALCURG          2.727    0.231   11.794    0.000    2.727    0.608
# .ATALCPRB          1.022    0.201    5.081    0.000    1.022    0.461
# .ATALCEXP          1.197    0.249    4.800    0.000    1.197    0.470
# .ATALCWOR          1.660    0.189    8.793    0.000    1.660    0.515
# .ATALCTRY          1.519    0.180    8.438    0.000    1.519    0.564
# .ATTOB3M           1.198    0.156    7.675    0.000    1.198    0.156
# .ATTOBURG          0.785    0.152    5.165    0.000    0.785    0.097
# .ATTOBPRB          3.757    0.281   13.385    0.000    3.757    0.737
# .ATTOBWOR          3.024    0.243   12.456    0.000    3.024    0.480
# .ATTOBTRY          2.431    0.231   10.519    0.000    2.431    0.427
# 
# >


############################ Test difference

anova(AST.Etoh_Tob.model.RACE.METRIC.fit, AST.Etoh_Tob.model.RACE.SCALAR.fit)
# > anova(AST.Etoh_Tob.model.RACE.METRIC.fit, AST.Etoh_Tob.model.RACE.SCALAR.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.RACE.METRIC.fit 84         108.69                              
# AST.Etoh_Tob.model.RACE.SCALAR.fit 92         113.45     9.3724       8     0.3119
# >

#equal factor means specification

AST.Etoh_Tob.model.RACE.MEANS.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                   data.etoh_tob.dems.RACE.05,
                                                   std.lv=TRUE,
                                                   estimator = "WLSMV",
                                                   group = "RACE",
                                                   group.equal=c("loadings", "intercepts", "means"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.MEANS.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.Etoh_Tob.model.RACE.MEANS.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 123 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# Number of equality constraints                    27
# 
# Number of observations per group:                   
#   2                                              403
# 3                                              771
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               133.426     159.184
# Degrees of freedom                                95          95
# P-value (Chi-square)                           0.006       0.000
# Scaling correction factor                                  1.140
# Shift parameter for each group:                                 
#   2                                                     14.466
# 3                                                     27.677
# simple second-order correction                             
# Test statistic for each group:
#   2                                           75.003      80.259
# 3                                           58.423      78.925
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8535.069    3450.925
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.522
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.995       0.981
# Tucker-Lewis Index (TLI)                       0.995       0.978
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.026       0.034
# 90 Percent confidence interval - lower         0.015       0.024
# 90 Percent confidence interval - upper         0.036       0.043
# P-value RMSEA <= 0.05                          1.000       0.999
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.047       0.047
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.883    0.063   14.107    0.000    0.883    0.463
# ATALCUR (.p2.)    1.322    0.072   18.402    0.000    1.322    0.596
# ATALCPR (.p3.)    1.094    0.089   12.249    0.000    1.094    0.661
# ATALCEX (.p4.)    1.169    0.105   11.131    0.000    1.169    0.708
# ATALCWO (.p5.)    1.095    0.081   13.517    0.000    1.095    0.544
# ATALCTR (.p6.)    0.964    0.080   11.991    0.000    0.964    0.490
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.567    0.000    2.549    0.925
# ATTOBUR (.p8.)    2.703    0.030   89.874    0.000    2.703    0.952
# ATTOBPR (.p9.)    1.158    0.069   16.676    0.000    1.158    0.509
# ATTOBWO (.10.)    1.351    0.087   15.544    0.000    1.351    0.518
# ATTOBTR (.11.)    1.369    0.085   16.196    0.000    1.369    0.551
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.470    0.067   -6.999    0.000   -0.470   -0.247
# ATALCWO (.13.)    0.686    0.081    8.507    0.000    0.686    0.341
# ATALCTR (.14.)    0.558    0.071    7.865    0.000    0.558    0.284
# ATTOBWO (.15.)    1.113    0.091   12.303    0.000    1.113    0.426
# ATTOBTR (.16.)    1.097    0.090   12.196    0.000    1.097    0.442
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.991    0.280    3.533    0.000    0.991    0.407
# .ATALC3M ~~                                                            
#   .ATALCURG          1.775    0.223    7.955    0.000    1.775    0.609
# ETOH ~~                                                               
#   TOB               0.340    0.060    5.654    0.000    0.340    0.340
# ETOH_TOB          0.050    0.091    0.545    0.586    0.050    0.050
# TOB ~~                                                                
#   ETOH_TOB          0.108    0.088    1.226    0.220    0.108    0.108
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.037    0.055   37.102    0.000    2.037    1.069
# .ATALCUR (.37.)    1.404    0.063   22.366    0.000    1.404    0.633
# .ATALCPR (.38.)    0.459    0.045   10.201    0.000    0.459    0.277
# .ATALCEX (.39.)    0.445    0.047    9.454    0.000    0.445    0.270
# .ATALCWO (.40.)    1.019    0.054   18.753    0.000    1.019    0.507
# .ATALCTR (.41.)    0.825    0.051   16.305    0.000    0.825    0.420
# .ATTOB3M (.42.)    2.966    0.081   36.716    0.000    2.966    1.076
# .ATTOBUR (.43.)    2.712    0.083   32.691    0.000    2.712    0.955
# .ATTOBPR (.44.)    1.015    0.066   15.371    0.000    1.015    0.446
# .ATTOBWO (.45.)    2.121    0.074   28.568    0.000    2.121    0.812
# .ATTOBTR (.46.)    2.171    0.071   30.754    0.000    2.171    0.874
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.672    0.220   12.138    0.000    2.672    0.736
# .ATALCURG          3.178    0.275   11.540    0.000    3.178    0.645
# .ATALCPRB          1.546    0.355    4.352    0.000    1.546    0.564
# .ATALCEXP          1.356    0.398    3.404    0.001    1.356    0.498
# .ATALCWOR          2.305    0.293    7.880    0.000    2.305    0.569
# .ATALCTRY          2.569    0.288    8.922    0.000    2.569    0.665
# .ATTOB3M           1.098    0.189    5.798    0.000    1.098    0.145
# .ATTOBURG          0.758    0.169    4.485    0.000    0.758    0.094
# .ATTOBPRB          3.831    0.439    8.719    0.000    3.831    0.741
# .ATTOBWOR          3.426    0.299   11.462    0.000    3.426    0.503
# .ATTOBTRY          2.766    0.287    9.651    0.000    2.766    0.448
# 
# 
# Group 2 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.883    0.063   14.107    0.000    0.883    0.472
# ATALCUR (.p2.)    1.322    0.072   18.402    0.000    1.322    0.624
# ATALCPR (.p3.)    1.094    0.089   12.249    0.000    1.094    0.735
# ATALCEX (.p4.)    1.169    0.105   11.131    0.000    1.169    0.732
# ATALCWO (.p5.)    1.095    0.081   13.517    0.000    1.095    0.610
# ATALCTR (.p6.)    0.964    0.080   11.991    0.000    0.964    0.587
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.567    0.000    2.549    0.919
# ATTOBUR (.p8.)    2.703    0.030   89.874    0.000    2.703    0.950
# ATTOBPR (.p9.)    1.158    0.069   16.676    0.000    1.158    0.513
# ATTOBWO (.10.)    1.351    0.087   15.544    0.000    1.351    0.538
# ATTOBTR (.11.)    1.369    0.085   16.196    0.000    1.369    0.573
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.470    0.067   -6.999    0.000   -0.470   -0.251
# ATALCWO (.13.)    0.686    0.081    8.507    0.000    0.686    0.382
# ATALCTR (.14.)    0.558    0.071    7.865    0.000    0.558    0.340
# ATTOBWO (.15.)    1.113    0.091   12.303    0.000    1.113    0.443
# ATTOBTR (.16.)    1.097    0.090   12.196    0.000    1.097    0.460
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.866    0.173    4.993    0.000    0.866    0.442
# .ATALC3M ~~                                                            
#   .ATALCURG          1.304    0.161    8.098    0.000    1.304    0.505
# ETOH ~~                                                               
#   TOB               0.260    0.040    6.494    0.000    0.260    0.260
# ETOH_TOB         -0.070    0.081   -0.866    0.387   -0.070   -0.070
# TOB ~~                                                                
#   ETOH_TOB          0.067    0.070    0.967    0.334    0.067    0.067
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.037    0.055   37.102    0.000    2.037    1.090
# .ATALCUR (.37.)    1.404    0.063   22.366    0.000    1.404    0.663
# .ATALCPR (.38.)    0.459    0.045   10.201    0.000    0.459    0.308
# .ATALCEX (.39.)    0.445    0.047    9.454    0.000    0.445    0.279
# .ATALCWO (.40.)    1.019    0.054   18.753    0.000    1.019    0.567
# .ATALCTR (.41.)    0.825    0.051   16.305    0.000    0.825    0.503
# .ATTOB3M (.42.)    2.966    0.081   36.716    0.000    2.966    1.069
# .ATTOBUR (.43.)    2.712    0.083   32.691    0.000    2.712    0.954
# .ATTOBPR (.44.)    1.015    0.066   15.371    0.000    1.015    0.450
# .ATTOBWO (.45.)    2.121    0.074   28.568    0.000    2.121    0.845
# .ATTOBTR (.46.)    2.171    0.071   30.754    0.000    2.171    0.910
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.433    0.150   16.241    0.000    2.433    0.697
# .ATALCURG          2.742    0.228   12.050    0.000    2.742    0.611
# .ATALCPRB          1.020    0.198    5.139    0.000    1.020    0.460
# .ATALCEXP          1.183    0.246    4.801    0.000    1.183    0.464
# .ATALCWOR          1.662    0.184    9.050    0.000    1.662    0.515
# .ATALCTRY          1.531    0.175    8.750    0.000    1.531    0.568
# .ATTOB3M           1.198    0.156    7.669    0.000    1.198    0.156
# .ATTOBURG          0.786    0.152    5.168    0.000    0.786    0.097
# .ATTOBPRB          3.757    0.280   13.397    0.000    3.757    0.737
# .ATTOBWOR          3.036    0.237   12.810    0.000    3.036    0.482
# .ATTOBTRY          2.417    0.228   10.586    0.000    2.417    0.424
# 
# >



anova(AST.Etoh_Tob.model.RACE.SCALAR.fit, AST.Etoh_Tob.model.RACE.MEANS.fit)
# > anova(AST.Etoh_Tob.model.RACE.SCALAR.fit, AST.Etoh_Tob.model.RACE.MEANS.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
# AST.Etoh_Tob.model.RACE.SCALAR.fit 92         113.45                                
# AST.Etoh_Tob.model.RACE.MEANS.fit  95         133.43     6.9867       3    0.07232 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# >


# Note: Figure out what this means considering these models significantly differ.


################### strict invariance

AST.Etoh_Tob.model.RACE.STRICT.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                  data.etoh_tob.dems.RACE.05,
                                                  std.lv=TRUE,
                                                  estimator = "WLSMV",
                                                  group = "RACE",
                                                  group.equal=c("loadings", "intercepts", "residuals"))

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.STRICT.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.Etoh_Tob.model.RACE.STRICT.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 92 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         89
# Number of equality constraints                    38
# 
# Number of observations per group:                   
#   2                                              403
# 3                                              771
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               128.312     161.675
# Degrees of freedom                               103         103
# P-value (Chi-square)                           0.046       0.000
# Scaling correction factor                                  1.114
# Shift parameter for each group:                                 
#   2                                                     15.968
# 3                                                     30.549
# simple second-order correction                             
# Test statistic for each group:
#   2                                           70.581      79.313
# 3                                           57.731      82.362
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8535.069    3450.925
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.522
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.982
# Tucker-Lewis Index (TLI)                       0.997       0.981
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.020       0.031
# 90 Percent confidence interval - lower         0.003       0.022
# 90 Percent confidence interval - upper         0.031       0.040
# P-value RMSEA <= 0.05                          1.000       1.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.050       0.050
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.872    0.063   13.869    0.000    0.872    0.469
# ATALCUR (.p2.)    1.330    0.072   18.498    0.000    1.330    0.617
# ATALCPR (.p3.)    1.095    0.090   12.225    0.000    1.095    0.710
# ATALCEX (.p4.)    1.166    0.105   11.071    0.000    1.166    0.723
# ATALCWO (.p5.)    1.099    0.082   13.426    0.000    1.099    0.573
# ATALCTR (.p6.)    0.971    0.081   12.018    0.000    0.971    0.543
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.697    0.000    2.549    0.921
# ATTOBUR (.p8.)    2.702    0.030   89.864    0.000    2.702    0.951
# ATTOBPR (.p9.)    1.158    0.069   16.675    0.000    1.158    0.512
# ATTOBWO (.10.)    1.362    0.087   15.647    0.000    1.362    0.528
# ATTOBTR (.11.)    1.378    0.085   16.187    0.000    1.378    0.562
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.429    0.070   -6.112    0.000   -0.429   -0.231
# ATALCWO (.13.)    0.711    0.084    8.500    0.000    0.711    0.371
# ATALCTR (.14.)    0.599    0.074    8.053    0.000    0.599    0.335
# ATTOBWO (.15.)    1.102    0.094   11.707    0.000    1.102    0.427
# ATTOBTR (.16.)    1.096    0.093   11.841    0.000    1.096    0.447
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          1.002    0.281    3.566    0.000    1.002    0.474
# .ATALC3M ~~                                                            
#   .ATALCURG          1.789    0.225    7.963    0.000    1.789    0.659
# ETOH ~~                                                               
#   TOB               0.330    0.060    5.543    0.000    0.330    0.330
# ETOH_TOB          0.066    0.090    0.736    0.462    0.066    0.066
# TOB ~~                                                                
#   ETOH_TOB          0.123    0.085    1.442    0.149    0.123    0.123
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.051    0.077   26.703    0.000    2.051    1.104
# .ATALCUR (.37.)    1.515    0.090   16.751    0.000    1.515    0.703
# .ATALCPR (.38.)    0.553    0.072    7.675    0.000    0.553    0.358
# .ATALCEX (.39.)    0.541    0.075    7.203    0.000    0.541    0.335
# .ATALCWO (.40.)    1.215    0.086   14.113    0.000    1.215    0.634
# .ATALCTR (.41.)    1.000    0.080   12.481    0.000    1.000    0.560
# .ATTOB3M (.42.)    2.993    0.136   22.076    0.000    2.993    1.081
# .ATTOBUR (.43.)    2.741    0.142   19.313    0.000    2.741    0.964
# .ATTOBPR (.44.)    1.028    0.083   12.339    0.000    1.028    0.454
# .ATTOBWO (.45.)    2.284    0.114   20.123    0.000    2.284    0.886
# .ATTOBTR (.46.)    2.334    0.112   20.900    0.000    2.334    0.952
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.558    0.130   19.707    0.000    2.558    0.741
# .ATALCUR (.23.)    2.879    0.190   15.152    0.000    2.879    0.619
# .ATALCPR (.24.)    1.180    0.124    9.511    0.000    1.180    0.496
# .ATALCEX (.25.)    1.243    0.138    9.028    0.000    1.243    0.477
# .ATALCWO (.26.)    1.858    0.157   11.853    0.000    1.858    0.506
# .ATALCTR (.27.)    1.815    0.138   13.169    0.000    1.815    0.568
# .ATTOB3M (.28.)    1.166    0.141    8.243    0.000    1.166    0.152
# .ATTOBUR (.29.)    0.779    0.138    5.648    0.000    0.779    0.096
# .ATTOBPR (.30.)    3.782    0.177   21.345    0.000    3.782    0.738
# .ATTOBWO (.31.)    3.210    0.225   14.281    0.000    3.210    0.483
# .ATTOBTR (.32.)    2.543    0.213   11.921    0.000    2.543    0.423
# 
# 
# Group 2 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.872    0.063   13.869    0.000    0.872    0.461
# ATALCUR (.p2.)    1.330    0.072   18.498    0.000    1.330    0.617
# ATALCPR (.p3.)    1.095    0.090   12.225    0.000    1.095    0.710
# ATALCEX (.p4.)    1.166    0.105   11.071    0.000    1.166    0.723
# ATALCWO (.p5.)    1.099    0.082   13.426    0.000    1.099    0.595
# ATALCTR (.p6.)    0.971    0.081   12.018    0.000    0.971    0.561
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.549    0.035   73.697    0.000    2.549    0.921
# ATTOBUR (.p8.)    2.702    0.030   89.864    0.000    2.702    0.951
# ATTOBPR (.p9.)    1.158    0.069   16.675    0.000    1.158    0.512
# ATTOBWO (.10.)    1.362    0.087   15.647    0.000    1.362    0.537
# ATTOBTR (.11.)    1.378    0.085   16.187    0.000    1.378    0.573
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.429    0.070   -6.112    0.000   -0.429   -0.227
# ATALCWO (.13.)    0.711    0.084    8.500    0.000    0.711    0.385
# ATALCTR (.14.)    0.599    0.074    8.053    0.000    0.599    0.346
# ATTOBWO (.15.)    1.102    0.094   11.707    0.000    1.102    0.435
# ATTOBTR (.16.)    1.096    0.093   11.841    0.000    1.096    0.455
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.860    0.174    4.951    0.000    0.860    0.407
# .ATALC3M ~~                                                            
#   .ATALCURG          1.297    0.158    8.218    0.000    1.297    0.478
# ETOH ~~                                                               
#   TOB               0.264    0.040    6.589    0.000    0.264    0.264
# ETOH_TOB         -0.102    0.083   -1.228    0.220   -0.102   -0.102
# TOB ~~                                                                
#   ETOH_TOB          0.048    0.068    0.716    0.474    0.048    0.048
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.051    0.077   26.703    0.000    2.051    1.084
# .ATALCUR (.37.)    1.515    0.090   16.751    0.000    1.515    0.703
# .ATALCPR (.38.)    0.553    0.072    7.675    0.000    0.553    0.358
# .ATALCEX (.39.)    0.541    0.075    7.203    0.000    0.541    0.335
# .ATALCWO (.40.)    1.215    0.086   14.113    0.000    1.215    0.658
# .ATALCTR (.41.)    1.000    0.080   12.481    0.000    1.000    0.578
# .ATTOB3M (.42.)    2.993    0.136   22.076    0.000    2.993    1.081
# .ATTOBUR (.43.)    2.741    0.142   19.313    0.000    2.741    0.964
# .ATTOBPR (.44.)    1.028    0.083   12.339    0.000    1.028    0.454
# .ATTOBWO (.45.)    2.284    0.114   20.123    0.000    2.284    0.901
# .ATTOBTR (.46.)    2.334    0.112   20.900    0.000    2.334    0.970
# ETOH             -0.122    0.072   -1.698    0.089   -0.122   -0.122
# TOB              -0.016    0.065   -0.250    0.803   -0.016   -0.016
# ETOH_TO          -0.200    0.085   -2.358    0.018   -0.200   -0.200
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.558    0.130   19.707    0.000    2.558    0.715
# .ATALCUR (.23.)    2.879    0.190   15.152    0.000    2.879    0.619
# .ATALCPR (.24.)    1.180    0.124    9.511    0.000    1.180    0.496
# .ATALCEX (.25.)    1.243    0.138    9.028    0.000    1.243    0.477
# .ATALCWO (.26.)    1.858    0.157   11.853    0.000    1.858    0.545
# .ATALCTR (.27.)    1.815    0.138   13.169    0.000    1.815    0.606
# .ATTOB3M (.28.)    1.166    0.141    8.243    0.000    1.166    0.152
# .ATTOBUR (.29.)    0.779    0.138    5.648    0.000    0.779    0.096
# .ATTOBPR (.30.)    3.782    0.177   21.345    0.000    3.782    0.738
# .ATTOBWO (.31.)    3.210    0.225   14.281    0.000    3.210    0.500
# .ATTOBTR (.32.)    2.543    0.213   11.921    0.000    2.543    0.439
# >

anova(AST.Etoh_Tob.model.RACE.SCALAR.fit, AST.Etoh_Tob.model.RACE.STRICT.fit)
# > anova(AST.Etoh_Tob.model.RACE.SCALAR.fit, AST.Etoh_Tob.model.RACE.STRICT.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.RACE.SCALAR.fit  92         113.45                              
# AST.Etoh_Tob.model.RACE.STRICT.fit 103         128.31     14.313      11     0.2162
# >

################### factor variances and covariances
AST.Etoh_Tob.model.RACE.FVCV.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                   data.etoh_tob.dems.RACE.05,
                                                   std.lv=TRUE,
                                                   estimator = "WLSMV",
                                                   group = "RACE",
                                                   group.equal=c("loadings", "intercepts",
                                                                 "residuals", "lv.variances", "lv.covariances"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.FVCV.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.Etoh_Tob.model.RACE.FVCV.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 107 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         89
# Number of equality constraints                    41
# 
# Number of observations per group:                   
#   2                                              403
# 3                                              771
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               153.387     150.258
# Degrees of freedom                               106         106
# P-value (Chi-square)                           0.002       0.003
# Scaling correction factor                                  1.645
# Shift parameter for each group:                                 
#   2                                                     19.565
# 3                                                     37.432
# simple second-order correction                             
# Test statistic for each group:
#   2                                           88.003      73.073
# 3                                           65.383      77.185
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8535.069    3450.925
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.522
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.994       0.987
# Tucker-Lewis Index (TLI)                       0.994       0.986
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.028       0.027
# 90 Percent confidence interval - lower         0.017       0.016
# 90 Percent confidence interval - upper         0.037       0.036
# P-value RMSEA <= 0.05                          1.000       1.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.054       0.054
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.874    0.063   13.927    0.000    0.874    0.465
# ATALCUR (.p2.)    1.312    0.073   18.069    0.000    1.312    0.608
# ATALCPR (.p3.)    1.092    0.089   12.277    0.000    1.092    0.708
# ATALCEX (.p4.)    1.161    0.105   11.088    0.000    1.161    0.720
# ATALCWO (.p5.)    1.120    0.080   13.962    0.000    1.120    0.599
# ATALCTR (.p6.)    0.978    0.080   12.249    0.000    0.978    0.559
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.548    0.035   73.471    0.000    2.548    0.920
# ATTOBUR (.p8.)    2.703    0.030   89.729    0.000    2.703    0.951
# ATTOBPR (.p9.)    1.157    0.069   16.697    0.000    1.157    0.511
# ATTOBWO (.10.)    1.371    0.087   15.706    0.000    1.371    0.537
# ATTOBTR (.11.)    1.390    0.082   16.953    0.000    1.390    0.574
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.475    0.066   -7.176    0.000   -0.475   -0.252
# ATALCWO (.13.)    0.699    0.081    8.660    0.000    0.699    0.374
# ATALCTR (.14.)    0.552    0.070    7.864    0.000    0.552    0.316
# ATTOBWO (.15.)    1.146    0.088   13.026    0.000    1.146    0.449
# ATTOBTR (.16.)    1.059    0.087   12.175    0.000    1.059    0.437
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPR           1.059    0.315    3.368    0.001    1.059    0.500
# .ATALC3M ~~                                                            
#   .ATALCUR           1.736    0.214    8.097    0.000    1.736    0.640
# ETOH ~~                                                               
#   TOB     (.33.)    0.286    0.031    9.146    0.000    0.286    0.286
# ETOH_TO (.34.)   -0.046    0.066   -0.696    0.486   -0.046   -0.046
# TOB ~~                                                                
#   ETOH_TO (.35.)    0.064    0.063    1.014    0.311    0.064    0.064
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.049    0.077   26.599    0.000    2.049    1.089
# .ATALCUR (.37.)    1.516    0.091   16.720    0.000    1.516    0.703
# .ATALCPR (.38.)    0.555    0.073    7.653    0.000    0.555    0.360
# .ATALCEX (.39.)    0.543    0.076    7.189    0.000    0.543    0.337
# .ATALCWO (.40.)    1.215    0.086   14.184    0.000    1.215    0.649
# .ATALCTR (.41.)    0.994    0.078   12.699    0.000    0.994    0.569
# .ATTOB3M (.42.)    2.994    0.136   22.084    0.000    2.994    1.082
# .ATTOBUR (.43.)    2.743    0.142   19.312    0.000    2.743    0.965
# .ATTOBPR (.44.)    1.028    0.083   12.342    0.000    1.028    0.454
# .ATTOBWO (.45.)    2.287    0.115   19.887    0.000    2.287    0.896
# .ATTOBTR (.46.)    2.326    0.111   21.047    0.000    2.326    0.960
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.511    0.126   19.908    0.000    2.511    0.710
# .ATALCUR (.23.)    2.927    0.190   15.422    0.000    2.927    0.630
# .ATALCPR (.24.)    1.187    0.121    9.809    0.000    1.187    0.499
# .ATALCEX (.25.)    1.255    0.135    9.310    0.000    1.255    0.482
# .ATALCWO (.26.)    1.828    0.151   12.082    0.000    1.828    0.522
# .ATALCTR (.27.)    1.845    0.133   13.924    0.000    1.845    0.603
# .ATTOB3M (.28.)    1.172    0.142    8.260    0.000    1.172    0.153
# .ATTOBUR (.29.)    0.775    0.138    5.592    0.000    0.775    0.096
# .ATTOBPR (.30.)    3.784    0.177   21.317    0.000    3.784    0.739
# .ATTOBWO (.31.)    3.121    0.216   14.451    0.000    3.121    0.479
# .ATTOBTR (.32.)    2.631    0.200   13.182    0.000    2.631    0.448
# 
# 
# Group 2 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.874    0.063   13.927    0.000    0.874    0.465
# ATALCUR (.p2.)    1.312    0.073   18.069    0.000    1.312    0.608
# ATALCPR (.p3.)    1.092    0.089   12.277    0.000    1.092    0.708
# ATALCEX (.p4.)    1.161    0.105   11.088    0.000    1.161    0.720
# ATALCWO (.p5.)    1.120    0.080   13.962    0.000    1.120    0.599
# ATALCTR (.p6.)    0.978    0.080   12.249    0.000    0.978    0.559
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.548    0.035   73.471    0.000    2.548    0.920
# ATTOBUR (.p8.)    2.703    0.030   89.729    0.000    2.703    0.951
# ATTOBPR (.p9.)    1.157    0.069   16.697    0.000    1.157    0.511
# ATTOBWO (.10.)    1.371    0.087   15.706    0.000    1.371    0.537
# ATTOBTR (.11.)    1.390    0.082   16.953    0.000    1.390    0.574
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.475    0.066   -7.176    0.000   -0.475   -0.252
# ATALCWO (.13.)    0.699    0.081    8.660    0.000    0.699    0.374
# ATALCTR (.14.)    0.552    0.070    7.864    0.000    0.552    0.316
# ATTOBWO (.15.)    1.146    0.088   13.026    0.000    1.146    0.449
# ATTOBTR (.16.)    1.059    0.087   12.175    0.000    1.059    0.437
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPR           0.833    0.181    4.594    0.000    0.833    0.393
# .ATALC3M ~~                                                            
#   .ATALCUR           1.340    0.157    8.539    0.000    1.340    0.494
# ETOH ~~                                                               
#   TOB     (.33.)    0.286    0.031    9.146    0.000    0.286    0.286
# ETOH_TO (.34.)   -0.046    0.066   -0.696    0.486   -0.046   -0.046
# TOB ~~                                                                
#   ETOH_TO (.35.)    0.064    0.063    1.014    0.311    0.064    0.064
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.049    0.077   26.599    0.000    2.049    1.089
# .ATALCUR (.37.)    1.516    0.091   16.720    0.000    1.516    0.703
# .ATALCPR (.38.)    0.555    0.073    7.653    0.000    0.555    0.360
# .ATALCEX (.39.)    0.543    0.076    7.189    0.000    0.543    0.337
# .ATALCWO (.40.)    1.215    0.086   14.184    0.000    1.215    0.649
# .ATALCTR (.41.)    0.994    0.078   12.699    0.000    0.994    0.569
# .ATTOB3M (.42.)    2.994    0.136   22.084    0.000    2.994    1.082
# .ATTOBUR (.43.)    2.743    0.142   19.312    0.000    2.743    0.965
# .ATTOBPR (.44.)    1.028    0.083   12.342    0.000    1.028    0.454
# .ATTOBWO (.45.)    2.287    0.115   19.887    0.000    2.287    0.896
# .ATTOBTR (.46.)    2.326    0.111   21.047    0.000    2.326    0.960
# ETOH             -0.126    0.074   -1.709    0.087   -0.126   -0.126
# TOB              -0.017    0.065   -0.265    0.791   -0.017   -0.017
# ETOH_TO          -0.194    0.085   -2.281    0.023   -0.194   -0.194
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.511    0.126   19.908    0.000    2.511    0.710
# .ATALCUR (.23.)    2.927    0.190   15.422    0.000    2.927    0.630
# .ATALCPR (.24.)    1.187    0.121    9.809    0.000    1.187    0.499
# .ATALCEX (.25.)    1.255    0.135    9.310    0.000    1.255    0.482
# .ATALCWO (.26.)    1.828    0.151   12.082    0.000    1.828    0.522
# .ATALCTR (.27.)    1.845    0.133   13.924    0.000    1.845    0.603
# .ATTOB3M (.28.)    1.172    0.142    8.260    0.000    1.172    0.153
# .ATTOBUR (.29.)    0.775    0.138    5.592    0.000    0.775    0.096
# .ATTOBPR (.30.)    3.784    0.177   21.317    0.000    3.784    0.739
# .ATTOBWO (.31.)    3.121    0.216   14.451    0.000    3.121    0.479
# .ATTOBTR (.32.)    2.631    0.200   13.182    0.000    2.631    0.448
# 
# >

####################################################################################
####################################################################################
####################################################################################

##################### INVARIANCE TESTS_GENDER ######################################

table(data.etoh_tob.dems$GENDER)
# > table(data.etoh_tob.dems$GENDER)
# 
# 1   2  97  98 
# 619 812   1   1 
# >

### Filter out 98 for GENDER
data.etoh_tob.dems.GENDER <- dplyr::filter(data.etoh_tob.dems, GENDER != 98) #Extract rows that meet logical criteria.

### Filter out 97 for GENDER

data.etoh_tob.dems.GENDER.02 <- dplyr::filter(data.etoh_tob.dems.GENDER, GENDER != 97) #Extract rows that meet logical criteria.

table(data.etoh_tob.dems.GENDER.02$GENDER)
# > table(data.etoh_tob.dems.GENDER.02$GENDER)
# 
# 1   2 
# 619 812 
# >

##### Descriptive Stats
psych::describe(data.etoh_tob.dems.GENDER.02)
# > psych::describe(data.etoh_tob.dems.GENDER.02)
# vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1431 4864324.87 2882615.05 4733263 4838233.49 3741683.58 10594 9994871 9984277  0.08
# ALC_TOT     2 1431       6.23       7.77       3       4.60       4.45     0      39      39  2.01
# ATALC3M     3 1431       2.01       1.89       2       1.83       2.97     0       6       6  0.42
# ATALCURG    4 1431       1.36       2.15       0       0.99       0.00     0       6       6  1.10
# ATALCPRB    5 1431       0.48       1.56       0       0.00       0.00     0       7       7  3.10
# ATALCEXP    6 1431       0.46       1.64       0       0.00       0.00     0       8       8  3.41
# ATALCWOR    7 1431       1.05       1.90       0       0.62       0.00     0       6       6  1.59
# ATALCTRY    8 1431       0.86       1.79       0       0.42       0.00     0       6       6  1.92
# TOB_TOT     9 1431      10.91      10.11       8      10.06      11.86     0      31      31  0.43
# ATTOB3M    10 1431       2.92       2.78       3       2.90       4.45     0       6       6  0.06
# ATTOBURG   11 1431       2.68       2.84       0       2.59       0.00     0       6       6  0.18
# ATTOBPRB   12 1431       1.01       2.25       0       0.41       0.00     0       7       7  1.91
# ATTOBWOR   13 1431       2.12       2.53       0       1.90       0.00     0       6       6  0.59
# ATTOBTRY   14 1431       2.18       2.41       3       1.98       4.45     0       6       6  0.53
# GENDER     15 1431       1.57       0.50       2       1.58       0.00     1       2       1 -0.27
# AGE        16 1431      45.89      14.73      48      45.88      16.31    18      94      76 -0.02
# RACE       17 1431       2.85       1.18       3       2.68       0.00     1       7       6  1.31
# EDU        18 1431      14.29       2.96      15      14.44       2.97     0      21      21 -0.44
# JOB        19 1431       5.08      13.65       3       3.05       2.97     1      99      98  6.60
# MARTL      20 1431       3.61       1.68       5       3.71       1.48     1       6       5 -0.50
# kurtosis       se
# PROJID      -1.21 76202.08
# ALC_TOT      4.11     0.21
# ATALC3M     -0.84     0.05
# ATALCURG    -0.48     0.06
# ATALCPRB     8.08     0.04
# ATALCEXP    10.17     0.04
# ATALCWOR     1.23     0.05
# ATALCTRY     2.43     0.05
# TOB_TOT     -1.22     0.27
# ATTOB3M     -1.85     0.07
# ATTOBURG    -1.89     0.08
# ATTOBPRB     1.91     0.06
# ATTOBWOR    -1.33     0.07
# ATTOBTRY    -1.26     0.06
# GENDER      -1.93     0.01
# AGE         -0.65     0.39
# RACE         2.11     0.03
# EDU          0.41     0.08
# JOB         42.47     0.36
# MARTL       -1.30     0.04
# > 

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.Etoh_Tob.model.GENDER.fit <- lavaan::cfa(AST.Etoh_Tob.model,
                                             data.etoh_tob.dems.GENDER.02,
                                             std.lv=TRUE,
                                             estimator = "WLSMV",
                                             group = "GENDER")


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.GENDER.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 118 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# 
# Number of observations per group:                   
#   2                                              812
# 1                                              619
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               116.686     227.104
# Degrees of freedom                                68          68
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.577
# Shift parameter for each group:                                 
#   2                                                     14.134
# 1                                                     10.775
# simple second-order correction                             
# Test statistic for each group:
#   2                                           74.772     143.700
# 1                                           41.914      83.404
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10505.671    4234.116
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.521
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.995       0.961
# Tucker-Lewis Index (TLI)                       0.992       0.938
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.032       0.057
# 90 Percent confidence interval - lower         0.022       0.049
# 90 Percent confidence interval - upper         0.041       0.066
# P-value RMSEA <= 0.05                          0.999       0.071
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.042       0.042
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M           0.972    0.083   11.682    0.000    0.972    0.512
# ATALCURG          1.422    0.089   16.028    0.000    1.422    0.653
# ATALCPRB          1.078    0.110    9.778    0.000    1.078    0.686
# ATALCEXP          1.133    0.127    8.927    0.000    1.133    0.710
# ATALCWOR          1.087    0.099   10.943    0.000    1.087    0.571
# ATALCTRY          0.977    0.100    9.762    0.000    0.977    0.554
# TOB =~                                                                
#   ATTOB3M           2.570    0.039   66.616    0.000    2.570    0.928
# ATTOBURG          2.695    0.038   70.257    0.000    2.695    0.947
# ATTOBPRB          1.189    0.084   14.141    0.000    1.189    0.531
# ATTOBWOR          1.307    0.097   13.423    0.000    1.307    0.519
# ATTOBTRY          1.381    0.096   14.342    0.000    1.381    0.571
# ETOH_TOB =~                                                           
#   ATALC3M          -0.539    0.076   -7.142    0.000   -0.539   -0.284
# ATALCWOR          0.708    0.098    7.219    0.000    0.708    0.372
# ATALCTRY          0.591    0.087    6.810    0.000    0.591    0.335
# ATTOBWOR          1.075    0.099   10.809    0.000    1.075    0.427
# ATTOBTRY          1.089    0.100   10.865    0.000    1.089    0.450
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.842    0.165    5.110    0.000    0.842    0.388
# .ATALC3M ~~                                                            
#   .ATALCURG          1.366    0.160    8.516    0.000    1.366    0.539
# ETOH ~~                                                               
#   TOB               0.294    0.039    7.448    0.000    0.294    0.294
# ETOH_TOB         -0.004    0.076   -0.054    0.957   -0.004   -0.004
# TOB ~~                                                                
#   ETOH_TOB          0.060    0.071    0.844    0.399    0.060    0.060
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           2.022    0.067   30.357    0.000    2.022    1.065
# .ATALCURG          1.421    0.076   18.590    0.000    1.421    0.652
# .ATALCPRB          0.477    0.055    8.638    0.000    0.477    0.303
# .ATALCEXP          0.430    0.056    7.675    0.000    0.430    0.269
# .ATALCWOR          1.042    0.067   15.606    0.000    1.042    0.548
# .ATALCTRY          0.842    0.062   13.617    0.000    0.842    0.478
# .ATTOB3M           2.904    0.097   29.881    0.000    2.904    1.049
# .ATTOBURG          2.676    0.100   26.799    0.000    2.676    0.941
# .ATTOBPRB          0.996    0.079   12.683    0.000    0.996    0.445
# .ATTOBWOR          2.135    0.088   24.181    0.000    2.135    0.849
# .ATTOBTRY          2.195    0.085   25.845    0.000    2.195    0.907
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.362    0.163   14.475    0.000    2.362    0.656
# .ATALCURG          2.723    0.240   11.351    0.000    2.723    0.574
# .ATALCPRB          1.309    0.158    8.276    0.000    1.309    0.529
# .ATALCEXP          1.264    0.156    8.088    0.000    1.264    0.496
# .ATALCWOR          1.942    0.197    9.863    0.000    1.942    0.537
# .ATALCTRY          1.808    0.157   11.523    0.000    1.808    0.582
# .ATTOB3M           1.061    0.154    6.877    0.000    1.061    0.138
# .ATTOBURG          0.831    0.180    4.615    0.000    0.831    0.103
# .ATTOBPRB          3.596    0.206   17.456    0.000    3.596    0.718
# .ATTOBWOR          3.301    0.249   13.264    0.000    3.301    0.521
# .ATTOBTRY          2.582    0.236   10.921    0.000    2.582    0.441
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M           0.988    0.085   11.686    0.000    0.988    0.526
# ATALCURG          1.355    0.101   13.420    0.000    1.355    0.644
# ATALCPRB          1.076    0.122    8.847    0.000    1.076    0.695
# ATALCEXP          1.239    0.137    9.040    0.000    1.239    0.728
# ATALCWOR          1.200    0.114   10.565    0.000    1.200    0.631
# ATALCTRY          1.061    0.109    9.695    0.000    1.061    0.583
# TOB =~                                                                
#   ATTOB3M           2.608    0.053   49.633    0.000    2.608    0.935
# ATTOBURG          2.675    0.054   49.708    0.000    2.675    0.940
# ATTOBPRB          1.132    0.097   11.695    0.000    1.132    0.497
# ATTOBWOR          1.366    0.110   12.413    0.000    1.366    0.538
# ATTOBTRY          1.400    0.106   13.272    0.000    1.400    0.582
# ETOH_TOB =~                                                           
#   ATALC3M          -0.393    0.097   -4.049    0.000   -0.393   -0.209
# ATALCWOR          0.863    0.122    7.072    0.000    0.863    0.454
# ATALCTRY          0.642    0.107    6.022    0.000    0.642    0.353
# ATTOBWOR          1.026    0.127    8.095    0.000    1.026    0.404
# ATTOBTRY          1.048    0.123    8.513    0.000    1.048    0.435
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.944    0.200    4.717    0.000    0.944    0.430
# .ATALC3M ~~                                                            
#   .ATALCURG          1.162    0.176    6.589    0.000    1.162    0.473
# ETOH ~~                                                               
#   TOB               0.314    0.039    8.075    0.000    0.314    0.314
# ETOH_TOB         -0.088    0.098   -0.901    0.368   -0.088   -0.088
# TOB ~~                                                                
#   ETOH_TOB          0.064    0.080    0.799    0.425    0.064    0.064
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           1.998    0.075   26.493    0.000    1.998    1.065
# .ATALCURG          1.284    0.085   15.188    0.000    1.284    0.610
# .ATALCPRB          0.481    0.062    7.739    0.000    0.481    0.311
# .ATALCEXP          0.506    0.068    7.396    0.000    0.506    0.297
# .ATALCWOR          1.071    0.076   14.021    0.000    1.071    0.563
# .ATALCTRY          0.892    0.073   12.196    0.000    0.892    0.490
# .ATTOB3M           2.942    0.112   26.238    0.000    2.942    1.054
# .ATTOBURG          2.674    0.114   23.389    0.000    2.674    0.940
# .ATTOBPRB          1.019    0.091   11.151    0.000    1.019    0.448
# .ATTOBWOR          2.108    0.102   20.646    0.000    2.108    0.830
# .ATTOBTRY          2.171    0.097   22.447    0.000    2.171    0.902
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.324    0.169   13.768    0.000    2.324    0.660
# .ATALCURG          2.591    0.256   10.129    0.000    2.591    0.585
# .ATALCPRB          1.238    0.158    7.844    0.000    1.238    0.517
# .ATALCEXP          1.359    0.192    7.090    0.000    1.359    0.469
# .ATALCWOR          1.612    0.218    7.386    0.000    1.612    0.446
# .ATALCTRY          1.892    0.204    9.280    0.000    1.892    0.571
# .ATTOB3M           0.984    0.233    4.220    0.000    0.984    0.126
# .ATTOBURG          0.936    0.255    3.675    0.000    0.936    0.116
# .ATTOBPRB          3.894    0.247   15.733    0.000    3.894    0.753
# .ATTOBWOR          3.357    0.289   11.611    0.000    3.357    0.520
# .ATTOBTRY          2.546    0.283    8.985    0.000    2.546    0.439
# 
# >


############### Metric invariance

AST.Etoh_Tob.model.GENDER.METRIC.fit <- lavaan::cfa(AST.Etoh_Tob.model,
                                                    data.etoh_tob.dems.GENDER.02,
                                                    std.lv=TRUE,
                                                    estimator = "WLSMV",
                                                    group = "GENDER",
                                                    group.equal=c("loadings"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.GENDER.METRIC.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.GENDER.METRIC.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 104 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# Number of equality constraints                    16
# 
# Number of observations per group:                   
#   2                                              812
# 1                                              619
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               121.691     158.086
# Degrees of freedom                                84          84
# P-value (Chi-square)                           0.005       0.000
# Scaling correction factor                                  0.985
# Shift parameter for each group:                                 
#   2                                                     19.580
# 1                                                     14.926
# simple second-order correction                             
# Test statistic for each group:
#   2                                           76.980      97.755
# 1                                           44.712      60.332
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10505.671    4234.116
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.521
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.996       0.982
# Tucker-Lewis Index (TLI)                       0.995       0.976
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.025       0.035
# 90 Percent confidence interval - lower         0.014       0.027
# 90 Percent confidence interval - upper         0.034       0.043
# P-value RMSEA <= 0.05                          1.000       0.999
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.043       0.043
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.473    0.000    0.978    0.515
# ATALCUR (.p2.)    1.391    0.067   20.919    0.000    1.391    0.639
# ATALCPR (.p3.)    1.076    0.081   13.209    0.000    1.076    0.685
# ATALCEX (.p4.)    1.177    0.093   12.638    0.000    1.177    0.738
# ATALCWO (.p5.)    1.135    0.074   15.368    0.000    1.135    0.597
# ATALCTR (.p6.)    1.016    0.074   13.751    0.000    1.016    0.576
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.579    0.000    2.586    0.934
# ATTOBUR (.p8.)    2.687    0.032   83.802    0.000    2.687    0.944
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.520
# ATTOBWO (.10.)    1.333    0.074   18.086    0.000    1.333    0.530
# ATTOBTR (.11.)    1.392    0.071   19.566    0.000    1.392    0.575
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.474    0.060   -7.921    0.000   -0.474   -0.250
# ATALCWO (.13.)    0.780    0.076   10.318    0.000    0.780    0.410
# ATALCTR (.14.)    0.611    0.067    9.090    0.000    0.611    0.346
# ATTOBWO (.15.)    1.072    0.079   13.580    0.000    1.072    0.426
# ATTOBTR (.16.)    1.057    0.078   13.602    0.000    1.057    0.437
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.854    0.176    4.845    0.000    0.854    0.390
# .ATALC3M ~~                                                            
#   .ATALCURG          1.365    0.163    8.356    0.000    1.365    0.527
# ETOH ~~                                                               
#   TOB               0.291    0.041    7.133    0.000    0.291    0.291
# ETOH_TOB         -0.039    0.074   -0.534    0.593   -0.039   -0.039
# TOB ~~                                                                
#   ETOH_TOB          0.045    0.062    0.727    0.467    0.045    0.045
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           2.022    0.067   30.357    0.000    2.022    1.065
# .ATALCURG          1.421    0.076   18.590    0.000    1.421    0.652
# .ATALCPRB          0.477    0.055    8.638    0.000    0.477    0.303
# .ATALCEXP          0.430    0.056    7.675    0.000    0.430    0.269
# .ATALCWOR          1.042    0.067   15.606    0.000    1.042    0.548
# .ATALCTRY          0.842    0.062   13.617    0.000    0.842    0.478
# .ATTOB3M           2.904    0.097   29.881    0.000    2.904    1.049
# .ATTOBURG          2.676    0.100   26.799    0.000    2.676    0.941
# .ATTOBPRB          0.996    0.079   12.683    0.000    0.996    0.445
# .ATTOBWOR          2.135    0.088   24.181    0.000    2.135    0.849
# .ATTOBTRY          2.195    0.085   25.845    0.000    2.195    0.907
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.386    0.160   14.910    0.000    2.386    0.662
# .ATALCURG          2.809    0.217   12.924    0.000    2.809    0.592
# .ATALCPRB          1.313    0.217    6.042    0.000    1.313    0.531
# .ATALCEXP          1.161    0.244    4.761    0.000    1.161    0.456
# .ATALCWOR          1.791    0.201    8.893    0.000    1.791    0.495
# .ATALCTRY          1.751    0.178    9.862    0.000    1.751    0.564
# .ATTOB3M           0.978    0.152    6.441    0.000    0.978    0.128
# .ATTOBURG          0.878    0.166    5.281    0.000    0.878    0.108
# .ATTOBPRB          3.654    0.279   13.114    0.000    3.654    0.729
# .ATTOBWOR          3.276    0.222   14.751    0.000    3.276    0.517
# .ATTOBTRY          2.664    0.212   12.556    0.000    2.664    0.455
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.473    0.000    0.978    0.521
# ATALCUR (.p2.)    1.391    0.067   20.919    0.000    1.391    0.661
# ATALCPR (.p3.)    1.076    0.081   13.209    0.000    1.076    0.695
# ATALCEX (.p4.)    1.177    0.093   12.638    0.000    1.177    0.692
# ATALCWO (.p5.)    1.135    0.074   15.368    0.000    1.135    0.597
# ATALCTR (.p6.)    1.016    0.074   13.751    0.000    1.016    0.558
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.579    0.000    2.586    0.927
# ATTOBUR (.p8.)    2.687    0.032   83.802    0.000    2.687    0.945
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.512
# ATTOBWO (.10.)    1.333    0.074   18.086    0.000    1.333    0.525
# ATTOBTR (.11.)    1.392    0.071   19.566    0.000    1.392    0.578
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.474    0.060   -7.921    0.000   -0.474   -0.252
# ATALCWO (.13.)    0.780    0.076   10.318    0.000    0.780    0.410
# ATALCTR (.14.)    0.611    0.067    9.090    0.000    0.611    0.336
# ATTOBWO (.15.)    1.072    0.079   13.580    0.000    1.072    0.422
# ATTOBTR (.16.)    1.057    0.078   13.602    0.000    1.057    0.439
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.925    0.213    4.335    0.000    0.925    0.426
# .ATALC3M ~~                                                            
#   .ATALCURG          1.157    0.193    5.994    0.000    1.157    0.483
# ETOH ~~                                                               
#   TOB               0.319    0.043    7.347    0.000    0.319    0.319
# ETOH_TOB         -0.046    0.089   -0.520    0.603   -0.046   -0.046
# TOB ~~                                                                
#   ETOH_TOB          0.078    0.069    1.136    0.256    0.078    0.078
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           1.998    0.075   26.493    0.000    1.998    1.065
# .ATALCURG          1.284    0.085   15.188    0.000    1.284    0.610
# .ATALCPRB          0.481    0.062    7.739    0.000    0.481    0.311
# .ATALCEXP          0.506    0.068    7.396    0.000    0.506    0.297
# .ATALCWOR          1.071    0.076   14.021    0.000    1.071    0.563
# .ATALCTRY          0.892    0.073   12.196    0.000    0.892    0.490
# .ATTOB3M           2.942    0.112   26.238    0.000    2.942    1.054
# .ATTOBURG          2.674    0.114   23.389    0.000    2.674    0.940
# .ATTOBPRB          1.019    0.091   11.151    0.000    1.019    0.448
# .ATTOBWOR          2.108    0.102   20.646    0.000    2.108    0.830
# .ATTOBTRY          2.171    0.097   22.447    0.000    2.171    0.902
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TOB          0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.299    0.178   12.915    0.000    2.299    0.653
# .ATALCURG          2.491    0.248   10.044    0.000    2.491    0.563
# .ATALCPRB          1.237    0.251    4.932    0.000    1.237    0.516
# .ATALCEXP          1.509    0.316    4.781    0.000    1.509    0.521
# .ATALCWOR          1.798    0.210    8.581    0.000    1.798    0.498
# .ATALCTRY          1.963    0.225    8.737    0.000    1.963    0.593
# .ATTOB3M           1.094    0.162    6.771    0.000    1.094    0.141
# .ATTOBURG          0.873    0.170    5.133    0.000    0.873    0.108
# .ATTOBPRB          3.818    0.346   11.042    0.000    3.818    0.738
# .ATTOBWOR          3.307    0.239   13.828    0.000    3.307    0.512
# .ATTOBTRY          2.507    0.227   11.029    0.000    2.507    0.433
# 
# >

################# Chi-square difference test between Configural invariance and metric invariance. 

anova(AST.Etoh_Tob.model.GENDER.fit, AST.Etoh_Tob.model.GENDER.METRIC.fit)
# > anova(AST.Etoh_Tob.model.GENDER.fit, AST.Etoh_Tob.model.GENDER.METRIC.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.GENDER.fit        68         116.69                              
# AST.Etoh_Tob.model.GENDER.METRIC.fit 84         121.69     8.2728      16     0.9403
# >


################### scalar invariance

AST.Etoh_Tob.model.GENDER.SCALAR.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                   data.etoh_tob.dems.GENDER.02,
                                                   std.lv=TRUE,
                                                   estimator = "WLSMV",
                                                   group = "GENDER",
                                                   group.equal=c("loadings", "intercepts"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.GENDER.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.GENDER.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 112 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         89
# Number of equality constraints                    27
# 
# Number of observations per group:                   
#   2                                              812
# 1                                              619
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               124.396     165.769
# Degrees of freedom                                92          92
# P-value (Chi-square)                           0.014       0.000
# Scaling correction factor                                  0.974
# Shift parameter for each group:                                 
#   2                                                     21.609
# 1                                                     16.473
# simple second-order correction                             
# Test statistic for each group:
#   2                                           78.158     101.834
# 1                                           46.238      63.934
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10505.671    4234.116
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.521
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.982
# Tucker-Lewis Index (TLI)                       0.996       0.979
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.022       0.033
# 90 Percent confidence interval - lower         0.010       0.025
# 90 Percent confidence interval - upper         0.032       0.042
# P-value RMSEA <= 0.05                          1.000       1.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.043       0.043
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.470    0.000    0.978    0.515
# ATALCUR (.p2.)    1.391    0.067   20.917    0.000    1.391    0.639
# ATALCPR (.p3.)    1.076    0.081   13.210    0.000    1.076    0.685
# ATALCEX (.p4.)    1.177    0.093   12.639    0.000    1.177    0.738
# ATALCWO (.p5.)    1.136    0.074   15.368    0.000    1.136    0.597
# ATALCTR (.p6.)    1.016    0.074   13.751    0.000    1.016    0.576
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.579    0.000    2.586    0.934
# ATTOBUR (.p8.)    2.687    0.032   83.806    0.000    2.687    0.944
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.520
# ATTOBWO (.10.)    1.334    0.074   18.096    0.000    1.334    0.530
# ATTOBTR (.11.)    1.392    0.071   19.576    0.000    1.392    0.576
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.474    0.060   -7.915    0.000   -0.474   -0.250
# ATALCWO (.13.)    0.780    0.076   10.317    0.000    0.780    0.410
# ATALCTR (.14.)    0.611    0.067    9.103    0.000    0.611    0.347
# ATTOBWO (.15.)    1.071    0.079   13.567    0.000    1.071    0.426
# ATTOBTR (.16.)    1.057    0.078   13.594    0.000    1.057    0.437
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.854    0.176    4.845    0.000    0.854    0.390
# .ATALC3M ~~                                                            
#   .ATALCURG          1.365    0.163    8.354    0.000    1.365    0.527
# ETOH ~~                                                               
#   TOB               0.291    0.041    7.133    0.000    0.291    0.291
# ETOH_TOB         -0.039    0.074   -0.536    0.592   -0.039   -0.039
# TOB ~~                                                                
#   ETOH_TOB          0.045    0.062    0.725    0.468    0.045    0.045
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.013    0.060   33.808    0.000    2.013    1.061
# .ATALCUR (.37.)    1.358    0.068   19.931    0.000    1.358    0.623
# .ATALCPR (.38.)    0.477    0.050    9.528    0.000    0.477    0.304
# .ATALCEX (.39.)    0.459    0.052    8.773    0.000    0.459    0.288
# .ATALCWO (.40.)    1.049    0.061   17.079    0.000    1.049    0.551
# .ATALCTR (.41.)    0.858    0.056   15.348    0.000    0.858    0.487
# .ATTOB3M (.42.)    2.920    0.096   30.319    0.000    2.920    1.055
# .ATTOBUR (.43.)    2.675    0.099   26.914    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.066   15.218    0.000    1.006    0.450
# .ATTOBWO (.45.)    2.118    0.081   26.030    0.000    2.118    0.842
# .ATTOBTR (.46.)    2.179    0.079   27.562    0.000    2.179    0.900
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.385    0.160   14.910    0.000    2.385    0.662
# .ATALCURG          2.809    0.217   12.921    0.000    2.809    0.592
# .ATALCPRB          1.313    0.217    6.041    0.000    1.313    0.531
# .ATALCEXP          1.160    0.244    4.759    0.000    1.160    0.456
# .ATALCWOR          1.790    0.202    8.873    0.000    1.790    0.495
# .ATALCTRY          1.750    0.178    9.842    0.000    1.750    0.563
# .ATTOB3M           0.979    0.152    6.443    0.000    0.979    0.128
# .ATTOBURG          0.878    0.166    5.282    0.000    0.878    0.108
# .ATTOBPRB          3.654    0.279   13.114    0.000    3.654    0.729
# .ATTOBWOR          3.277    0.222   14.733    0.000    3.277    0.518
# .ATTOBTRY          2.665    0.212   12.542    0.000    2.665    0.455
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.470    0.000    0.978    0.521
# ATALCUR (.p2.)    1.391    0.067   20.917    0.000    1.391    0.661
# ATALCPR (.p3.)    1.076    0.081   13.210    0.000    1.076    0.695
# ATALCEX (.p4.)    1.177    0.093   12.639    0.000    1.177    0.692
# ATALCWO (.p5.)    1.136    0.074   15.368    0.000    1.136    0.597
# ATALCTR (.p6.)    1.016    0.074   13.751    0.000    1.016    0.558
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.579    0.000    2.586    0.927
# ATTOBUR (.p8.)    2.687    0.032   83.806    0.000    2.687    0.945
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.512
# ATTOBWO (.10.)    1.334    0.074   18.096    0.000    1.334    0.525
# ATTOBTR (.11.)    1.392    0.071   19.576    0.000    1.392    0.579
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.474    0.060   -7.915    0.000   -0.474   -0.253
# ATALCWO (.13.)    0.780    0.076   10.317    0.000    0.780    0.411
# ATALCTR (.14.)    0.611    0.067    9.103    0.000    0.611    0.336
# ATTOBWO (.15.)    1.071    0.079   13.567    0.000    1.071    0.422
# ATTOBTR (.16.)    1.057    0.078   13.594    0.000    1.057    0.439
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.925    0.213    4.335    0.000    0.925    0.426
# .ATALC3M ~~                                                            
#   .ATALCURG          1.157    0.193    5.994    0.000    1.157    0.483
# ETOH ~~                                                               
#   TOB               0.319    0.043    7.347    0.000    0.319    0.319
# ETOH_TOB         -0.047    0.089   -0.521    0.602   -0.047   -0.047
# TOB ~~                                                                
#   ETOH_TOB          0.078    0.069    1.135    0.257    0.078    0.078
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.013    0.060   33.808    0.000    2.013    1.073
# .ATALCUR (.37.)    1.358    0.068   19.931    0.000    1.358    0.645
# .ATALCPR (.38.)    0.477    0.050    9.528    0.000    0.477    0.308
# .ATALCEX (.39.)    0.459    0.052    8.773    0.000    0.459    0.270
# .ATALCWO (.40.)    1.049    0.061   17.079    0.000    1.049    0.552
# .ATALCTR (.41.)    0.858    0.056   15.348    0.000    0.858    0.472
# .ATTOB3M (.42.)    2.920    0.096   30.319    0.000    2.920    1.047
# .ATTOBUR (.43.)    2.675    0.099   26.914    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.066   15.218    0.000    1.006    0.442
# .ATTOBWO (.45.)    2.118    0.081   26.030    0.000    2.118    0.834
# .ATTOBTR (.46.)    2.179    0.079   27.562    0.000    2.179    0.905
# ETOH              0.003    0.062    0.050    0.960    0.003    0.003
# TOB              -0.000    0.056   -0.001    0.999   -0.000   -0.000
# ETOH_TO           0.013    0.073    0.173    0.863    0.013    0.013
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.299    0.178   12.898    0.000    2.299    0.653
# .ATALCURG          2.492    0.248   10.049    0.000    2.492    0.563
# .ATALCPRB          1.237    0.251    4.933    0.000    1.237    0.516
# .ATALCEXP          1.509    0.316    4.781    0.000    1.509    0.521
# .ATALCWOR          1.797    0.209    8.591    0.000    1.797    0.497
# .ATALCTRY          1.962    0.224    8.747    0.000    1.962    0.593
# .ATTOB3M           1.094    0.162    6.772    0.000    1.094    0.141
# .ATTOBURG          0.873    0.170    5.134    0.000    0.873    0.108
# .ATTOBPRB          3.818    0.346   11.043    0.000    3.818    0.738
# .ATTOBWOR          3.308    0.239   13.860    0.000    3.308    0.512
# .ATTOBTRY          2.508    0.227   11.054    0.000    2.508    0.433
# 
# >


############################ Test difference

anova(AST.Etoh_Tob.model.GENDER.METRIC.fit, AST.Etoh_Tob.model.GENDER.SCALAR.fit)
# > anova(AST.Etoh_Tob.model.GENDER.METRIC.fit, AST.Etoh_Tob.model.GENDER.SCALAR.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.GENDER.METRIC.fit 84         121.69                              
# AST.Etoh_Tob.model.GENDER.SCALAR.fit 92         124.40     6.0059       8     0.6466
# >

#equal factor means specification

AST.Etoh_Tob.model.GENDER.MEANS.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                  data.etoh_tob.dems.GENDER.02,
                                                  std.lv=TRUE,
                                                  estimator = "WLSMV",
                                                  group = "GENDER",
                                                  group.equal=c("loadings", "intercepts", "means"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.GENDER.MEANS.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob.model.GENDER.MEANS.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 108 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# Number of equality constraints                    27
# 
# Number of observations per group:                   
#   2                                              812
# 1                                              619
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               124.453     152.958
# Degrees of freedom                                95          95
# P-value (Chi-square)                           0.023       0.000
# Scaling correction factor                                  1.117
# Shift parameter for each group:                                 
#   2                                                     23.549
# 1                                                     17.951
# simple second-order correction                             
# Test statistic for each group:
#   2                                           78.172      93.558
# 1                                           46.281      59.400
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10505.671    4234.116
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.521
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.986
# Tucker-Lewis Index (TLI)                       0.997       0.984
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.021       0.029
# 90 Percent confidence interval - lower         0.008       0.020
# 90 Percent confidence interval - upper         0.030       0.038
# P-value RMSEA <= 0.05                          1.000       1.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.043       0.043
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.473    0.000    0.978    0.515
# ATALCUR (.p2.)    1.391    0.067   20.919    0.000    1.391    0.639
# ATALCPR (.p3.)    1.076    0.081   13.209    0.000    1.076    0.685
# ATALCEX (.p4.)    1.177    0.093   12.638    0.000    1.177    0.738
# ATALCWO (.p5.)    1.135    0.074   15.368    0.000    1.135    0.597
# ATALCTR (.p6.)    1.016    0.074   13.751    0.000    1.016    0.576
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.579    0.000    2.586    0.934
# ATTOBUR (.p8.)    2.687    0.032   83.802    0.000    2.687    0.944
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.520
# ATTOBWO (.10.)    1.333    0.074   18.086    0.000    1.333    0.530
# ATTOBTR (.11.)    1.392    0.071   19.566    0.000    1.392    0.575
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.474    0.060   -7.921    0.000   -0.474   -0.250
# ATALCWO (.13.)    0.780    0.076   10.318    0.000    0.780    0.410
# ATALCTR (.14.)    0.611    0.067    9.090    0.000    0.611    0.346
# ATTOBWO (.15.)    1.072    0.079   13.580    0.000    1.072    0.426
# ATTOBTR (.16.)    1.057    0.078   13.602    0.000    1.057    0.437
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.854    0.176    4.845    0.000    0.854    0.390
# .ATALC3M ~~                                                            
#   .ATALCURG          1.365    0.163    8.356    0.000    1.365    0.527
# ETOH ~~                                                               
#   TOB               0.291    0.041    7.133    0.000    0.291    0.291
# ETOH_TOB         -0.039    0.074   -0.534    0.593   -0.039   -0.039
# TOB ~~                                                                
#   ETOH_TOB          0.045    0.062    0.727    0.467    0.045    0.045
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.012    0.050   40.291    0.000    2.012    1.060
# .ATALCUR (.37.)    1.360    0.057   23.976    0.000    1.360    0.624
# .ATALCPR (.38.)    0.479    0.041   11.598    0.000    0.479    0.304
# .ATALCEX (.39.)    0.460    0.043   10.623    0.000    0.460    0.288
# .ATALCWO (.40.)    1.055    0.050   20.977    0.000    1.055    0.554
# .ATALCTR (.41.)    0.863    0.047   18.272    0.000    0.863    0.490
# .ATTOB3M (.42.)    2.920    0.073   39.765    0.000    2.920    1.055
# .ATTOBUR (.43.)    2.675    0.075   35.570    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.060   16.887    0.000    1.006    0.450
# .ATTOBWO (.45.)    2.124    0.067   31.795    0.000    2.124    0.844
# .ATTOBTR (.46.)    2.184    0.064   34.232    0.000    2.184    0.903
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.386    0.160   14.910    0.000    2.386    0.662
# .ATALCURG          2.809    0.217   12.924    0.000    2.809    0.592
# .ATALCPRB          1.313    0.217    6.042    0.000    1.313    0.531
# .ATALCEXP          1.161    0.244    4.761    0.000    1.161    0.456
# .ATALCWOR          1.791    0.201    8.893    0.000    1.791    0.495
# .ATALCTRY          1.751    0.178    9.862    0.000    1.751    0.564
# .ATTOB3M           0.978    0.152    6.442    0.000    0.978    0.128
# .ATTOBURG          0.878    0.166    5.281    0.000    0.878    0.108
# .ATTOBPRB          3.654    0.279   13.114    0.000    3.654    0.729
# .ATTOBWOR          3.276    0.222   14.751    0.000    3.276    0.517
# .ATTOBTRY          2.664    0.212   12.556    0.000    2.664    0.455
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.473    0.000    0.978    0.521
# ATALCUR (.p2.)    1.391    0.067   20.919    0.000    1.391    0.661
# ATALCPR (.p3.)    1.076    0.081   13.209    0.000    1.076    0.695
# ATALCEX (.p4.)    1.177    0.093   12.638    0.000    1.177    0.692
# ATALCWO (.p5.)    1.135    0.074   15.368    0.000    1.135    0.597
# ATALCTR (.p6.)    1.016    0.074   13.751    0.000    1.016    0.558
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.579    0.000    2.586    0.927
# ATTOBUR (.p8.)    2.687    0.032   83.802    0.000    2.687    0.945
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.512
# ATTOBWO (.10.)    1.333    0.074   18.086    0.000    1.333    0.525
# ATTOBTR (.11.)    1.392    0.071   19.566    0.000    1.392    0.578
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.474    0.060   -7.921    0.000   -0.474   -0.252
# ATALCWO (.13.)    0.780    0.076   10.318    0.000    0.780    0.410
# ATALCTR (.14.)    0.611    0.067    9.090    0.000    0.611    0.336
# ATTOBWO (.15.)    1.072    0.079   13.580    0.000    1.072    0.422
# ATTOBTR (.16.)    1.057    0.078   13.602    0.000    1.057    0.439
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.925    0.213    4.335    0.000    0.925    0.426
# .ATALC3M ~~                                                            
#   .ATALCURG          1.157    0.193    5.994    0.000    1.157    0.483
# ETOH ~~                                                               
#   TOB               0.319    0.043    7.347    0.000    0.319    0.319
# ETOH_TOB         -0.046    0.089   -0.520    0.603   -0.046   -0.046
# TOB ~~                                                                
#   ETOH_TOB          0.078    0.069    1.136    0.256    0.078    0.078
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.012    0.050   40.291    0.000    2.012    1.072
# .ATALCUR (.37.)    1.360    0.057   23.976    0.000    1.360    0.646
# .ATALCPR (.38.)    0.479    0.041   11.598    0.000    0.479    0.309
# .ATALCEX (.39.)    0.460    0.043   10.623    0.000    0.460    0.271
# .ATALCWO (.40.)    1.055    0.050   20.977    0.000    1.055    0.555
# .ATALCTR (.41.)    0.863    0.047   18.272    0.000    0.863    0.474
# .ATTOB3M (.42.)    2.920    0.073   39.765    0.000    2.920    1.047
# .ATTOBUR (.43.)    2.675    0.075   35.570    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.060   16.887    0.000    1.006    0.442
# .ATTOBWO (.45.)    2.124    0.067   31.795    0.000    2.124    0.836
# .ATTOBTR (.46.)    2.184    0.064   34.232    0.000    2.184    0.908
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TOB          1.000                               1.000    1.000
# .ATALC3M           2.299    0.178   12.915    0.000    2.299    0.653
# .ATALCURG          2.491    0.248   10.044    0.000    2.491    0.563
# .ATALCPRB          1.237    0.251    4.932    0.000    1.237    0.516
# .ATALCEXP          1.509    0.316    4.781    0.000    1.509    0.521
# .ATALCWOR          1.798    0.210    8.581    0.000    1.798    0.498
# .ATALCTRY          1.963    0.225    8.737    0.000    1.963    0.593
# .ATTOB3M           1.094    0.162    6.771    0.000    1.094    0.141
# .ATTOBURG          0.873    0.170    5.133    0.000    0.873    0.108
# .ATTOBPRB          3.818    0.346   11.043    0.000    3.818    0.738
# .ATTOBWOR          3.307    0.239   13.828    0.000    3.307    0.512
# .ATTOBTRY          2.507    0.227   11.029    0.000    2.507    0.433
# 
# >


anova(AST.Etoh_Tob.model.GENDER.SCALAR.fit, AST.Etoh_Tob.model.GENDER.MEANS.fit)
# > anova(AST.Etoh_Tob.model.GENDER.SCALAR.fit, AST.Etoh_Tob.model.GENDER.MEANS.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.GENDER.SCALAR.fit 92         124.40                              
# AST.Etoh_Tob.model.GENDER.MEANS.fit  95         124.45    0.44247       3     0.9313
# >


# Note: Figure out what this means considering these models significantly differ.


################### strict invariance

AST.Etoh_Tob.model.GENDER.STRICT.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                   data.etoh_tob.dems.GENDER.02,
                                                   std.lv=TRUE,
                                                   estimator = "WLSMV",
                                                   group = "GENDER",
                                                   group.equal=c("loadings", "intercepts", "residuals"))

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.GENDER.STRICT.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.Etoh_Tob.model.GENDER.STRICT.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 100 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         89
# Number of equality constraints                    38
# 
# Number of observations per group:                   
#   2                                              812
# 1                                              619
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               127.399     161.468
# Degrees of freedom                               103         103
# P-value (Chi-square)                           0.052       0.000
# Scaling correction factor                                  1.106
# Shift parameter for each group:                                 
#   2                                                     26.280
# 1                                                     20.033
# simple second-order correction                             
# Test statistic for each group:
#   2                                           79.441      98.086
# 1                                           47.957      63.382
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10505.671    4234.116
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.521
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.998       0.986
# Tucker-Lewis Index (TLI)                       0.997       0.985
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.018       0.028
# 90 Percent confidence interval - lower         0.000       0.019
# 90 Percent confidence interval - upper         0.028       0.036
# P-value RMSEA <= 0.05                          1.000       1.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.044       0.044
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.478    0.000    0.978    0.518
# ATALCUR (.p2.)    1.391    0.067   20.914    0.000    1.391    0.648
# ATALCPR (.p3.)    1.076    0.081   13.210    0.000    1.076    0.690
# ATALCEX (.p4.)    1.177    0.093   12.640    0.000    1.177    0.717
# ATALCWO (.p5.)    1.136    0.074   15.372    0.000    1.136    0.598
# ATALCTR (.p6.)    1.016    0.074   13.748    0.000    1.016    0.569
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.619    0.000    2.586    0.931
# ATTOBUR (.p8.)    2.687    0.032   83.830    0.000    2.687    0.944
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.517
# ATTOBWO (.10.)    1.334    0.074   18.072    0.000    1.334    0.529
# ATTOBTR (.11.)    1.393    0.071   19.653    0.000    1.393    0.579
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.475    0.060   -7.926    0.000   -0.475   -0.251
# ATALCWO (.13.)    0.782    0.076   10.330    0.000    0.782    0.411
# ATALCTR (.14.)    0.611    0.067    9.113    0.000    0.611    0.342
# ATTOBWO (.15.)    1.075    0.079   13.598    0.000    1.075    0.427
# ATTOBTR (.16.)    1.051    0.078   13.518    0.000    1.051    0.437
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.854    0.176    4.838    0.000    0.854    0.391
# .ATALC3M ~~                                                            
#   .ATALCURG          1.361    0.163    8.371    0.000    1.361    0.543
# ETOH ~~                                                               
#   TOB               0.291    0.041    7.158    0.000    0.291    0.291
# ETOH_TOB         -0.045    0.073   -0.623    0.533   -0.045   -0.045
# TOB ~~                                                                
#   ETOH_TOB          0.048    0.061    0.783    0.434    0.048    0.048
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.013    0.060   33.799    0.000    2.013    1.066
# .ATALCUR (.37.)    1.358    0.068   19.932    0.000    1.358    0.632
# .ATALCPR (.38.)    0.477    0.050    9.528    0.000    0.477    0.306
# .ATALCEX (.39.)    0.459    0.052    8.773    0.000    0.459    0.279
# .ATALCWO (.40.)    1.049    0.061   17.074    0.000    1.049    0.552
# .ATALCTR (.41.)    0.858    0.056   15.347    0.000    0.858    0.481
# .ATTOB3M (.42.)    2.920    0.096   30.319    0.000    2.920    1.051
# .ATTOBUR (.43.)    2.675    0.099   26.915    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.066   15.218    0.000    1.006    0.446
# .ATTOBWO (.45.)    2.118    0.081   26.012    0.000    2.118    0.840
# .ATTOBTR (.46.)    2.179    0.079   27.587    0.000    2.179    0.905
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.346    0.118   19.872    0.000    2.346    0.657
# .ATALCUR (.23.)    2.680    0.175   15.336    0.000    2.680    0.581
# .ATALCPR (.24.)    1.278    0.112   11.383    0.000    1.278    0.524
# .ATALCEX (.25.)    1.309    0.121   10.778    0.000    1.309    0.486
# .ATALCWO (.26.)    1.791    0.145   12.365    0.000    1.791    0.496
# .ATALCTR (.27.)    1.840    0.125   14.697    0.000    1.840    0.577
# .ATTOB3M (.28.)    1.031    0.134    7.689    0.000    1.031    0.134
# .ATTOBUR (.29.)    0.876    0.151    5.780    0.000    0.876    0.108
# .ATTOBPR (.30.)    3.724    0.158   23.508    0.000    3.724    0.733
# .ATTOBWO (.31.)    3.281    0.191   17.204    0.000    3.281    0.517
# .ATTOBTR (.32.)    2.610    0.180   14.498    0.000    2.610    0.450
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.978    0.059   16.478    0.000    0.978    0.518
# ATALCUR (.p2.)    1.391    0.067   20.914    0.000    1.391    0.648
# ATALCPR (.p3.)    1.076    0.081   13.210    0.000    1.076    0.690
# ATALCEX (.p4.)    1.177    0.093   12.640    0.000    1.177    0.717
# ATALCWO (.p5.)    1.136    0.074   15.372    0.000    1.136    0.597
# ATALCTR (.p6.)    1.016    0.074   13.748    0.000    1.016    0.568
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.586    0.032   81.619    0.000    2.586    0.931
# ATTOBUR (.p8.)    2.687    0.032   83.830    0.000    2.687    0.944
# ATTOBPR (.p9.)    1.165    0.063   18.342    0.000    1.165    0.517
# ATTOBWO (.10.)    1.334    0.074   18.072    0.000    1.334    0.526
# ATTOBTR (.11.)    1.393    0.071   19.653    0.000    1.393    0.575
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.475    0.060   -7.926    0.000   -0.475   -0.251
# ATALCWO (.13.)    0.782    0.076   10.330    0.000    0.782    0.411
# ATALCTR (.14.)    0.611    0.067    9.113    0.000    0.611    0.342
# ATTOBWO (.15.)    1.075    0.079   13.598    0.000    1.075    0.424
# ATTOBTR (.16.)    1.051    0.078   13.518    0.000    1.051    0.434
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPRB          0.926    0.214    4.334    0.000    0.926    0.425
# .ATALC3M ~~                                                            
#   .ATALCURG          1.161    0.191    6.066    0.000    1.161    0.463
# ETOH ~~                                                               
#   TOB               0.319    0.044    7.323    0.000    0.319    0.319
# ETOH_TOB         -0.040    0.086   -0.465    0.642   -0.040   -0.040
# TOB ~~                                                                
#   ETOH_TOB          0.074    0.067    1.115    0.265    0.074    0.074
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.013    0.060   33.799    0.000    2.013    1.066
# .ATALCUR (.37.)    1.358    0.068   19.932    0.000    1.358    0.632
# .ATALCPR (.38.)    0.477    0.050    9.528    0.000    0.477    0.306
# .ATALCEX (.39.)    0.459    0.052    8.773    0.000    0.459    0.279
# .ATALCWO (.40.)    1.049    0.061   17.074    0.000    1.049    0.551
# .ATALCTR (.41.)    0.858    0.056   15.347    0.000    0.858    0.480
# .ATTOB3M (.42.)    2.920    0.096   30.319    0.000    2.920    1.051
# .ATTOBUR (.43.)    2.675    0.099   26.915    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.066   15.218    0.000    1.006    0.446
# .ATTOBWO (.45.)    2.118    0.081   26.012    0.000    2.118    0.835
# .ATTOBTR (.46.)    2.179    0.079   27.587    0.000    2.179    0.899
# ETOH              0.003    0.062    0.050    0.960    0.003    0.003
# TOB              -0.000    0.056   -0.001    0.999   -0.000   -0.000
# ETOH_TO           0.013    0.073    0.174    0.862    0.013    0.013
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.346    0.118   19.872    0.000    2.346    0.658
# .ATALCUR (.23.)    2.680    0.175   15.336    0.000    2.680    0.581
# .ATALCPR (.24.)    1.278    0.112   11.383    0.000    1.278    0.524
# .ATALCEX (.25.)    1.309    0.121   10.778    0.000    1.309    0.486
# .ATALCWO (.26.)    1.791    0.145   12.365    0.000    1.791    0.495
# .ATALCTR (.27.)    1.840    0.125   14.697    0.000    1.840    0.576
# .ATTOB3M (.28.)    1.031    0.134    7.689    0.000    1.031    0.134
# .ATTOBUR (.29.)    0.876    0.151    5.780    0.000    0.876    0.108
# .ATTOBPR (.30.)    3.724    0.158   23.508    0.000    3.724    0.733
# .ATTOBWO (.31.)    3.281    0.191   17.204    0.000    3.281    0.510
# .ATTOBTR (.32.)    2.610    0.180   14.498    0.000    2.610    0.444
# 
# > 

anova(AST.Etoh_Tob.model.GENDER.SCALAR.fit, AST.Etoh_Tob.model.GENDER.STRICT.fit)
# > anova(AST.Etoh_Tob.model.GENDER.SCALAR.fit, AST.Etoh_Tob.model.GENDER.STRICT.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC Chisq Chisq diff Df diff Pr(>Chisq)
# AST.Etoh_Tob.model.GENDER.SCALAR.fit  92         124.4                              
# AST.Etoh_Tob.model.GENDER.STRICT.fit 103         127.4      5.399      11     0.9103
# >

################### factor variances and covariances
AST.Etoh_Tob.model.GENDER.FVCV.fit  <- lavaan::cfa(AST.Etoh_Tob.model,
                                                 data.etoh_tob.dems.GENDER.02,
                                                 std.lv=TRUE,
                                                 estimator = "WLSMV",
                                                 group = "GENDER",
                                                 group.equal=c("loadings", "intercepts",
                                                               "residuals", "lv.variances", "lv.covariances"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.GENDER.FVCV.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.Etoh_Tob.model.GENDER.FVCV.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 118 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         89
# Number of equality constraints                    41
# 
# Number of observations per group:                   
#   2                                              812
# 1                                              619
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               130.032     137.754
# Degrees of freedom                               106         106
# P-value (Chi-square)                           0.056       0.021
# Scaling correction factor                                  1.568
# Shift parameter for each group:                                 
#   2                                                     31.113
# 1                                                     23.718
# simple second-order correction                             
# Test statistic for each group:
#   2                                           80.469      82.430
# 1                                           49.563      55.325
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10505.671    4234.116
# Degrees of freedom                               110         110
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.521
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.998       0.992
# Tucker-Lewis Index (TLI)                       0.998       0.992
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.018       0.020
# 90 Percent confidence interval - lower         0.000       0.008
# 90 Percent confidence interval - upper         0.027       0.030
# P-value RMSEA <= 0.05                          1.000       1.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.045       0.045
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.977    0.059   16.440    0.000    0.977    0.517
# ATALCUR (.p2.)    1.392    0.067   20.904    0.000    1.392    0.648
# ATALCPR (.p3.)    1.078    0.082   13.215    0.000    1.078    0.690
# ATALCEX (.p4.)    1.176    0.093   12.626    0.000    1.176    0.717
# ATALCWO (.p5.)    1.136    0.074   15.344    0.000    1.136    0.597
# ATALCTR (.p6.)    1.016    0.074   13.755    0.000    1.016    0.569
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.585    0.032   81.888    0.000    2.585    0.930
# ATTOBUR (.p8.)    2.688    0.032   84.409    0.000    2.688    0.945
# ATTOBPR (.p9.)    1.164    0.064   18.333    0.000    1.164    0.517
# ATTOBWO (.10.)    1.333    0.074   18.089    0.000    1.333    0.528
# ATTOBTR (.11.)    1.395    0.071   19.752    0.000    1.395    0.578
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.477    0.060   -7.986    0.000   -0.477   -0.253
# ATALCWO (.13.)    0.779    0.075   10.314    0.000    0.779    0.410
# ATALCTR (.14.)    0.609    0.067    9.054    0.000    0.609    0.341
# ATTOBWO (.15.)    1.074    0.079   13.654    0.000    1.074    0.425
# ATTOBTR (.16.)    1.053    0.078   13.550    0.000    1.053    0.436
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPR           0.838    0.190    4.419    0.000    0.838    0.385
# .ATALC3M ~~                                                            
#   .ATALCUR           1.363    0.158    8.612    0.000    1.363    0.544
# ETOH ~~                                                               
#   TOB     (.33.)    0.303    0.028   10.845    0.000    0.303    0.303
# ETOH_TO (.34.)   -0.043    0.060   -0.713    0.476   -0.043   -0.043
# TOB ~~                                                                
#   ETOH_TO (.35.)    0.059    0.053    1.108    0.268    0.059    0.059
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.013    0.060   33.798    0.000    2.013    1.066
# .ATALCUR (.37.)    1.358    0.068   19.929    0.000    1.358    0.632
# .ATALCPR (.38.)    0.477    0.050    9.525    0.000    0.477    0.306
# .ATALCEX (.39.)    0.459    0.052    8.775    0.000    0.459    0.279
# .ATALCWO (.40.)    1.049    0.061   17.080    0.000    1.049    0.551
# .ATALCTR (.41.)    0.858    0.056   15.351    0.000    0.858    0.480
# .ATTOB3M (.42.)    2.920    0.096   30.326    0.000    2.920    1.051
# .ATTOBUR (.43.)    2.675    0.099   26.909    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.066   15.218    0.000    1.006    0.446
# .ATTOBWO (.45.)    2.118    0.081   26.020    0.000    2.118    0.838
# .ATTOBTR (.46.)    2.179    0.079   27.570    0.000    2.179    0.903
# ETOH              0.000                               0.000    0.000
# TOB               0.000                               0.000    0.000
# ETOH_TO           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.345    0.118   19.887    0.000    2.345    0.658
# .ATALCUR (.23.)    2.677    0.175   15.306    0.000    2.677    0.580
# .ATALCPR (.24.)    1.276    0.112   11.358    0.000    1.276    0.524
# .ATALCEX (.25.)    1.311    0.121   10.794    0.000    1.311    0.486
# .ATALCWO (.26.)    1.795    0.145   12.358    0.000    1.795    0.496
# .ATALCTR (.27.)    1.842    0.125   14.699    0.000    1.842    0.577
# .ATTOB3M (.28.)    1.038    0.133    7.788    0.000    1.038    0.134
# .ATTOBUR (.29.)    0.869    0.150    5.780    0.000    0.869    0.107
# .ATTOBPR (.30.)    3.725    0.158   23.502    0.000    3.725    0.733
# .ATTOBWO (.31.)    3.285    0.190   17.265    0.000    3.285    0.514
# .ATTOBTR (.32.)    2.600    0.180   14.454    0.000    2.600    0.446
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH =~                                                               
#   ATALC3M (.p1.)    0.977    0.059   16.440    0.000    0.977    0.517
# ATALCUR (.p2.)    1.392    0.067   20.904    0.000    1.392    0.648
# ATALCPR (.p3.)    1.078    0.082   13.215    0.000    1.078    0.690
# ATALCEX (.p4.)    1.176    0.093   12.626    0.000    1.176    0.717
# ATALCWO (.p5.)    1.136    0.074   15.344    0.000    1.136    0.597
# ATALCTR (.p6.)    1.016    0.074   13.755    0.000    1.016    0.569
# TOB =~                                                                
#   ATTOB3M (.p7.)    2.585    0.032   81.888    0.000    2.585    0.930
# ATTOBUR (.p8.)    2.688    0.032   84.409    0.000    2.688    0.945
# ATTOBPR (.p9.)    1.164    0.064   18.333    0.000    1.164    0.517
# ATTOBWO (.10.)    1.333    0.074   18.089    0.000    1.333    0.528
# ATTOBTR (.11.)    1.395    0.071   19.752    0.000    1.395    0.578
# ETOH_TOB =~                                                           
#   ATALC3M (.12.)   -0.477    0.060   -7.986    0.000   -0.477   -0.253
# ATALCWO (.13.)    0.779    0.075   10.314    0.000    0.779    0.410
# ATALCTR (.14.)    0.609    0.067    9.054    0.000    0.609    0.341
# ATTOBWO (.15.)    1.074    0.079   13.654    0.000    1.074    0.425
# ATTOBTR (.16.)    1.053    0.078   13.550    0.000    1.053    0.436
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCPRB ~~                                                           
#   .ATTOBPR           0.945    0.229    4.118    0.000    0.945    0.434
# .ATALC3M ~~                                                            
#   .ATALCUR           1.159    0.181    6.420    0.000    1.159    0.463
# ETOH ~~                                                               
#   TOB     (.33.)    0.303    0.028   10.845    0.000    0.303    0.303
# ETOH_TO (.34.)   -0.043    0.060   -0.713    0.476   -0.043   -0.043
# TOB ~~                                                                
#   ETOH_TO (.35.)    0.059    0.053    1.108    0.268    0.059    0.059
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M (.36.)    2.013    0.060   33.798    0.000    2.013    1.066
# .ATALCUR (.37.)    1.358    0.068   19.929    0.000    1.358    0.632
# .ATALCPR (.38.)    0.477    0.050    9.525    0.000    0.477    0.306
# .ATALCEX (.39.)    0.459    0.052    8.775    0.000    0.459    0.279
# .ATALCWO (.40.)    1.049    0.061   17.080    0.000    1.049    0.551
# .ATALCTR (.41.)    0.858    0.056   15.351    0.000    0.858    0.480
# .ATTOB3M (.42.)    2.920    0.096   30.326    0.000    2.920    1.051
# .ATTOBUR (.43.)    2.675    0.099   26.909    0.000    2.675    0.940
# .ATTOBPR (.44.)    1.006    0.066   15.218    0.000    1.006    0.446
# .ATTOBWO (.45.)    2.118    0.081   26.020    0.000    2.118    0.838
# .ATTOBTR (.46.)    2.179    0.079   27.570    0.000    2.179    0.903
# ETOH              0.003    0.062    0.050    0.960    0.003    0.003
# TOB              -0.000    0.056   -0.001    0.999   -0.000   -0.000
# ETOH_TO           0.013    0.073    0.173    0.863    0.013    0.013
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# TOB               1.000                               1.000    1.000
# ETOH_TO           1.000                               1.000    1.000
# .ATALC3M (.22.)    2.345    0.118   19.887    0.000    2.345    0.658
# .ATALCUR (.23.)    2.677    0.175   15.306    0.000    2.677    0.580
# .ATALCPR (.24.)    1.276    0.112   11.358    0.000    1.276    0.524
# .ATALCEX (.25.)    1.311    0.121   10.794    0.000    1.311    0.486
# .ATALCWO (.26.)    1.795    0.145   12.358    0.000    1.795    0.496
# .ATALCTR (.27.)    1.842    0.125   14.699    0.000    1.842    0.577
# .ATTOB3M (.28.)    1.038    0.133    7.788    0.000    1.038    0.134
# .ATTOBUR (.29.)    0.869    0.150    5.780    0.000    0.869    0.107
# .ATTOBPR (.30.)    3.725    0.158   23.502    0.000    3.725    0.733
# .ATTOBWO (.31.)    3.285    0.190   17.265    0.000    3.285    0.514
# .ATTOBTR (.32.)    2.600    0.180   14.454    0.000    2.600    0.446
# 
# >








