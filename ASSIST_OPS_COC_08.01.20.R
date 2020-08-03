

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

data.ops.dems <- merge.data.frame(data.ops, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)



data.ops_coc.dems <- merge.data.frame(data.coc, data.ops.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

data.ops_coc_etoh.dems <- merge.data.frame(data.etoh, data.ops_coc.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)


data.ops.dems <- merge.data.frame(data.ops, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                  by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                  suffixes = c(".x", ".y"), incomparables = NULL)


##### Descriptives
psych::describe(data.ops_coc.dems)
# > psych::describe(data.ops_coc.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 335 4835739.03 2989158.36 4741519 4810468.85 3907966.07 10115 9937226 9927111  0.07
# COC_TOT     2 335       6.31       9.14       3       4.18       4.45     0      39      39  1.94
# ATCOC3M     3 335       0.67       1.50       0       0.28       0.00     0       6       6  2.16
# ATCOCURG    4 335       0.73       1.74       0       0.25       0.00     0       6       6  2.11
# ATCOCPRB    5 335       0.59       1.75       0       0.04       0.00     0       7       7  2.72
# ATCOCEXP    6 335       0.65       1.97       0       0.02       0.00     0       8       8  2.79
# ATCOCWOR    7 335       1.93       2.13       3       1.66       4.45     0       6       6  0.64
# ATCOCTRY    8 335       1.75       2.02       0       1.44       0.00     0       6       6  0.73
# OPS_TOT     9 335       8.39      11.02       6       6.05       8.90     0      44      44  1.69
# ATOPS3M    10 335       0.91       1.85       0       0.43       0.00     0       6       6  1.90
# ATOPSURG   11 335       0.99       2.06       0       0.49       0.00     0       6       6  1.71
# ATOPSPRB   12 335       0.72       1.94       0       0.15       0.00     0       7       7  2.43
# ATOPSEXP   13 335       0.85       2.26       0       0.19       0.00     0       8       8  2.38
# ATOPSWOR   14 335       1.88       2.14       0       1.61       0.00     0       6       6  0.68
# ATOPSTRY   15 335       1.83       2.16       0       1.54       0.00     0       6       6  0.74
# ATDRGINJ   16 335       0.41       0.60       0       0.32       0.00     0       2       2  1.13
# ATINJFRQ   17 335       0.81       1.31       0       0.64       0.00     0       3       3  1.03
# GENDER     18 335       2.14       7.42       2       1.58       0.00     1      98      97 12.68
# AGE        19 335      46.54      14.63      49      46.60      14.83    18      89      71 -0.05
# RACE       20 335       2.81       1.10       3       2.67       0.00     1       7       6  1.44
# EDU        21 335      14.36       2.90      15      14.47       2.97     4      21      17 -0.32
# JOB        22 335       4.57      11.81       3       3.02       2.97     1      99      98  7.63
# MARTL      23 335       3.50       1.75       4       3.57       1.48     1       6       5 -0.37
# kurtosis        se
# PROJID      -1.29 163315.17
# COC_TOT      2.95      0.50
# ATCOC3M      3.58      0.08
# ATCOCURG     2.77      0.10
# ATCOCPRB     5.83      0.10
# ATCOCEXP     6.16      0.11
# ATCOCWOR    -0.82      0.12
# ATCOCTRY    -0.60      0.11
# OPS_TOT      2.03      0.60
# ATOPS3M      2.21      0.10
# ATOPSURG     1.16      0.11
# ATOPSPRB     4.25      0.11
# ATOPSEXP     3.98      0.12
# ATOPSWOR    -0.79      0.12
# ATOPSTRY    -0.76      0.12
# ATDRGINJ     0.23      0.03
# ATINJFRQ    -0.91      0.07
# GENDER     160.08      0.41
# AGE         -0.61      0.80
# RACE         2.93      0.06
# EDU          0.13      0.16
# JOB         58.10      0.64
# MARTL       -1.46      0.10
# >


##### Descriptives
psych::describe(data.ops_coc_etoh.dems)
# > psych::describe(data.ops_coc_etoh.dems)
# vars   n       mean         sd    median    trimmed        mad   min     max   range  skew
# PROJID      1 326 4828972.75 2992750.43 4710303.0 4802331.68 3899415.91 10115 9937226 9927111  0.07
# ALC_TOT     2 326       9.09       9.90       6.0       7.33       5.93     0      39      39  1.41
# ATALC3M     3 326       2.08       2.10       2.0       1.85       2.97     0       6       6  0.48
# ATALCURG    4 326       1.67       2.34       0.0       1.34       0.00     0       6       6  0.82
# ATALCPRB    5 326       0.95       2.18       0.0       0.39       0.00     0       7       7  1.95
# ATALCEXP    6 326       0.89       2.24       0.0       0.27       0.00     0       8       8  2.23
# ATALCWOR    7 326       1.91       2.19       0.0       1.65       0.00     0       6       6  0.67
# ATALCTRY    8 326       1.59       2.15       0.0       1.25       0.00     0       6       6  0.97
# COC_TOT     9 326       6.12       8.94       3.0       4.03       4.45     0      39      39  1.94
# ATCOC3M    10 326       0.65       1.46       0.0       0.27       0.00     0       6       6  2.15
# ATCOCURG   11 326       0.71       1.72       0.0       0.24       0.00     0       6       6  2.13
# ATCOCPRB   12 326       0.57       1.72       0.0       0.03       0.00     0       7       7  2.78
# ATCOCEXP   13 326       0.63       1.93       0.0       0.00       0.00     0       8       8  2.86
# ATCOCWOR   14 326       1.87       2.12       0.0       1.59       0.00     0       6       6  0.68
# ATCOCTRY   15 326       1.69       2.01       0.0       1.37       0.00     0       6       6  0.77
# OPS_TOT    16 326       8.26      10.97       4.0       5.92       5.93     0      44      44  1.69
# ATOPS3M    17 326       0.91       1.85       0.0       0.44       0.00     0       6       6  1.89
# ATOPSURG   18 326       0.98       2.06       0.0       0.50       0.00     0       6       6  1.71
# ATOPSPRB   19 326       0.71       1.92       0.0       0.14       0.00     0       7       7  2.46
# ATOPSEXP   20 326       0.83       2.24       0.0       0.17       0.00     0       8       8  2.42
# ATOPSWOR   21 326       1.84       2.15       0.0       1.56       0.00     0       6       6  0.72
# ATOPSTRY   22 326       1.79       2.17       0.0       1.49       0.00     0       6       6  0.78
# ATDRGINJ   23 326       0.40       0.59       0.0       0.31       0.00     0       2       2  1.16
# ATINJFRQ   24 326       0.79       1.30       0.0       0.62       0.00     0       3       3  1.05
# GENDER     25 326       2.15       7.52       2.0       1.58       0.00     1      98      97 12.51
# AGE        26 326      46.44      14.67      48.5      46.50      15.57    18      89      71 -0.05
# RACE       27 326       2.83       1.10       3.0       2.67       0.00     1       7       6  1.46
# EDU        28 326      14.33       2.90      15.0      14.44       2.97     4      21      17 -0.31
# JOB        29 326       4.59      11.96       3.0       3.01       2.97     1      99      98  7.53
# MARTL      30 326       3.52       1.73       4.0       3.60       1.48     1       6       5 -0.40
# kurtosis        se
# PROJID      -1.30 165753.12
# ALC_TOT      1.04      0.55
# ATALC3M     -1.03      0.12
# ATALCURG    -1.10      0.13
# ATALCPRB     2.02      0.12
# ATALCEXP     3.27      0.12
# ATALCWOR    -0.86      0.12
# ATALCTRY    -0.45      0.12
# COC_TOT      2.97      0.50
# ATCOC3M      3.58      0.08
# ATCOCURG     2.86      0.10
# ATCOCPRB     6.16      0.10
# ATCOCEXP     6.55      0.11
# ATCOCWOR    -0.77      0.12
# ATCOCTRY    -0.54      0.11
# OPS_TOT      2.00      0.61
# ATOPS3M      2.19      0.10
# ATOPSURG     1.15      0.11
# ATOPSPRB     4.40      0.11
# ATOPSEXP     4.14      0.12
# ATOPSWOR    -0.77      0.12
# ATOPSTRY    -0.72      0.12
# ATDRGINJ     0.32      0.03
# ATINJFRQ    -0.85      0.07
# GENDER     155.65      0.42
# AGE         -0.61      0.81
# RACE         2.90      0.06
# EDU          0.14      0.16
# JOB         56.53      0.66
# MARTL       -1.43      0.10
# >

### Convert to data.frame
data.frame(data.ops_coc.dems)


### Provide colnames to dataframes
colnames(data.ops_coc.dems) <- c("PROJID", "COC_TOT", "ATCOC3M", "ATCOCURG", "ATCOCPRB",
                                 "ATCOCEXP", "ATCOCWOR", "ATCOCTRY", "OPS_TOT", "ATOPS3M",
                                 "ATOPSURG", "ATOPSPRB", "ATOPSEXP", "ATOPSWOR", "ATOPSTRY",
                                 "ATDRGINJ", "ATINJFRQ", "GENDER", "AGE", "RACE", "EDU", "JOB", "MARTL")


##### Create data sets for the ESEM and invariance testing

(data.ops_coc.dems  <- subset(data.ops_coc.dems, select = c(ATCOC3M, ATCOCURG, ATCOCPRB,
                                 ATCOCEXP, ATCOCWOR, ATCOCTRY, ATOPS3M, ATOPSURG, ATOPSPRB,
                                 ATOPSEXP, ATOPSWOR, ATOPSTRY, ATDRGINJ, ATINJFRQ, GENDER,
                                 AGE, RACE, EDU, JOB, MARTL)))


##### Export the data 

write.table(data.ops_coc.dems, file = "data.ops_coc.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)
# > write.table(data.ops_coc.dems, file = "data.ops_coc.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)
# >



##################################################################################################
##################################################################################################
##################################################################################################

############### CFA

############### MODEL.ETOH_TOB_THC

AST.Etoh_Tob_Thc.model <- ' THC =~ ATTHC3M  + ATTHC3M + ATTHCURG + ATTHCPRB + ATTHCEXP + ATTHCWOR + ATTHCTRY   
                        
                            ETOH =~ NA*ATALC3M + ATALC3M + ATALCURG + ATALCPRB + ATALCEXP + ATALCWOR + ATALCTRY
                        
                            THC_ETOH_TOB =~ NA*ATTHC3M + ATTHC3M + ATTHCURG + ATALCEXP + ATALCWOR + ATALCTRY + 
                                                     ATTOB3M + ATTOBURG + ATTOBPRB + ATTOBWOR + ATTOBTRY
                        
                        
                        ATALCTRY ~~ ATALCWOR
                        ATTOBURG ~~ ATTOB3M
                        ATALCTRY ~~ ATTHCTRY
                        ATALCWOR ~~ ATTHCWOR
                        ATTHCTRY ~~ ATTHCWOR
                        
                        ETOH ~~ 1*ETOH
                        THC ~~ 1*THC
                        THC_ETOH_TOB ~~ 1*THC_ETOH_TOB'


############### MODEL.fit


data.etoh_tob_thc.dems.fit <- lavaan::cfa(AST.Etoh_Tob_Thc.model, 
                                          data.etoh_tob_thc.dems, 
                                          std.lv=TRUE, 
                                          estimator = "WLSMV", 
                                          ordered = c("ATTHC3M", "ATTHCURG",
                                                      "ATTHCPRB", "ATTHCEXP",
                                                      "ATTHCWOR", "ATTHCTRY",
                                                      
                                                      "ATALC3M", "ATALCURG",
                                                      "ATALCPRB", "ATALCEXP",
                                                      "ATALCWOR", "ATALCTRY",
                                                      
                                                      "ATTOB3M", "ATTOBURG",
                                                      "ATTOBPRB", "ATTOBWOR",
                                                      "ATTOBTRY", "GENDER",
                                                      "AGE", "RACE", "EDU",
                                                      "JOB", "MARTL"))




############### MODEL.summary

summary(data.etoh_tob_thc.dems.fit, standardized = TRUE, fit.measures = TRUE)
# > summary(data.etoh_tob_thc.dems.fit, standardized = TRUE, fit.measures = TRUE)
# lavaan 0.6-6 ended normally after 48 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         86
# 
# Number of observations                          1121
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               826.637     875.922
# Degrees of freedom                               106         106
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.989
# Shift parameter                                           40.188
# simple second-order correction                             
# 
# Model Test Baseline Model:
#   
#   Test statistic                            132431.076   62722.082
# Degrees of freedom                               136         136
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.114
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.995       0.988
# Tucker-Lewis Index (TLI)                       0.993       0.984
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.078       0.081
# 90 Percent confidence interval - lower         0.073       0.076
# 90 Percent confidence interval - upper         0.083       0.086
# P-value RMSEA <= 0.05                          0.000       0.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.102       0.102
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# THC =~                                                                
#   ATTHC3M           1.202    0.043   27.852    0.000    1.202    1.202
# ATTHCURG          1.148    0.041   28.037    0.000    1.148    1.148
# ATTHCPRB          0.921    0.026   35.958    0.000    0.921    0.921
# ATTHCEXP          0.903    0.027   33.288    0.000    0.903    0.903
# ATTHCWOR          0.429    0.038   11.150    0.000    0.429    0.429
# ATTHCTRY          0.584    0.033   17.568    0.000    0.584    0.584
# ETOH =~                                                               
#   ATALC3M           0.794    0.018   45.200    0.000    0.794    0.794
# ATALCURG          0.937    0.018   52.741    0.000    0.937    0.937
# ATALCPRB          0.938    0.023   40.575    0.000    0.938    0.938
# ATALCEXP          0.839    0.025   33.053    0.000    0.839    0.839
# ATALCWOR          0.444    0.032   13.863    0.000    0.444    0.444
# ATALCTRY          0.415    0.033   12.478    0.000    0.415    0.415
# THC_ETOH_TOB =~                                                       
#   ATTHC3M          -0.516    0.064   -8.076    0.000   -0.516   -0.516
# ATTHCURG         -0.420    0.061   -6.829    0.000   -0.420   -0.420
# ATALCEXP          0.306    0.038    7.994    0.000    0.306    0.306
# ATALCWOR          0.282    0.037    7.706    0.000    0.282    0.282
# ATALCTRY          0.267    0.039    6.873    0.000    0.267    0.267
# ATTOB3M           0.807    0.029   28.231    0.000    0.807    0.807
# ATTOBURG          0.818    0.029   28.458    0.000    0.818    0.818
# ATTOBPRB          0.852    0.026   33.412    0.000    0.852    0.852
# ATTOBWOR          0.715    0.026   27.848    0.000    0.715    0.715
# ATTOBTRY          0.779    0.025   31.180    0.000    0.779    0.779
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCWOR ~~                                                           
#   .ATALCTRY          0.400    0.028   14.125    0.000    0.400    0.581
# .ATTOB3M ~~                                                            
#   .ATTOBURG          0.313    0.042    7.395    0.000    0.313    0.922
# .ATTHCTRY ~~                                                           
#   .ATALCTRY          0.370    0.034   11.046    0.000    0.370    0.541
# .ATTHCWOR ~~                                                           
#   .ATALCWOR          0.338    0.034   10.039    0.000    0.338    0.458
# .ATTHCTRY          0.401    0.034   11.861    0.000    0.401    0.547
# THC ~~                                                                
#   ETOH              0.368    0.040    9.211    0.000    0.368    0.368
# THC_ETOH_TOB      0.574    0.042   13.631    0.000    0.574    0.574
# ETOH ~~                                                               
#   THC_ETOH_TOB      0.214    0.037    5.729    0.000    0.214    0.214
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATTHC3M           0.000                               0.000    0.000
# .ATTHCURG          0.000                               0.000    0.000
# .ATTHCPRB          0.000                               0.000    0.000
# .ATTHCEXP          0.000                               0.000    0.000
# .ATTHCWOR          0.000                               0.000    0.000
# .ATTHCTRY          0.000                               0.000    0.000
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
# THC               0.000                               0.000    0.000
# ETOH              0.000                               0.000    0.000
# THC_ETOH_TOB      0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATTHC3M|t1        0.577    0.040   14.500    0.000    0.577    0.577
# ATTHC3M|t2        0.811    0.042   19.174    0.000    0.811    0.811
# ATTHC3M|t3        1.003    0.045   22.208    0.000    1.003    1.003
# ATTHC3M|t4        1.303    0.052   25.232    0.000    1.303    1.303
# ATTHCURG|t1       0.792    0.042   18.841    0.000    0.792    0.792
# ATTHCURG|t2       0.953    0.044   21.490    0.000    0.953    0.953
# ATTHCURG|t3       1.100    0.047   23.424    0.000    1.100    1.100
# ATTHCURG|t4       1.346    0.053   25.488    0.000    1.346    1.346
# ATTHCPRB|t1       1.564    0.060   26.103    0.000    1.564    1.564
# ATTHCPRB|t2       1.719    0.066   25.875    0.000    1.719    1.719
# ATTHCPRB|t3       1.903    0.076   24.965    0.000    1.903    1.903
# ATTHCPRB|t4       2.143    0.094   22.908    0.000    2.143    2.143
# ATTHCEXP|t1       1.628    0.062   26.072    0.000    1.628    1.628
# ATTHCEXP|t2       1.851    0.073   25.288    0.000    1.851    1.851
# ATTHCEXP|t3       2.008    0.083   24.166    0.000    2.008    2.008
# ATTHCEXP|t4       2.301    0.109   21.161    0.000    2.301    2.301
# ATTHCWOR|t1       0.833    0.043   19.560    0.000    0.833    0.833
# ATTHCWOR|t2       1.672    0.064   25.999    0.000    1.672    1.672
# ATTHCTRY|t1       0.898    0.043   20.645    0.000    0.898    0.898
# ATTHCTRY|t2       1.620    0.062   26.082    0.000    1.620    1.620
# ATALC3M|t1       -0.298    0.038   -7.840    0.000   -0.298   -0.298
# ATALC3M|t2        0.152    0.038    4.029    0.000    0.152    0.152
# ATALC3M|t3        0.653    0.041   16.121    0.000    0.653    0.653
# ATALC3M|t4        1.391    0.054   25.711    0.000    1.391    1.391
# ATALCURG|t1       0.430    0.039   11.099    0.000    0.430    0.430
# ATALCURG|t2       0.698    0.041   17.037    0.000    0.698    0.698
# ATALCURG|t3       0.949    0.044   21.438    0.000    0.949    0.949
# ATALCURG|t4       1.440    0.056   25.893    0.000    1.440    1.440
# ATALCPRB|t1       1.282    0.051   25.093    0.000    1.282    1.282
# ATALCPRB|t2       1.434    0.055   25.873    0.000    1.434    1.434
# ATALCPRB|t3       1.719    0.066   25.875    0.000    1.719    1.719
# ATALCPRB|t4       2.008    0.083   24.166    0.000    2.008    2.008
# ATALCEXP|t1       1.362    0.053   25.576    0.000    1.362    1.362
# ATALCEXP|t2       1.645    0.063   26.049    0.000    1.645    1.645
# ATALCEXP|t3       1.851    0.073   25.288    0.000    1.851    1.851
# ATALCEXP|t4       2.166    0.096   22.672    0.000    2.166    2.166
# ATALCWOR|t1       0.525    0.039   13.333    0.000    0.525    0.525
# ATALCWOR|t2       1.287    0.051   25.129    0.000    1.287    1.287
# ATALCTRY|t1       0.684    0.041   16.752    0.000    0.684    0.684
# ATALCTRY|t2       1.368    0.053   25.604    0.000    1.368    1.368
# ATTOB3M|t1       -0.280    0.038   -7.365    0.000   -0.280   -0.280
# ATTOB3M|t2       -0.102    0.038   -2.716    0.007   -0.102   -0.102
# ATTOB3M|t3       -0.003    0.037   -0.090    0.929   -0.003   -0.003
# ATTOB3M|t4        0.140    0.038    3.731    0.000    0.140    0.140
# ATTOBURG|t1      -0.064    0.037   -1.702    0.089   -0.064   -0.064
# ATTOBURG|t2       0.039    0.037    1.045    0.296    0.039    0.039
# ATTOBURG|t3       0.095    0.038    2.537    0.011    0.095    0.095
# ATTOBURG|t4       0.264    0.038    6.948    0.000    0.264    0.264
# ATTOBPRB|t1       0.865    0.043   20.106    0.000    0.865    0.865
# ATTOBPRB|t2       1.033    0.046   22.607    0.000    1.033    1.033
# ATTOBPRB|t3       1.205    0.049   24.479    0.000    1.205    1.205
# ATTOBPRB|t4       1.346    0.053   25.488    0.000    1.346    1.346
# ATTOBWOR|t1      -0.021    0.037   -0.567    0.571   -0.021   -0.021
# ATTOBWOR|t2       0.607    0.040   15.139    0.000    0.607    0.607
# ATTOBTRY|t1      -0.131    0.038   -3.492    0.000   -0.131   -0.131
# ATTOBTRY|t2       0.667    0.041   16.408    0.000    0.667    0.667
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# THC               1.000                               1.000    1.000
# THC_ETOH_TOB      1.000                               1.000    1.000
# .ATTHC3M           0.001                               0.001    0.001
# .ATTHCURG          0.058                               0.058    0.058
# .ATTHCPRB          0.152                               0.152    0.152
# .ATTHCEXP          0.185                               0.185    0.185
# .ATTHCWOR          0.816                               0.816    0.816
# .ATTHCTRY          0.659                               0.659    0.659
# .ATALC3M           0.370                               0.370    0.370
# .ATALCURG          0.122                               0.122    0.122
# .ATALCPRB          0.121                               0.121    0.121
# .ATALCEXP          0.092                               0.092    0.092
# .ATALCWOR          0.670                               0.670    0.670
# .ATALCTRY          0.709                               0.709    0.709
# .ATTOB3M           0.348                               0.348    0.348
# .ATTOBURG          0.330                               0.330    0.330
# .ATTOBPRB          0.274                               0.274    0.274
# .ATTOBWOR          0.488                               0.488    0.488
# .ATTOBTRY          0.393                               0.393    0.393
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATTHC3M           1.000                               1.000    1.000
# ATTHCURG          1.000                               1.000    1.000
# ATTHCPRB          1.000                               1.000    1.000
# ATTHCEXP          1.000                               1.000    1.000
# ATTHCWOR          1.000                               1.000    1.000
# ATTHCTRY          1.000                               1.000    1.000
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


##################### ##################### ##################### ##################### 
##################### ##################### ##################### ##################### 


##################### INVARIANCE TESTS

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.Etoh_Tob_Thc.model.RACE.fit <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                               data.etoh_tob_thc.dems,
                                               std.lv=TRUE,
                                               estimator = "WLSMV",
                                               group = "RACE")

# Error in lav_samplestats_icov(COV = cov[[g]], ridge = ridge, x.idx = x.idx[[g]],  : 
#                                 lavaan ERROR: sample covariance matrix is not positive-definite
#                               In addition: Warning messages:
# 1: In lav_data_full(data = data, group = group, cluster = cluster,  :
#    lavaan WARNING: small number of observations (nobs < nvar) in group 6
#    nobs = 5 nvar = 17
#2: In lav_data_full(data = data, group = group, cluster = cluster,  :
#   lavaan WARNING: small number of observations (nobs < nvar) in group 7
#   nobs = 2 nvar = 17
#3: In lav_samplestats_from_data(lavdata = lavdata, missing = lavoptions$missing,  :
#                                                                                                           lavaan WARNING: number of observations (87) too small to compute Gamma in group: 1
#                                                                                                         
#                                                                                                         4: In lav_samplestats_from_data(lavdata = lavdata, missing = lavoptions$missing,  :
#                                                                                                                                           lavaan WARNING: number of observations (83) too small to compute Gamma in group: 4
#                                                                                                                                         
#                                                                                                                                         >




##################  ##################  Adjustment to RACE  ################## ##################
################## ################## ################## ################## ##################

table(data.etoh_tob_thc.dems$RACE)
# > table(data.etoh_tob_thc.dems$RACE)
# 
# 1   2   3   4   5   6   7 
# 83 312 605   5  27  87   2 
# >


### Filter the data for RACE to only include Latinx, Whites, AA, Milt 
### Multiple arguments are equivalent to and

### Filter out Native Americans/Indigenious for RACE = RACE != 4
data.etoh_tob_thc.dems.RACE <- dplyr::filter(data.etoh_tob_thc.dems, RACE != 4) #Extract rows that meet logical criteria.

### Filter out Asians for RACE = RACE != 5

data.etoh_tob_thc.dems.RACE.02 <- dplyr::filter(data.etoh_tob_thc.dems.RACE, RACE != 5) #Extract rows that meet logical criteria.


### Filter out not specified for RACE = RACE != 7

data.etoh_tob_thc.dems.RACE.03 <- dplyr::filter(data.etoh_tob_thc.dems.RACE.02, RACE != 7) #Extract rows that meet logical criteria.

### Filter out Latinx for RACE = RACE != 1

data.etoh_tob_thc.dems.RACE.04 <- dplyr::filter(data.etoh_tob_thc.dems.RACE.03, RACE != 1) #Extract rows that meet logical criteria.

### Filter out multi-racial for RACE = RACE != 6

data.etoh_tob_thc.dems.RACE.05 <- dplyr::filter(data.etoh_tob_thc.dems.RACE.04, RACE != 6) #Extract rows that meet logical criteria.


### RACE is Whites(2) & AAs(3)
table(data.etoh_tob_thc.dems.RACE.05$RACE)
# > table(data.etoh_tob_thc.dems.RACE.05$RACE)
# 
# 2   3 
# 312 605 
# >

##################### INVARIANCE TESTS

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.Etoh_Tob_Thc.model.RACE.fit <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                               data.etoh_tob_thc.dems.RACE.05,
                                               std.lv=TRUE,
                                               estimator = "WLSMV",
                                               group = "RACE")

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_Thc.model.RACE.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob_Thc.model.RACE.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 131 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                        128
# 
# Number of observations per group:                   
#   3                                              605
# 2                                              312
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               505.964     652.873
# Degrees of freedom                               212         212
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.917
# Shift parameter for each group:                                 
#   3                                                     66.692
# 2                                                     34.393
# simple second-order correction                             
# Test statistic for each group:
#   3                                          356.562     455.547
# 2                                          149.402     197.327
# 
# Model Test Baseline Model:
#   
#   Test statistic                              8512.015    3042.713
# Degrees of freedom                               272         272
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.974
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.964       0.841
# Tucker-Lewis Index (TLI)                       0.954       0.796
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.055       0.067
# 90 Percent confidence interval - lower         0.049       0.062
# 90 Percent confidence interval - upper         0.061       0.073
# P-value RMSEA <= 0.05                          0.087       0.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.078       0.078
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# 
# Group 1 [3]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# THC =~                                                                
#   ATTHC3M           1.766    0.095   18.596    0.000    1.766    0.946
# ATTHCURG          1.966    0.114   17.265    0.000    1.966    0.999
# ATTHCPRB          0.709    0.120    5.892    0.000    0.709    0.567
# ATTHCEXP          0.661    0.128    5.166    0.000    0.661    0.503
# ATTHCWOR          0.352    0.094    3.729    0.000    0.352    0.244
# ATTHCTRY          0.478    0.097    4.907    0.000    0.478    0.334
# ETOH =~                                                               
#   ATALC3M           1.118    0.072   15.515    0.000    1.118    0.592
# ATALCURG          1.750    0.072   24.331    0.000    1.750    0.811
# ATALCPRB          1.124    0.130    8.662    0.000    1.124    0.717
# ATALCEXP          1.111    0.137    8.080    0.000    1.111    0.646
# ATALCWOR          0.718    0.119    6.046    0.000    0.718    0.383
# ATALCTRY          0.534    0.115    4.634    0.000    0.534    0.316
# THC_ETOH_TOB =~                                                       
#   ATTHC3M          -0.413    0.130   -3.176    0.001   -0.413   -0.221
# ATTHCURG         -0.384    0.154   -2.493    0.013   -0.384   -0.195
# ATALCEXP          0.201    0.062    3.257    0.001    0.201    0.117
# ATALCWOR          0.413    0.079    5.257    0.000    0.413    0.220
# ATALCTRY          0.322    0.071    4.534    0.000    0.322    0.191
# ATTOB3M           1.907    0.103   18.462    0.000    1.907    0.695
# ATTOBURG          2.095    0.107   19.574    0.000    2.095    0.735
# ATTOBPRB          1.402    0.109   12.867    0.000    1.402    0.587
# ATTOBWOR          1.711    0.101   16.996    0.000    1.711    0.678
# ATTOBTRY          1.722    0.096   17.903    0.000    1.722    0.714
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCWOR ~~                                                           
#   .ATALCTRY          1.309    0.154    8.485    0.000    1.309    0.508
# .ATTOB3M ~~                                                            
#   .ATTOBURG          2.966    0.383    7.751    0.000    2.966    0.778
# .ATTHCTRY ~~                                                           
#   .ATALCTRY          0.610    0.125    4.865    0.000    0.610    0.291
# .ATTHCWOR ~~                                                           
#   .ATALCWOR          0.547    0.121    4.536    0.000    0.547    0.235
# .ATTHCTRY          0.746    0.126    5.913    0.000    0.746    0.395
# THC ~~                                                                
#   ETOH              0.261    0.058    4.462    0.000    0.261    0.261
# THC_ETOH_TOB      0.343    0.075    4.578    0.000    0.343    0.343
# ETOH ~~                                                               
#   THC_ETOH_TOB      0.132    0.053    2.488    0.013    0.132    0.132
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATTHC3M           0.998    0.076   13.150    0.000    0.998    0.535
# .ATTHCURG          0.936    0.080   11.697    0.000    0.936    0.476
# .ATTHCPRB          0.284    0.051    5.589    0.000    0.284    0.227
# .ATTHCEXP          0.284    0.053    5.315    0.000    0.284    0.216
# .ATTHCWOR          0.630    0.059   10.709    0.000    0.630    0.435
# .ATTHCTRY          0.605    0.058   10.397    0.000    0.605    0.423
# .ATALC3M           2.084    0.077   27.149    0.000    2.084    1.104
# .ATALCURG          1.460    0.088   16.644    0.000    1.460    0.677
# .ATALCPRB          0.471    0.064    7.393    0.000    0.471    0.301
# .ATALCEXP          0.506    0.070    7.238    0.000    0.506    0.294
# .ATALCWOR          1.051    0.076   13.793    0.000    1.051    0.561
# .ATALCTRY          0.813    0.069   11.837    0.000    0.813    0.481
# .ATTOB3M           3.190    0.112   28.577    0.000    3.190    1.162
# .ATTOBURG          2.932    0.116   25.304    0.000    2.932    1.029
# .ATTOBPRB          1.155    0.097   11.899    0.000    1.155    0.484
# .ATTOBWOR          2.182    0.103   21.245    0.000    2.182    0.864
# .ATTOBTRY          2.321    0.098   23.673    0.000    2.321    0.963
# THC               0.000                               0.000    0.000
# ETOH              0.000                               0.000    0.000
# THC_ETOH_TOB      0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# THC               1.000                               1.000    1.000
# THC_ETOH_TOB      1.000                               1.000    1.000
# .ATTHC3M           0.695    0.191    3.640    0.000    0.695    0.199
# .ATTHCURG          0.374    0.194    1.928    0.054    0.374    0.097
# .ATTHCPRB          1.062    0.138    7.687    0.000    1.062    0.679
# .ATTHCEXP          1.293    0.182    7.124    0.000    1.293    0.747
# .ATTHCWOR          1.967    0.183   10.748    0.000    1.967    0.941
# .ATTHCTRY          1.819    0.168   10.844    0.000    1.819    0.889
# .ATALC3M           2.315    0.163   14.220    0.000    2.315    0.650
# .ATALCURG          1.588    0.243    6.532    0.000    1.588    0.342
# .ATALCPRB          1.192    0.167    7.148    0.000    1.192    0.485
# .ATALCEXP          1.619    0.171    9.479    0.000    1.619    0.548
# .ATALCWOR          2.748    0.185   14.852    0.000    2.748    0.782
# .ATALCTRY          2.420    0.178   13.605    0.000    2.420    0.848
# .ATTOB3M           3.899    0.369   10.576    0.000    3.899    0.517
# .ATTOBURG          3.729    0.434    8.597    0.000    3.729    0.459
# .ATTOBPRB          3.734    0.258   14.454    0.000    3.734    0.655
# .ATTOBWOR          3.450    0.285   12.119    0.000    3.450    0.541
# .ATTOBTRY          2.845    0.271   10.508    0.000    2.845    0.490
# 
# 
# Group 2 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# THC =~                                                                
#   ATTHC3M           1.960    0.151   13.001    0.000    1.960    0.897
# ATTHCURG          2.104    0.137   15.402    0.000    2.104    0.957
# ATTHCPRB          0.894    0.176    5.078    0.000    0.894    0.616
# ATTHCEXP          0.916    0.196    4.668    0.000    0.916    0.574
# ATTHCWOR          0.868    0.161    5.392    0.000    0.868    0.490
# ATTHCTRY          1.096    0.164    6.695    0.000    1.096    0.598
# ETOH =~                                                               
#   ATALC3M           1.419    0.099   14.383    0.000    1.419    0.710
# ATALCURG          1.992    0.106   18.874    0.000    1.992    0.869
# ATALCPRB          1.185    0.173    6.855    0.000    1.185    0.674
# ATALCEXP          0.928    0.163    5.687    0.000    0.928    0.535
# ATALCWOR          0.853    0.154    5.552    0.000    0.853    0.410
# ATALCTRY          0.846    0.148    5.726    0.000    0.846    0.403
# THC_ETOH_TOB =~                                                       
#   ATTHC3M          -0.428    0.167   -2.560    0.010   -0.428   -0.196
# ATTHCURG         -0.410    0.158   -2.593    0.010   -0.410   -0.187
# ATALCEXP          0.260    0.086    3.033    0.002    0.260    0.150
# ATALCWOR          0.232    0.126    1.844    0.065    0.232    0.112
# ATALCTRY          0.270    0.120    2.254    0.024    0.270    0.128
# ATTOB3M           2.040    0.119   17.111    0.000    2.040    0.744
# ATTOBURG          2.157    0.119   18.146    0.000    2.157    0.760
# ATTOBPRB          1.403    0.156    9.000    0.000    1.403    0.594
# ATTOBWOR          1.595    0.141   11.314    0.000    1.595    0.614
# ATTOBTRY          1.875    0.130   14.410    0.000    1.875    0.737
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCWOR ~~                                                           
#   .ATALCTRY          0.815    0.267    3.049    0.002    0.815    0.234
# .ATTOB3M ~~                                                            
#   .ATTOBURG          2.629    0.455    5.779    0.000    2.629    0.778
# .ATTHCTRY ~~                                                           
#   .ATALCTRY          1.051    0.239    4.398    0.000    1.051    0.381
# .ATTHCWOR ~~                                                           
#   .ATALCWOR          0.877    0.223    3.939    0.000    0.877    0.306
# .ATTHCTRY          0.649    0.234    2.769    0.006    0.649    0.286
# THC ~~                                                                
#   ETOH              0.301    0.074    4.060    0.000    0.301    0.301
# THC_ETOH_TOB      0.429    0.069    6.207    0.000    0.429    0.429
# ETOH ~~                                                               
#   THC_ETOH_TOB      0.225    0.070    3.227    0.001    0.225    0.225
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATTHC3M           1.359    0.124   10.988    0.000    1.359    0.622
# .ATTHCURG          1.202    0.124    9.665    0.000    1.202    0.547
# .ATTHCPRB          0.391    0.082    4.757    0.000    0.391    0.269
# .ATTHCEXP          0.401    0.090    4.434    0.000    0.401    0.251
# .ATTHCWOR          0.923    0.100    9.208    0.000    0.923    0.521
# .ATTHCTRY          0.865    0.104    8.346    0.000    0.865    0.472
# .ATALC3M           2.160    0.113   19.089    0.000    2.160    1.080
# .ATALCURG          1.657    0.130   12.775    0.000    1.657    0.723
# .ATALCPRB          0.603    0.100    6.052    0.000    0.603    0.342
# .ATALCEXP          0.503    0.098    5.121    0.000    0.503    0.290
# .ATALCWOR          1.375    0.118   11.684    0.000    1.375    0.661
# .ATALCTRY          1.288    0.119   10.841    0.000    1.288    0.613
# .ATTOB3M           3.221    0.155   20.763    0.000    3.221    1.175
# .ATTOBURG          3.045    0.161   18.959    0.000    3.045    1.073
# .ATTOBPRB          1.131    0.134    8.459    0.000    1.131    0.479
# .ATTOBWOR          2.606    0.147   17.711    0.000    2.606    1.002
# .ATTOBTRY          2.606    0.144   18.103    0.000    2.606    1.024
# THC               0.000                               0.000    0.000
# ETOH              0.000                               0.000    0.000
# THC_ETOH_TOB      0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# THC               1.000                               1.000    1.000
# THC_ETOH_TOB      1.000                               1.000    1.000
# .ATTHC3M           1.470    0.318    4.630    0.000    1.470    0.308
# .ATTHCURG          0.974    0.232    4.191    0.000    0.974    0.202
# .ATTHCPRB          1.310    0.197    6.637    0.000    1.310    0.621
# .ATTHCEXP          1.710    0.295    5.792    0.000    1.710    0.671
# .ATTHCWOR          2.385    0.260    9.158    0.000    2.385    0.760
# .ATTHCTRY          2.156    0.261    8.270    0.000    2.156    0.642
# .ATALC3M           1.985    0.213    9.325    0.000    1.985    0.496
# .ATALCURG          1.287    0.377    3.414    0.001    1.287    0.245
# .ATALCPRB          1.691    0.252    6.705    0.000    1.691    0.546
# .ATALCEXP          1.978    0.271    7.311    0.000    1.978    0.656
# .ATALCWOR          3.454    0.282   12.266    0.000    3.454    0.799
# .ATALCTRY          3.521    0.279   12.617    0.000    3.521    0.798
# .ATTOB3M           3.355    0.452    7.431    0.000    3.355    0.446
# .ATTOBURG          3.404    0.489    6.961    0.000    3.404    0.423
# .ATTOBPRB          3.618    0.324   11.160    0.000    3.618    0.648
# .ATTOBWOR          4.215    0.405   10.414    0.000    4.215    0.623
# .ATTOBTRY          2.956    0.431    6.851    0.000    2.956    0.457
# 
# >

##### Note: The fit of the model is poor and I am going to leave it alone. 

############### ############### ############### ############### ############### ############### 
############### ############### STOP STOP STOP  ############### ############### ############### 
############### ############### ############### ############### ############### ############### 

############### Metric invariance

AST.Etoh_Tob.model.RACE.METRIC.fit <- lavaan::cfa(AST.Etoh_Tob.model, data.etoh_tob.dems.RACE.05, std.lv=TRUE, estimator = "WLSMV", group = "RACE", group.equal=c("loadings"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob.model.RACE.METRIC.fit, fit.measures= TRUE, standardized=TRUE)


################# Chi-square difference test between Configural invariance and metric invariance. 

anova(AST.Etoh_Tob.model.RACE.fit, AST.Etoh_Tob.model.RACE.METRIC.fit)



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




############################ Test difference

anova(AST.Etoh_Tob.model.RACE.METRIC.fit, AST.Etoh_Tob.model.RACE.SCALAR.fit)


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




anova(AST.Etoh_Tob.model.RACE.SCALAR.fit, AST.Etoh_Tob.model.RACE.MEANS.fit)



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


anova(AST.Etoh_Tob.model.RACE.SCALAR.fit, AST.Etoh_Tob.model.RACE.STRICT.fit)


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


####################################################################################
####################################################################################
####################################################################################

##################### INVARIANCE TESTS_GENDER ######################################

table(data.etoh_tob_thc.dems$GENDER)
# > table(data.etoh_tob_thc.dems$GENDER)
# 
# 1   2  97  98 
# 488 631   1   1 
# >

### Filter out 98 for GENDER
data.etoh_tob_thc.dems.GENDER <- dplyr::filter(data.etoh_tob_thc.dems, GENDER != 98) #Extract rows that meet logical criteria.

### Filter out 97 for GENDER

data.etoh_tob_thc.dems.GENDER.02 <- dplyr::filter(data.etoh_tob_thc.dems.GENDER, GENDER != 97) #Extract rows that meet logical criteria.

table(data.etoh_tob_thc.dems.GENDER.02$GENDER)
# > table(data.etoh_tob_thc.dems.GENDER.02$GENDER)
# 
# 1   2 
# 488 631 
# >

##### Descriptive Stats
psych::describe(data.etoh_tob_thc.dems.GENDER.02)
# > psych::describe(data.etoh_tob_thc.dems.GENDER.02)
# vars    n  mean    sd median trimmed   mad min max range  skew kurtosis   se
# ATTHC3M     1 1119  1.12  1.98      0    0.66  0.00   0   6     6  1.54     0.90 0.06
# ATTHCURG    2 1119  1.03  2.06      0    0.56  0.00   0   6     6  1.62     0.88 0.06
# ATTHCPRB    3 1119  0.32  1.32      0    0.00  0.00   0   7     7  4.02    14.87 0.04
# ATTHCEXP    4 1119  0.32  1.40      0    0.00  0.00   0   8     8  4.33    17.46 0.04
# ATTHCWOR    5 1119  0.75  1.59      0    0.38  0.00   0   6     6  2.05     3.24 0.05
# ATTHCTRY    6 1119  0.71  1.61      0    0.32  0.00   0   6     6  2.20     3.79 0.05
# ATALC3M     7 1119  2.09  1.93      2    1.91  2.97   0   6     6  0.38    -0.91 0.06
# ATALCURG    8 1119  1.49  2.20      0    1.15  0.00   0   6     6  0.96    -0.79 0.07
# ATALCPRB    9 1119  0.54  1.66      0    0.00  0.00   0   7     7  2.87     6.70 0.05
# ATALCEXP   10 1119  0.53  1.76      0    0.00  0.00   0   8     8  3.14     8.32 0.05
# ATALCWOR   11 1119  1.20  1.99      0    0.75  0.00   0   6     6  1.40     0.61 0.06
# ATALCTRY   12 1119  1.00  1.88      0    0.56  0.00   0   6     6  1.69     1.53 0.06
# ATTOB3M    13 1119  3.15  2.76      4    3.19  2.97   0   6     6 -0.09    -1.83 0.08
# ATTOBURG   14 1119  2.92  2.85      3    2.90  4.45   0   6     6  0.02    -1.92 0.09
# ATTOBPRB   15 1119  1.13  2.37      0    0.56  0.00   0   7     7  1.73     1.21 0.07
# ATTOBWOR   16 1119  2.34  2.54      3    2.18  4.45   0   6     6  0.43    -1.47 0.08
# ATTOBTRY   17 1119  2.42  2.44      3    2.27  4.45   0   6     6  0.37    -1.40 0.07
# GENDER     18 1119  1.56  0.50      2    1.58  0.00   1   2     1 -0.26    -1.94 0.01
# AGE        19 1119 45.97 14.81     48   45.95 16.31  18  94    76 -0.01    -0.62 0.44
# RACE       20 1119  2.87  1.18      3    2.70  0.00   1   7     6  1.28     1.98 0.04
# EDU        21 1119 14.23  2.98     15   14.39  2.97   0  21    21 -0.48     0.52 0.09
# JOB        22 1119  5.00 13.45      3    3.02  2.97   1  99    98  6.70    43.87 0.40
# MARTL      23 1119  3.63  1.68      5    3.73  1.48   1   6     5 -0.52    -1.27 0.05
# >


##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.Etoh_Tob_Thc.model.GENDER.fit <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                                 data.etoh_tob_thc.dems.GENDER.02,
                                                 std.lv=TRUE,
                                                 estimator = "WLSMV",
                                                 group = "GENDER")

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_Thc.model.GENDER.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.Etoh_Tob_Thc.model.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 124 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                        128
# 
# Number of observations per group:                   
#   2                                              631
# 1                                              488
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                               550.327     720.301
# Degrees of freedom                               212         212
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.885
# Shift parameter for each group:                                 
#   2                                                     55.335
# 1                                                     42.795
# simple second-order correction                             
# Test statistic for each group:
#   2                                          283.356     375.682
# 1                                          266.971     344.619
# 
# Model Test Baseline Model:
#   
#   Test statistic                             10634.954    3843.064
# Degrees of freedom                               272         272
# P-value                                        0.000       0.000
# Scaling correction factor                                  2.902
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.967       0.858
# Tucker-Lewis Index (TLI)                       0.958       0.817
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.053       0.066
# 90 Percent confidence interval - lower         0.048       0.060
# 90 Percent confidence interval - upper         0.059       0.071
# P-value RMSEA <= 0.05                          0.146       0.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.074       0.074
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
# THC =~                                                                
#   ATTHC3M           1.785    0.105   16.985    0.000    1.785    0.938
# ATTHCURG          1.902    0.106   17.959    0.000    1.902    0.942
# ATTHCPRB          0.855    0.130    6.575    0.000    0.855    0.612
# ATTHCEXP          0.870    0.140    6.204    0.000    0.870    0.602
# ATTHCWOR          0.553    0.110    5.044    0.000    0.553    0.349
# ATTHCTRY          0.799    0.118    6.758    0.000    0.799    0.477
# ETOH =~                                                               
#   ATALC3M           1.223    0.075   16.298    0.000    1.223    0.627
# ATALCURG          1.861    0.075   24.837    0.000    1.861    0.834
# ATALCPRB          1.222    0.127    9.592    0.000    1.222    0.728
# ATALCEXP          1.095    0.133    8.222    0.000    1.095    0.636
# ATALCWOR          0.792    0.117    6.776    0.000    0.792    0.400
# ATALCTRY          0.699    0.114    6.129    0.000    0.699    0.377
# THC_ETOH_TOB =~                                                       
#   ATTHC3M          -0.337    0.119   -2.820    0.005   -0.337   -0.177
# ATTHCURG         -0.265    0.127   -2.085    0.037   -0.265   -0.131
# ATALCEXP          0.204    0.062    3.283    0.001    0.204    0.118
# ATALCWOR          0.356    0.086    4.143    0.000    0.356    0.180
# ATALCTRY          0.310    0.078    3.970    0.000    0.310    0.168
# ATTOB3M           1.932    0.088   21.899    0.000    1.932    0.700
# ATTOBURG          2.054    0.098   20.917    0.000    2.054    0.718
# ATTOBPRB          1.471    0.110   13.407    0.000    1.471    0.625
# ATTOBWOR          1.596    0.105   15.209    0.000    1.596    0.633
# ATTOBTRY          1.838    0.092   20.011    0.000    1.838    0.748
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCWOR ~~                                                           
#   .ATALCTRY          1.127    0.170    6.646    0.000    1.127    0.388
# .ATTOB3M ~~                                                            
#   .ATTOBURG          3.110    0.348    8.940    0.000    3.110    0.794
# .ATTHCTRY ~~                                                           
#   .ATALCTRY          0.899    0.151    5.938    0.000    0.899    0.368
# .ATTHCWOR ~~                                                           
#   .ATALCWOR          0.797    0.146    5.458    0.000    0.797    0.307
# .ATTHCTRY          0.711    0.146    4.884    0.000    0.711    0.325
# THC ~~                                                                
#   ETOH              0.345    0.058    5.911    0.000    0.345    0.345
# THC_ETOH_TOB      0.414    0.059    6.967    0.000    0.414    0.414
# ETOH ~~                                                               
#   THC_ETOH_TOB      0.197    0.052    3.799    0.000    0.197    0.197
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATTHC3M           1.063    0.076   14.035    0.000    1.063    0.559
# .ATTHCURG          0.992    0.080   12.335    0.000    0.992    0.491
# .ATTHCPRB          0.355    0.056    6.375    0.000    0.355    0.254
# .ATTHCEXP          0.338    0.057    5.873    0.000    0.338    0.234
# .ATTHCWOR          0.727    0.063   11.526    0.000    0.727    0.459
# .ATTHCTRY          0.770    0.067   11.549    0.000    0.770    0.460
# .ATALC3M           2.095    0.078   26.993    0.000    2.095    1.075
# .ATALCURG          1.544    0.089   17.370    0.000    1.544    0.692
# .ATALCPRB          0.547    0.067    8.183    0.000    0.547    0.326
# .ATALCEXP          0.501    0.069    7.298    0.000    0.501    0.291
# .ATALCWOR          1.174    0.079   14.900    0.000    1.174    0.593
# .ATALCTRY          0.970    0.074   13.155    0.000    0.970    0.524
# .ATTOB3M           3.105    0.110   28.271    0.000    3.105    1.126
# .ATTOBURG          2.897    0.114   25.444    0.000    2.897    1.013
# .ATTOBPRB          1.124    0.094   11.997    0.000    1.124    0.478
# .ATTOBWOR          2.344    0.100   23.328    0.000    2.344    0.929
# .ATTOBTRY          2.406    0.098   24.603    0.000    2.406    0.980
# THC               0.000                               0.000    0.000
# ETOH              0.000                               0.000    0.000
# THC_ETOH_TOB      0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# THC               1.000                               1.000    1.000
# THC_ETOH_TOB      1.000                               1.000    1.000
# .ATTHC3M           0.819    0.197    4.150    0.000    0.819    0.226
# .ATTHCURG          0.810    0.191    4.240    0.000    0.810    0.199
# .ATTHCPRB          1.224    0.139    8.781    0.000    1.224    0.626
# .ATTHCEXP          1.328    0.157    8.461    0.000    1.328    0.637
# .ATTHCWOR          2.207    0.183   12.039    0.000    2.207    0.878
# .ATTHCTRY          2.168    0.167   12.974    0.000    2.168    0.773
# .ATALC3M           2.305    0.162   14.210    0.000    2.305    0.607
# .ATALCURG          1.519    0.264    5.760    0.000    1.519    0.305
# .ATALCPRB          1.323    0.185    7.157    0.000    1.323    0.470
# .ATALCEXP          1.641    0.166    9.908    0.000    1.641    0.552
# .ATALCWOR          3.054    0.203   15.065    0.000    3.054    0.779
# .ATALCTRY          2.759    0.188   14.642    0.000    2.759    0.805
# .ATTOB3M           3.876    0.324   11.950    0.000    3.876    0.509
# .ATTOBURG          3.957    0.396    9.985    0.000    3.957    0.484
# .ATTOBPRB          3.370    0.242   13.917    0.000    3.370    0.609
# .ATTOBWOR          3.820    0.296   12.900    0.000    3.820    0.600
# .ATTOBTRY          2.656    0.285    9.333    0.000    2.656    0.440
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# THC =~                                                                
#   ATTHC3M           2.002    0.112   17.929    0.000    2.002    0.962
# ATTHCURG          2.083    0.123   16.906    0.000    2.083    0.989
# ATTHCPRB          0.656    0.118    5.567    0.000    0.656    0.538
# ATTHCEXP          0.637    0.131    4.880    0.000    0.637    0.475
# ATTHCWOR          0.492    0.115    4.287    0.000    0.492    0.307
# ATTHCTRY          0.675    0.116    5.829    0.000    0.675    0.447
# ETOH =~                                                               
#   ATALC3M           1.296    0.080   16.158    0.000    1.296    0.680
# ATALCURG          1.774    0.096   18.406    0.000    1.774    0.819
# ATALCPRB          1.126    0.142    7.914    0.000    1.126    0.687
# ATALCEXP          1.167    0.141    8.253    0.000    1.167    0.649
# ATALCWOR          0.787    0.128    6.170    0.000    0.787    0.394
# ATALCTRY          0.680    0.129    5.294    0.000    0.680    0.354
# THC_ETOH_TOB =~                                                       
#   ATTHC3M          -0.580    0.144   -4.039    0.000   -0.580   -0.279
# ATTHCURG         -0.561    0.149   -3.774    0.000   -0.561   -0.266
# ATALCEXP          0.151    0.067    2.239    0.025    0.151    0.084
# ATALCWOR          0.436    0.092    4.745    0.000    0.436    0.218
# ATALCTRY          0.327    0.089    3.672    0.000    0.327    0.170
# ATTOB3M           2.046    0.115   17.780    0.000    2.046    0.741
# ATTOBURG          2.130    0.124   17.203    0.000    2.130    0.750
# ATTOBPRB          1.384    0.125   11.050    0.000    1.384    0.579
# ATTOBWOR          1.605    0.116   13.822    0.000    1.605    0.627
# ATTOBTRY          1.678    0.113   14.783    0.000    1.678    0.691
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCWOR ~~                                                           
#   .ATALCTRY          1.272    0.190    6.706    0.000    1.272    0.418
# .ATTOB3M ~~                                                            
#   .ATTOBURG          2.680    0.430    6.238    0.000    2.680    0.769
# .ATTHCTRY ~~                                                           
#   .ATALCTRY          0.631    0.160    3.938    0.000    0.631    0.268
# .ATTHCWOR ~~                                                           
#   .ATALCWOR          0.560    0.153    3.671    0.000    0.560    0.210
# .ATTHCTRY          0.729    0.145    5.032    0.000    0.729    0.353
# THC ~~                                                                
#   ETOH              0.263    0.058    4.504    0.000    0.263    0.263
# THC_ETOH_TOB      0.389    0.068    5.743    0.000    0.389    0.389
# ETOH ~~                                                               
#   THC_ETOH_TOB      0.208    0.057    3.654    0.000    0.208    0.208
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATTHC3M           1.193    0.094   12.665    0.000    1.193    0.573
# .ATTHCURG          1.088    0.095   11.417    0.000    1.088    0.517
# .ATTHCPRB          0.283    0.055    5.128    0.000    0.283    0.232
# .ATTHCEXP          0.293    0.061    4.830    0.000    0.293    0.219
# .ATTHCWOR          0.775    0.073   10.660    0.000    0.775    0.482
# .ATTHCTRY          0.633    0.068    9.269    0.000    0.633    0.420
# .ATALC3M           2.092    0.086   24.254    0.000    2.092    1.098
# .ATALCURG          1.426    0.098   14.540    0.000    1.426    0.658
# .ATALCPRB          0.535    0.074    7.212    0.000    0.535    0.326
# .ATALCEXP          0.570    0.081    6.997    0.000    0.570    0.317
# .ATALCWOR          1.230    0.090   13.595    0.000    1.230    0.615
# .ATALCTRY          1.039    0.087   11.925    0.000    1.039    0.540
# .ATTOB3M           3.211    0.125   25.678    0.000    3.211    1.162
# .ATTOBURG          2.947    0.129   22.928    0.000    2.947    1.038
# .ATTOBPRB          1.139    0.108   10.527    0.000    1.139    0.476
# .ATTOBWOR          2.336    0.116   20.176    0.000    2.336    0.913
# .ATTOBTRY          2.428    0.110   22.086    0.000    2.428    1.000
# THC               0.000                               0.000    0.000
# ETOH              0.000                               0.000    0.000
# THC_ETOH_TOB      0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH              1.000                               1.000    1.000
# THC               1.000                               1.000    1.000
# THC_ETOH_TOB      1.000                               1.000    1.000
# .ATTHC3M           0.888    0.241    3.680    0.000    0.888    0.205
# .ATTHCURG          0.691    0.234    2.958    0.003    0.691    0.156
# .ATTHCPRB          1.054    0.165    6.405    0.000    1.054    0.710
# .ATTHCEXP          1.391    0.240    5.783    0.000    1.391    0.774
# .ATTHCWOR          2.335    0.208   11.244    0.000    2.335    0.906
# .ATTHCTRY          1.823    0.182   10.043    0.000    1.823    0.800
# .ATALC3M           1.953    0.173   11.263    0.000    1.953    0.538
# .ATALCURG          1.550    0.290    5.347    0.000    1.550    0.330
# .ATALCPRB          1.417    0.186    7.614    0.000    1.417    0.528
# .ATALCEXP          1.778    0.192    9.273    0.000    1.778    0.549
# .ATALCWOR          3.040    0.208   14.639    0.000    3.040    0.762
# .ATALCTRY          3.042    0.211   14.431    0.000    3.042    0.821
# .ATTOB3M           3.446    0.440    7.830    0.000    3.446    0.451
# .ATTOBURG          3.528    0.511    6.909    0.000    3.528    0.438
# .ATTOBPRB          3.803    0.277   13.747    0.000    3.803    0.665
# .ATTOBWOR          3.969    0.318   12.475    0.000    3.969    0.606
# .ATTOBTRY          3.086    0.322    9.600    0.000    3.086    0.523
# 
# >

############### ############### ############### ############### ############### ############### 
############### ############### STOP STOP STOP  ############### ############### ############### 
############### ############### ############### ############### ############### ############### 


############### Metric invariance

AST.Etoh_Tob_Thc.model.GENDER.METRIC.fit <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                                        data.etoh_tob_thc.dems.GENDER.02,
                                                        std.lv=TRUE,
                                                        estimator = "WLSMV",
                                                        group = "GENDER",
                                                        group.equal=c("loadings"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_Thc.model.GENDER.METRIC.fit, fit.measures= TRUE, standardized=TRUE)



################# Chi-square difference test between Configural invariance and metric invariance. 

anova(AST.Etoh_Tob_Thc.model.GENDER.fit, AST.Etoh_Tob_Thc.model.GENDER.METRIC.fit)


################### scalar invariance

AST.Etoh_Tob_Thc.model.GENDER.SCALAR.fit  <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                                         data.etoh_tob_thc.dems.GENDER.02,
                                                         std.lv=TRUE,
                                                         estimator = "WLSMV",
                                                         group = "GENDER",
                                                         group.equal=c("loadings", "intercepts"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_Thc.model.GENDER.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)



############################ Test difference

anova(AST.Etoh_Tob_Thc.model.GENDER.METRIC.fit, AST.Etoh_Tob_Thc.model.GENDER.SCALAR.fit)

#equal factor means specification

AST.Etoh_Tob_Thc.model.GENDER.MEANS.fit  <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                                        data.etoh_tob_thc.dems.GENDER.02,
                                                        std.lv=TRUE,
                                                        estimator = "WLSMV",
                                                        group = "GENDER",
                                                        group.equal=c("loadings", "intercepts", "means"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_Thc.model.GENDER.MEANS.fit, fit.measures= TRUE, standardized=TRUE)




anova(AST.Etoh_Tob_Thc.model.GENDER.SCALAR.fit, AST.Etoh_Tob_Thc.model.GENDER.MEANS.fit)


# Note: Figure out what this means considering these models significantly differ.


################### strict invariance

AST.Etoh_Tob_Thc.model.GENDER.STRICT.fit  <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                                         data.etoh_tob_thc.dems.GENDER.02,
                                                         std.lv=TRUE,
                                                         estimator = "WLSMV",
                                                         group = "GENDER",
                                                         group.equal=c("loadings", "intercepts", "residuals"))

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_Thc.model.GENDER.STRICT.fit, fit.measures= TRUE, standardized=TRUE)


anova(AST.Etoh_Tob_Thc.model.GENDER.SCALAR.fit, AST.Etoh_Tob_Thc.model.GENDER.STRICT.fit)

################### factor variances and covariances
AST.Etoh_Tob_Thc.model.GENDER.FVCV.fit  <- lavaan::cfa(AST.Etoh_Tob_Thc.model,
                                                       data.etoh_tob_thc.dems.GENDER.02,
                                                       std.lv=TRUE,
                                                       estimator = "WLSMV",
                                                       group = "GENDER",
                                                       group.equal=c("loadings", "intercepts",
                                                                     "residuals", "lv.variances", "lv.covariances"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.Etoh_Tob_THC.model.GENDER.FVCV.fit, fit.measures= TRUE, standardized=TRUE)




