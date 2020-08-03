

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

#########################################################################################################
#########################################################################################################
#####################        ETOH datasets 
#########################################################################################################

##################### Merge OPS & DEM Data Sets ###############################

data.etoh.dems <- merge.data.frame(data.etoh, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                  by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                  suffixes = c(".x", ".y"), incomparables = NULL)

##################### Merge ETOH.DEMS & TOB Data Sets ###############################

data.etoh_tob.dems <- merge.data.frame(data.tob, data.etoh.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_tob.dems)
# > psych::describe(data.etoh_tob.dems)
# vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1433 4868986.18 2883468.94 4734816 4844011.48 3745043.15 10594 9994871 9984277  0.08
# TOB_TOT     2 1433      10.91      10.11       8      10.06      11.86     0      31      31  0.43
# ATTOB3M     3 1433       2.92       2.78       3       2.90       4.45     0       6       6  0.06
# ATTOBURG    4 1433       2.68       2.84       0       2.59       0.00     0       6       6  0.18
# ATTOBPRB    5 1433       1.00       2.25       0       0.41       0.00     0       7       7  1.91
# ATTOBWOR    6 1433       2.12       2.53       0       1.91       0.00     0       6       6  0.59
# ATTOBTRY    7 1433       2.18       2.41       3       1.98       4.45     0       6       6  0.53
# ALC_TOT     8 1433       6.23       7.77       3       4.59       4.45     0      39      39  2.01
# ATALC3M     9 1433       2.01       1.89       2       1.84       2.97     0       6       6  0.42
# ATALCURG   10 1433       1.36       2.15       0       0.99       0.00     0       6       6  1.11
# ATALCPRB   11 1433       0.48       1.56       0       0.00       0.00     0       7       7  3.10
# ATALCEXP   12 1433       0.46       1.64       0       0.00       0.00     0       8       8  3.41
# ATALCWOR   13 1433       1.05       1.90       0       0.62       0.00     0       6       6  1.59
# ATALCTRY   14 1433       0.86       1.79       0       0.42       0.00     0       6       6  1.93
# GENDER     15 1433       1.70       3.62       2       1.59       0.00     1      98      97 25.93
# AGE        16 1433      45.88      14.73      48      45.86      16.31    18      94      76 -0.02
# RACE       17 1433       2.85       1.18       3       2.68       0.00     1       7       6  1.31
# EDU        18 1433      14.29       2.96      15      14.45       2.97     0      21      21 -0.45
# JOB        19 1433       5.08      13.64       3       3.05       2.97     1      99      98  6.60
# MARTL      20 1433       3.61       1.68       5       3.71       1.48     1       6       5 -0.50

# kurtosis       se
# PROJID      -1.21 76171.44
# TOB_TOT     -1.22     0.27
# ATTOB3M     -1.85     0.07
# ATTOBURG    -1.89     0.08
# ATTOBPRB     1.92     0.06
# ATTOBWOR    -1.34     0.07
# ATTOBTRY    -1.26     0.06
# ALC_TOT      4.12     0.21
# ATALC3M     -0.84     0.05
# ATALCURG    -0.47     0.06
# ATALCPRB     8.10     0.04
# ATALCEXP    10.19     0.04
# ATALCWOR     1.24     0.05
# ATALCTRY     2.44     0.05
# GENDER     684.07     0.10
# AGE         -0.65     0.39
# RACE         2.12     0.03
# EDU          0.41     0.08
# JOB         42.54     0.36
# MARTL       -1.30     0.04
# >

###############################################################################################
################# colnames() for the data.frames will be used #################################
###############################################################################################


##### Provide colnames to dataframes
colnames(data.etoh_tob.dems) <- c("PROJID", "TOB_TOT", "ATTOB3M", "ATTOBURG", "ATTOBPRB", "ATTOBWOR", "ATTOBTRY",
                                  "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY",
                                  "GENDER", "AGE", "RACE", "EDU", "JOB", "MARTL")
                          

###############################################################################################
################# subset() the data.frames that will be used for Co-use analysis ##############
###############################################################################################


##### Create data sets for the ESEM and invariance testing

(data.etoh_tob.dems  <- subset(data.etoh_tob.dems, select = c(ATALC3M, ATALCURG, ATALCPRB, ATALCEXP, ATALCWOR, ATALCTRY,
                                                              ATTOB3M, ATTOBURG, ATTOBPRB, ATTOBWOR, ATTOBTRY,
                                                              GENDER, AGE, RACE, EDU, JOB, MARTL)))
                                                              


##### Export the data 

write.table(data.etoh_tob.dems, file = "data.etoh_tob.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)


##################### Merge ETOH.DEMS.TOB & THC Data Sets ###############################


data.etoh_tob_thc.dems <- merge.data.frame(data.thc, data.etoh_tob.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                           by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                           suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_tob_thc.dems)
# > psych::describe(data.etoh_tob_thc.dems )
# vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1121 4885922.11 2908198.42 4868106 4864249.46 3831575.10 16534 9994871 9978337  0.05
# THC_TOT     2 1121       4.27       7.23       0       2.54       0.00     0      39      39  2.20
# ATTHC3M     3 1121       1.12       1.99       0       0.66       0.00     0       6       6  1.53
# ATTHCURG    4 1121       1.04       2.06       0       0.56       0.00     0       6       6  1.62
# ATTHCPRB    5 1121       0.32       1.32       0       0.00       0.00     0       7       7  4.02
# ATTHCEXP    6 1121       0.32       1.41       0       0.00       0.00     0       8       8  4.28
# ATTHCWOR    7 1121       0.75       1.59       0       0.38       0.00     0       6       6  2.05
# ATTHCTRY    8 1121       0.71       1.61       0       0.32       0.00     0       6       6  2.19
# TOB_TOT     9 1121      11.95      10.10      11      11.31      14.83     0      31      31  0.29
# ATTOB3M    10 1121       3.15       2.76       4       3.19       2.97     0       6       6 -0.09
# ATTOBURG   11 1121       2.92       2.85       3       2.90       4.45     0       6       6  0.02
# ATTOBPRB   12 1121       1.13       2.37       0       0.55       0.00     0       7       7  1.73
# ATTOBWOR   13 1121       2.34       2.54       3       2.18       4.45     0       6       6  0.43
# ATTOBTRY   14 1121       2.41       2.44       3       2.27       4.45     0       6       6  0.37
# ALC_TOT    15 1121       6.85       8.15       4       5.19       5.93     0      39      39  1.88
# ATALC3M    16 1121       2.10       1.93       2       1.91       2.97     0       6       6  0.38
# ATALCURG   17 1121       1.49       2.20       0       1.14       0.00     0       6       6  0.96
# ATALCPRB   18 1121       0.54       1.66       0       0.00       0.00     0       7       7  2.88
# ATALCEXP   19 1121       0.53       1.75       0       0.00       0.00     0       8       8  3.14
# ATALCWOR   20 1121       1.20       1.99       0       0.75       0.00     0       6       6  1.40
# ATALCTRY   21 1121       1.00       1.88       0       0.55       0.00     0       6       6  1.69
# GENDER     22 1121       1.74       4.08       2       1.58       0.00     1      98      97 23.06
# AGE        23 1121      45.96      14.81      48      45.94      16.31    18      94      76 -0.01
# RACE       24 1121       2.87       1.18       3       2.70       0.00     1       7       6  1.28
# EDU        25 1121      14.24       2.98      15      14.40       2.97     0      21      21 -0.48
# JOB        26 1121       5.00      13.44       3       3.02       2.97     1      99      98  6.70
# MARTL      27 1121       3.63       1.68       5       3.74       1.48     1       6       5 -0.52
# kurtosis       se
# PROJID      -1.24 86860.28
# THC_TOT      4.83     0.22
# ATTHC3M      0.89     0.06
# ATTHCURG     0.87     0.06
# ATTHCPRB    14.90     0.04
# ATTHCEXP    17.06     0.04
# ATTHCWOR     3.23     0.05
# ATTHCTRY     3.77     0.05
# TOB_TOT     -1.30     0.30
# ATTOB3M     -1.83     0.08
# ATTOBURG    -1.92     0.09
# ATTOBPRB     1.22     0.07
# ATTOBWOR    -1.47     0.08
# ATTOBTRY    -1.40     0.07
# ALC_TOT      3.43     0.24
# ATALC3M     -0.91     0.06
# ATALCURG    -0.78     0.07
# ATALCPRB     6.72     0.05
# ATALCEXP     8.34     0.05
# ATALCWOR     0.62     0.06
# ATALCTRY     1.54     0.06
# GENDER     538.23     0.12
# AGE         -0.62     0.44
# RACE         1.98     0.04
# EDU          0.52     0.09
# JOB         43.95     0.40
# MARTL       -1.27     0.05
# > 

##################### Merge ETOH.DEMS & THC Data Sets ###############################


data.etoh_thc.dems <- merge.data.frame(data.thc, data.etoh.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                           by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                           suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_thc.dems)
# > psych::describe(data.etoh_thc.dems)
#          vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1256 4901558.89 2914568.31 4863908 4878278.46 3815257.61 10115 9994871 9984756  0.06
# THC_TOT     2 1256       4.08       7.09       0       2.37       0.00     0      39      39  2.27
# ATTHC3M     3 1256       1.08       1.95       0       0.63       0.00     0       6       6  1.59
# ATTHCURG    4 1256       0.99       2.03       0       0.50       0.00     0       6       6  1.70
# ATTHCPRB    5 1256       0.30       1.27       0       0.00       0.00     0       7       7  4.17
# ATTHCEXP    6 1256       0.30       1.36       0       0.00       0.00     0       8       8  4.45
# ATTHCWOR    7 1256       0.73       1.59       0       0.36       0.00     0       6       6  2.11
# ATTHCTRY    8 1256       0.68       1.57       0       0.29       0.00     0       6       6  2.27
# ALC_TOT     9 1256       6.66       8.03       4       5.00       5.93     0      39      39  1.93
# ATALC3M    10 1256       2.09       1.91       2       1.91       2.97     0       6       6  0.39
# ATALCURG   11 1256       1.45       2.17       0       1.10       0.00     0       6       6  1.00
# ATALCPRB   12 1256       0.52       1.63       0       0.00       0.00     0       7       7  2.94
# ATALCEXP   13 1256       0.50       1.71       0       0.00       0.00     0       8       8  3.25
# ATALCWOR   14 1256       1.14       1.96       0       0.69       0.00     0       6       6  1.48
# ATALCTRY   15 1256       0.96       1.85       0       0.52       0.00     0       6       6  1.75
# GENDER     16 1256       1.72       3.86       2       1.58       0.00     1      98      97 24.35
# AGE        17 1256      45.91      14.87      48      45.91      16.31    18      94      76 -0.01
# RACE       18 1256       2.86       1.17       3       2.69       0.00     1       7       6  1.28
# EDU        19 1256      14.23       2.99      15      14.38       2.97     0      21      21 -0.44
# JOB        20 1256       5.11      13.79       3       3.04       2.97     1      99      98  6.53
# MARTL      21 1256       3.64       1.68       5       3.75       1.48     1       6       5 -0.53

#          kurtosis       se
# PROJID      -1.23 82239.30
# THC_TOT      5.25     0.20
# ATTHC3M      1.10     0.06
# ATTHCURG     1.14     0.06
# ATTHCPRB    16.19     0.04
# ATTHCEXP    18.54     0.04
# ATTHCWOR     3.48     0.04
# ATTHCTRY     4.15     0.04
# ALC_TOT      3.63     0.23
# ATALC3M     -0.87     0.05
# ATALCURG    -0.69     0.06
# ATALCPRB     7.09     0.05
# ATALCEXP     9.07     0.05
# ATALCWOR     0.85     0.06
# ATALCTRY     1.78     0.05
# GENDER     601.61     0.11
# AGE         -0.64     0.42
# RACE         2.02     0.03
# EDU          0.39     0.08
# JOB         41.51     0.39
# MARTL       -1.26     0.05
# >

##################### Merge ETOH.DEMS & COC Data Sets ###############################


data.etoh_coc.dems <- merge.data.frame(data.coc, data.etoh.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_coc.dems)
# > psych::describe(data.etoh_coc.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 657 4892832.69 2903887.83 4839502 4876245.99 3751624.41 10115 9994871 9984756  0.06
# COC_TOT     2 657       4.80       8.03       0       2.73       0.00     0      39      39  2.34
# ATCOC3M     3 657       0.53       1.30       0       0.17       0.00     0       6       6  2.47
# ATCOCURG    4 657       0.54       1.51       0       0.08       0.00     0       6       6  2.61
# ATCOCPRB    5 657       0.45       1.52       0       0.00       0.00     0       7       7  3.20
# ATCOCEXP    6 657       0.49       1.71       0       0.00       0.00     0       8       8  3.34
# ATCOCWOR    7 657       1.47       1.98       0       1.12       0.00     0       6       6  1.00
# ATCOCTRY    8 657       1.32       1.89       0       1.00       0.00     0       6       6  1.12
# ALC_TOT     9 657       8.44       9.40       6       6.68       5.93     0      39      39  1.55
# ATALC3M    10 657       2.15       2.06       2       1.94       2.97     0       6       6  0.40
# ATALCURG   11 657       1.61       2.31       0       1.27       0.00     0       6       6  0.88
# ATALCPRB   12 657       0.82       2.02       0       0.27       0.00     0       7       7  2.16
# ATALCEXP   13 657       0.80       2.12       0       0.19       0.00     0       8       8  2.39
# ATALCWOR   14 657       1.65       2.15       0       1.31       0.00     0       6       6  0.91
# ATALCTRY   15 657       1.40       2.07       0       1.01       0.00     0       6       6  1.15
# GENDER     16 657       1.86       5.31       2       1.59       0.00     1      98      97 17.77
# AGE        17 657      45.80      14.87      48      45.79      16.31    18      94      76  0.01
# RACE       18 657       2.91       1.16       3       2.72       0.00     1       7       6  1.38
# EDU        19 657      14.31       3.00      15      14.46       2.97     4      21      17 -0.39
# JOB        20 657       4.63      11.90       3       3.06       2.97     1      99      98  7.58
# MARTL      21 657       3.68       1.67       5       3.80       1.48     1       6       5 -0.57

#          kurtosis        se
# PROJID      -1.23 113291.45
# COC_TOT      5.06      0.31
# ATCOC3M      5.37      0.05
# ATCOCURG     5.31      0.06
# ATCOCPRB     8.84      0.06
# ATCOCEXP     9.61      0.07
# ATCOCWOR    -0.18      0.08
# ATCOCTRY     0.15      0.07
# ALC_TOT      1.64      0.37
# ATALC3M     -1.06      0.08
# ATALCURG    -0.98      0.09
# ATALCPRB     2.96      0.08
# ATALCEXP     4.07      0.08
# ATALCWOR    -0.53      0.08
# ATALCTRY    -0.02      0.08
# GENDER     316.93      0.21
# AGE         -0.56      0.58
# RACE         2.08      0.05
# EDU          0.06      0.12
# JOB         57.15      0.46
# MARTL       -1.21      0.07
# >

##################### Merge ETOH.DEMS & AMP Data Sets ###############################


data.etoh_amp.dems <- merge.data.frame(data.amp, data.etoh.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_amp.dems)
# > psych::describe(data.etoh_amp.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 367 5026988.25 2972930.67 4992895 5026822.78 4031374.72 10115 9969094 9958979 -0.01
# AMP_TOT     2 367       2.05       5.22       0       0.85       0.00     0      39      39  4.22
# ATAMP3M     3 367       0.24       0.84       0       0.00       0.00     0       6       6  4.14
# ATAMPURG    4 367       0.24       1.01       0       0.00       0.00     0       6       6  4.15
# ATAMPPRB    5 367       0.16       0.92       0       0.00       0.00     0       7       7  5.95
# ATAMPEXP    6 367       0.20       1.12       0       0.00       0.00     0       8       8  5.54
# ATAMPWOR    7 367       0.67       1.52       0       0.32       0.00     0       6       6  2.21
# ATAMPTRY    8 367       0.54       1.37       0       0.19       0.00     0       6       6  2.55
# ALC_TOT     9 367       8.34       9.13       6       6.57       4.45     0      39      39  1.73
# ATALC3M    10 367       2.32       2.08       2       2.16       2.97     0       6       6  0.29
# ATALCURG   11 367       1.74       2.36       0       1.43       0.00     0       6       6  0.76
# ATALCPRB   12 367       0.69       1.90       0       0.12       0.00     0       7       7  2.49
# ATALCEXP   13 367       0.72       2.02       0       0.12       0.00     0       8       8  2.59
# ATALCWOR   14 367       1.60       2.05       0       1.26       0.00     0       6       6  0.90
# ATALCTRY   15 367       1.26       2.03       0       0.83       0.00     0       6       6  1.32
# GENDER     16 367       2.12       7.09       2       1.62       0.00     1      98      97 13.28
# AGE        17 367      45.31      14.61      47      45.23      16.31    18      90      72  0.02
# RACE       18 367       2.90       1.14       3       2.73       0.00     1       6       5  1.32
# EDU        19 367      14.41       2.81      15      14.57       2.97     4      21      17 -0.51
# JOB        20 367       4.55      11.29       3       3.13       2.97     1      99      98  7.96
# MARTL      21 367       3.69       1.67       5       3.81       1.48     1       6       5 -0.62
# kurtosis        se
# PROJID      -1.29 155185.72
# AMP_TOT     20.90      0.27
# ATAMP3M     19.71      0.04
# ATAMPURG    16.24      0.05
# ATAMPPRB    35.26      0.05
# ATAMPEXP    29.96      0.06
# ATAMPWOR     4.03      0.08
# ATAMPTRY     5.88      0.07
# ALC_TOT      2.43      0.48
# ATALC3M     -1.11      0.11
# ATALCURG    -1.19      0.12
# ATALCPRB     4.55      0.10
# ATALCEXP     5.13      0.11
# ATALCWOR    -0.42      0.11
# ATALCTRY     0.38      0.11
# GENDER     175.81      0.37
# AGE         -0.61      0.76
# RACE         2.05      0.06
# EDU          0.62      0.15
# JOB         63.66      0.59
# MARTL       -1.20      0.09
# > 

##################### Merge ETOH.DEMS & OPS Data Sets ###############################


data.etoh_ops.dems <- merge.data.frame(data.ops, data.etoh.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_ops.dems)
# > psych::describe(data.etoh_ops.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 399 4833237.26 2960543.43 4633423 4800404.36 3774499.45 10115 9937226 9927111  0.10
# OPS_TOT     2 399       7.79      10.86       3       5.42       4.45     0      44      44  1.73
# ATOPS3M     3 399       0.98       1.87       0       0.53       0.00     0       6       6  1.75
# ATOPSURG    4 399       1.02       2.08       0       0.54       0.00     0       6       6  1.66
# ATOPSPRB    5 399       0.68       1.89       0       0.11       0.00     0       7       7  2.54
# ATOPSEXP    6 399       0.81       2.21       0       0.16       0.00     0       8       8  2.46
# ATOPSWOR    7 399       1.64       2.14       0       1.31       0.00     0       6       6  0.91
# ATOPSTRY    8 399       1.65       2.19       0       1.33       0.00     0       6       6  0.92
# ATDRGINJ    9 399       0.35       0.57       0       0.26       0.00     0       2       2  1.38
# ATINJFRQ   10 399       0.66       1.22       0       0.45       0.00     0       3       3  1.34
# ALC_TOT    11 399       8.63       9.65       6       6.87       5.93     0      39      39  1.47
# ATALC3M    12 399       2.09       2.04       2       1.86       2.97     0       6       6  0.45
# ATALCURG   13 399       1.66       2.31       0       1.35       0.00     0       6       6  0.80
# ATALCPRB   14 399       0.89       2.11       0       0.34       0.00     0       7       7  2.03
# ATALCEXP   15 399       0.84       2.20       0       0.22       0.00     0       8       8  2.32
# ATALCWOR   16 399       1.71       2.16       0       1.40       0.00     0       6       6  0.84
# ATALCTRY   17 399       1.44       2.11       0       1.06       0.00     0       6       6  1.13
# GENDER     18 399       2.03       6.80       2       1.57       0.00     1      98      97 13.85
# AGE        19 399      46.15      14.60      49      46.22      14.83    18      89      71 -0.08
# RACE       20 399       2.83       1.13       3       2.68       0.00     1       7       6  1.36
# EDU        21 399      14.31       2.92      15      14.44       2.97     4      21      17 -0.33
# JOB        22 399       4.59      11.84       3       3.04       2.97     1      99      98  7.61
# MARTL      23 399       3.60       1.70       5       3.69       1.48     1       6       5 -0.50
# kurtosis        se
# PROJID      -1.25 148212.55
# OPS_TOT      2.09      0.54
# ATOPS3M      1.74      0.09
# ATOPSURG     0.97      0.10
# ATOPSPRB     4.83      0.09
# ATOPSEXP     4.36      0.11
# ATOPSWOR    -0.51      0.11
# ATOPSTRY    -0.57      0.11
# ATDRGINJ     0.89      0.03
# ATINJFRQ    -0.15      0.06
# ALC_TOT      1.29      0.48
# ATALC3M     -1.00      0.10
# ATALCURG    -1.12      0.12
# ATALCPRB     2.40      0.11
# ATALCEXP     3.69      0.11
# ATALCWOR    -0.63      0.11
# ATALCTRY    -0.12      0.11
# GENDER     191.45      0.34
# AGE         -0.61      0.73
# RACE         2.55      0.06
# EDU          0.07      0.15
# JOB         57.77      0.59
# MARTL       -1.34      0.09
# >

##################### Merge ETOH_OPS & TOB Data Sets ###############################


data.etoh_ops_tob.dems <- merge.data.frame(data.tob, data.etoh_ops.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.etoh_ops_tob.dems)
# > psych::describe(data.etoh_ops_tob.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 372 4847748.65 2942027.91 4589359 4816377.97 3713007.87 16534 9937226 9920692  0.10
# TOB_TOT     2 372      15.91      10.16      18      16.02      11.86     0      31      31 -0.20
# ATTOB3M     3 372       3.90       2.68       6       4.13       0.00     0       6       6 -0.63
# ATTOBURG    4 372       3.81       2.77       6       4.01       0.00     0       6       6 -0.58
# ATTOBPRB    5 372       1.84       2.87       0       1.43       0.00     0       7       7  1.00
# ATTOBWOR    6 372       3.08       2.54       3       3.10       4.45     0       6       6 -0.05
# ATTOBTRY    7 372       3.28       2.42       3       3.35       4.45     0       6       6 -0.17
# OPS_TOT     8 372       7.75      10.87       3       5.33       4.45     0      44      44  1.73
# ATOPS3M     9 372       0.95       1.86       0       0.49       0.00     0       6       6  1.79
# ATOPSURG   10 372       1.01       2.09       0       0.51       0.00     0       6       6  1.68
# ATOPSPRB   11 372       0.68       1.89       0       0.11       0.00     0       7       7  2.53
# ATOPSEXP   12 372       0.81       2.19       0       0.15       0.00     0       8       8  2.45
# ATOPSWOR   13 372       1.61       2.13       0       1.27       0.00     0       6       6  0.93
# ATOPSTRY   14 372       1.65       2.18       0       1.31       0.00     0       6       6  0.92
# ATDRGINJ   15 372       0.36       0.57       0       0.27       0.00     0       2       2  1.33
# ATINJFRQ   16 372       0.68       1.23       0       0.48       0.00     0       3       3  1.29
# ALC_TOT    17 372       8.65       9.65       6       6.86       5.93     0      39      39  1.48
# ATALC3M    18 372       2.08       2.04       2       1.85       2.97     0       6       6  0.44
# ATALCURG   19 372       1.68       2.33       0       1.37       0.00     0       6       6  0.79
# ATALCPRB   20 372       0.90       2.12       0       0.34       0.00     0       7       7  2.02
# ATALCEXP   21 372       0.85       2.20       0       0.22       0.00     0       8       8  2.30
# ATALCWOR   22 372       1.71       2.14       0       1.39       0.00     0       6       6  0.84
# ATALCTRY   23 372       1.44       2.11       0       1.05       0.00     0       6       6  1.13
# GENDER     24 372       2.07       7.04       2       1.57       0.00     1      98      97 13.37
# AGE        25 372      46.14      14.52      49      46.22      14.83    18      89      71 -0.07
# RACE       26 372       2.82       1.12       3       2.68       0.00     1       7       6  1.36
# EDU        27 372      14.29       2.93      15      14.43       2.97     4      21      17 -0.36
# JOB        28 372       4.40      11.23       3       2.98       2.97     1      99      98  8.03
# MARTL      29 372       3.61       1.70       5       3.71       1.48     1       6       5 -0.51
# kurtosis        se
# PROJID      -1.25 152537.04
# TOB_TOT     -1.28      0.53
# ATTOB3M     -1.47      0.14
# ATTOBURG    -1.58      0.14
# ATTOBPRB    -0.85      0.15
# ATTOBWOR    -1.61      0.13
# ATTOBTRY    -1.45      0.13
# OPS_TOT      2.11      0.56
# ATOPS3M      1.86      0.10
# ATOPSURG     1.02      0.11
# ATOPSPRB     4.77      0.10
# ATOPSEXP     4.34      0.11
# ATOPSWOR    -0.47      0.11
# ATOPSTRY    -0.55      0.11
# ATDRGINJ     0.77      0.03
# ATINJFRQ    -0.28      0.06
# ALC_TOT      1.34      0.50
# ATALC3M     -1.01      0.11
# ATALCURG    -1.16      0.12
# ATALCPRB     2.32      0.11
# ATALCEXP     3.59      0.11
# ATALCWOR    -0.61      0.11
# ATALCTRY    -0.12      0.11
# GENDER     178.22      0.37
# AGE         -0.57      0.75
# RACE         2.63      0.06
# EDU          0.05      0.15
# JOB         64.77      0.58
# MARTL       -1.32      0.09
# >

table(data.etoh_ops_tob.dems$GENDER)
# > table(data.etoh_ops_tob.dems$GENDER)
# 
# 1   2  97  98 
# 166 204   1   1 
# >

table(data.etoh_ops_tob.dems$RACE)
# > table(data.etoh_ops_tob.dems$RACE)
# 
# 1   2   3   4   5   6   7 
# 26 107 204   3   7  24   1 
# >

#########################################################################################################
#########################################################################################################
#####################     TOB Datasets
#########################################################################################################

##################### Merge TOB Data Sets ###############################

data.tob.dems <- merge.data.frame(data.tob, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                  by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                  suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.tob.dems)
# > psych::describe(data.tob.dems)
# vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1507 4874794.14 2888076.19 4751473 4851762.00 3762020.40 10594 9994871 9984277  0.07
# TOB_TOT     2 1507      11.00      10.19       8      10.16      11.86     0      31      31  0.42
# ATTOB3M     3 1507       2.94       2.78       3       2.92       4.45     0       6       6  0.04
# ATTOBURG    4 1507       2.70       2.85       0       2.62       0.00     0       6       6  0.17
# ATTOBPRB    5 1507       1.03       2.28       0       0.44       0.00     0       7       7  1.88
# ATTOBWOR    6 1507       2.14       2.54       0       1.92       0.00     0       6       6  0.58
# ATTOBTRY    7 1507       2.20       2.42       3       2.00       4.45     0       6       6  0.52
# GENDER      8 1507       1.70       3.53       2       1.59       0.00     1      98      97 26.56
# AGE         9 1507      45.93      14.65      48      45.95      16.31    18      94      76 -0.04
# RACE       10 1507       2.85       1.18       3       2.68       0.00     1       7       6  1.31
# EDU        11 1507      14.32       2.96      15      14.47       2.97     0      21      21 -0.45
# JOB        12 1507       5.06      13.53       3       3.07       2.97     1      99      98  6.66
# MARTL      13 1507       3.59       1.69       4       3.69       1.48     1       6       5 -0.48
# kurtosis       se
# PROJID      -1.22 74396.42
# TOB_TOT     -1.24     0.26
# ATTOB3M     -1.85     0.07
# ATTOBURG    -1.89     0.07
# ATTOBPRB     1.78     0.06
# ATTOBWOR    -1.36     0.07
# ATTOBTRY    -1.28     0.06
# GENDER     718.32     0.09
# AGE         -0.65     0.38
# RACE         2.13     0.03
# EDU          0.42     0.08
# JOB         43.29     0.35
# MARTL       -1.33     0.04
# >
  
  
##################### Merge TOB.DEMS & THC Data Sets ###############################

data.tob_thc.dems <- merge.data.frame(data.thc, data.tob.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                       by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                       suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.tob_thc.dems)
# > psych::describe(data.tob_thc.dems)
#          vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1150 4895245.90 2909213.24 4874128 4875709.10 3844716.13 16534 9994871 9978337  0.05
# THC_TOT     2 1150       4.33       7.31       0       2.60       0.00     0      39      39  2.18
# ATTHC3M     3 1150       1.14       2.00       0       0.68       0.00     0       6       6  1.51
# ATTHCURG    4 1150       1.05       2.07       0       0.57       0.00     0       6       6  1.61
# ATTHCPRB    5 1150       0.34       1.36       0       0.00       0.00     0       7       7  3.93
# ATTHCEXP    6 1150       0.33       1.42       0       0.00       0.00     0       8       8  4.27
# ATTHCWOR    7 1150       0.76       1.61       0       0.38       0.00     0       6       6  2.04
# ATTHCTRY    8 1150       0.73       1.63       0       0.33       0.00     0       6       6  2.16
# TOB_TOT     9 1150      12.09      10.15      12      11.45      13.34     0      31      31  0.28
# ATTOB3M    10 1150       3.18       2.75       4       3.22       2.97     0       6       6 -0.11
# ATTOBURG   11 1150       2.95       2.85       3       2.93       4.45     0       6       6  0.00
# ATTOBPRB   12 1150       1.17       2.40       0       0.59       0.00     0       7       7  1.68
# ATTOBWOR   13 1150       2.36       2.55       3       2.20       4.45     0       6       6  0.42
# ATTOBTRY   14 1150       2.44       2.45       3       2.30       4.45     0       6       6  0.35
# GENDER     15 1150       1.73       4.03       2       1.58       0.00     1      98      97 23.34
# AGE        16 1150      45.96      14.74      48      45.95      16.31    18      94      76 -0.01
# RACE       17 1150       2.86       1.17       3       2.69       0.00     1       7       6  1.29
# EDU        18 1150      14.26       2.98      15      14.42       2.97     0      21      21 -0.48
# JOB        19 1150       4.96      13.28       3       3.03       2.97     1      99      98  6.79
# MARTL      20 1150       3.61       1.69       4       3.71       1.48     1       6       5 -0.49

#          kurtosis       se
# PROJID      -1.24 85788.02
# THC_TOT      4.78     0.22
# ATTHC3M      0.81     0.06
# ATTHCURG     0.82     0.06
# ATTHCPRB    14.14     0.04
# ATTHCEXP    16.93     0.04
# ATTHCWOR     3.19     0.05
# ATTHCTRY     3.58     0.05
# TOB_TOT     -1.31     0.30
# ATTOB3M     -1.82     0.08
# ATTOBURG    -1.92     0.08
# ATTOBPRB     1.05     0.07
# ATTOBWOR    -1.49     0.08
# ATTOBTRY    -1.42     0.07
# GENDER     551.91     0.12
# AGE         -0.61     0.43
# RACE         2.06     0.03
# EDU          0.50     0.09
# JOB         45.16     0.39
# MARTL       -1.31     0.05
# >

##################### Merge TOB_THC & ETOH Data Sets ###############################

data.tob_thc_etoh.dems <- merge.data.frame(data.etoh, data.tob_thc.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.tob_thc_etoh.dems)
# > psych::describe(data.tob_thc_etoh.dems)
# vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1121 4885922.11 2908198.42 4868106 4864249.46 3831575.10 16534 9994871 9978337  0.05
# ALC_TOT     2 1121       6.85       8.15       4       5.19       5.93     0      39      39  1.88
# ATALC3M     3 1121       2.10       1.93       2       1.91       2.97     0       6       6  0.38
# ATALCURG    4 1121       1.49       2.20       0       1.14       0.00     0       6       6  0.96
# ATALCPRB    5 1121       0.54       1.66       0       0.00       0.00     0       7       7  2.88
# ATALCEXP    6 1121       0.53       1.75       0       0.00       0.00     0       8       8  3.14
# ATALCWOR    7 1121       1.20       1.99       0       0.75       0.00     0       6       6  1.40
# ATALCTRY    8 1121       1.00       1.88       0       0.55       0.00     0       6       6  1.69
# THC_TOT     9 1121       4.27       7.23       0       2.54       0.00     0      39      39  2.20
# ATTHC3M    10 1121       1.12       1.99       0       0.66       0.00     0       6       6  1.53
# ATTHCURG   11 1121       1.04       2.06       0       0.56       0.00     0       6       6  1.62
# ATTHCPRB   12 1121       0.32       1.32       0       0.00       0.00     0       7       7  4.02
# ATTHCEXP   13 1121       0.32       1.41       0       0.00       0.00     0       8       8  4.28
# ATTHCWOR   14 1121       0.75       1.59       0       0.38       0.00     0       6       6  2.05
# ATTHCTRY   15 1121       0.71       1.61       0       0.32       0.00     0       6       6  2.19
# TOB_TOT    16 1121      11.95      10.10      11      11.31      14.83     0      31      31  0.29
# ATTOB3M    17 1121       3.15       2.76       4       3.19       2.97     0       6       6 -0.09
# ATTOBURG   18 1121       2.92       2.85       3       2.90       4.45     0       6       6  0.02
# ATTOBPRB   19 1121       1.13       2.37       0       0.55       0.00     0       7       7  1.73
# ATTOBWOR   20 1121       2.34       2.54       3       2.18       4.45     0       6       6  0.43
# ATTOBTRY   21 1121       2.41       2.44       3       2.27       4.45     0       6       6  0.37
# GENDER     22 1121       1.74       4.08       2       1.58       0.00     1      98      97 23.06
# AGE        23 1121      45.96      14.81      48      45.94      16.31    18      94      76 -0.01
# RACE       24 1121       2.87       1.18       3       2.70       0.00     1       7       6  1.28
# EDU        25 1121      14.24       2.98      15      14.40       2.97     0      21      21 -0.48
# JOB        26 1121       5.00      13.44       3       3.02       2.97     1      99      98  6.70
# MARTL      27 1121       3.63       1.68       5       3.74       1.48     1       6       5 -0.52
# kurtosis       se
# PROJID      -1.24 86860.28
# ALC_TOT      3.43     0.24
# ATALC3M     -0.91     0.06
# ATALCURG    -0.78     0.07
# ATALCPRB     6.72     0.05
# ATALCEXP     8.34     0.05
# ATALCWOR     0.62     0.06
# ATALCTRY     1.54     0.06
# THC_TOT      4.83     0.22
# ATTHC3M      0.89     0.06
# ATTHCURG     0.87     0.06
# ATTHCPRB    14.90     0.04
# ATTHCEXP    17.06     0.04
# ATTHCWOR     3.23     0.05
# ATTHCTRY     3.77     0.05
# TOB_TOT     -1.30     0.30
# ATTOB3M     -1.83     0.08
# ATTOBURG    -1.92     0.09
# ATTOBPRB     1.22     0.07
# ATTOBWOR    -1.47     0.08
# ATTOBTRY    -1.40     0.07
# GENDER     538.23     0.12
# AGE         -0.62     0.44
# RACE         1.98     0.04
# EDU          0.52     0.09
# JOB         43.95     0.40
# MARTL       -1.27     0.05
# >

##################### Merge TOB.DEMS & COC Data Sets ###############################

data.tob_coc.dems <- merge.data.frame(data.coc, data.tob.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                           by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                           suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.tob_coc.dems)
# > psych::describe(data.tob_coc.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 621 4865951.35 2889398.10 4741519 4849546.94 3757050.73 10594 9994871 9984277  0.06
# COC_TOT     2 621       4.90       8.15       2       2.78       2.97     0      39      39  2.33
# ATCOC3M     3 621       0.53       1.31       0       0.17       0.00     0       6       6  2.46
# ATCOCURG    4 621       0.55       1.53       0       0.08       0.00     0       6       6  2.59
# ATCOCPRB    5 621       0.47       1.54       0       0.00       0.00     0       7       7  3.12
# ATCOCEXP    6 621       0.50       1.73       0       0.00       0.00     0       8       8  3.27
# ATCOCWOR    7 621       1.50       1.99       0       1.15       0.00     0       6       6  0.97
# ATCOCTRY    8 621       1.34       1.90       0       1.01       0.00     0       6       6  1.11
# TOB_TOT     9 621      14.43      10.01      15      14.24      13.34     0      31      31  0.00
# ATTOB3M    10 621       3.66       2.70       6       3.82       0.00     0       6       6 -0.44
# ATTOBURG   11 621       3.47       2.82       5       3.59       1.48     0       6       6 -0.36
# ATTOBPRB   12 621       1.48       2.65       0       0.98       0.00     0       7       7  1.32
# ATTOBWOR   13 621       2.90       2.55       3       2.87       4.45     0       6       6  0.06
# ATTOBTRY   14 621       2.92       2.42       3       2.90       4.45     0       6       6  0.05
# GENDER     15 621       1.88       5.46       2       1.59       0.00     1      98      97 17.28
# AGE        16 621      45.86      14.80      48      45.81      16.31    18      94      76  0.03
# RACE       17 621       2.89       1.16       3       2.70       0.00     1       7       6  1.39
# EDU        18 621      14.33       2.99      15      14.49       2.97     4      21      17 -0.41
# JOB        19 621       4.37      10.99       3       3.00       2.97     1      99      98  8.21
# MARTL      20 621       3.66       1.69       5       3.77       1.48     1       6       5 -0.55
# kurtosis        se
# PROJID      -1.24 115947.55
# COC_TOT      4.96      0.33
# ATCOC3M      5.29      0.05
# ATCOCURG     5.17      0.06
# ATCOCPRB     8.30      0.06
# ATCOCEXP     9.14      0.07
# ATCOCWOR    -0.24      0.08
# ATCOCTRY     0.11      0.08
# TOB_TOT     -1.31      0.40
# ATTOB3M     -1.64      0.11
# ATTOBURG    -1.80      0.11
# ATTOBPRB    -0.08      0.11
# ATTOBWOR    -1.61      0.10
# ATTOBTRY    -1.46      0.10
# GENDER     299.51      0.22
# AGE         -0.54      0.59
# RACE         2.16      0.05
# EDU          0.09      0.12
# JOB         67.75      0.44
# MARTL       -1.26      0.07
# >

##################### Merge TOB.DEMS & AMP Data Sets ###############################

data.tob_amp.dems <- merge.data.frame(data.amp, data.tob.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.tob_amp.dems)
# > psych::describe(data.tob_amp.dems)
# vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 342 5039539.32 2977790.03 4998093 5044434.99 4023981.00 37119 9969094 9931975 -0.01
# AMP_TOT     2 342       2.19       5.63       0       0.90       0.00     0      39      39  4.24
# ATAMP3M     3 342       0.26       0.90       0       0.00       0.00     0       6       6  4.17
# ATAMPURG    4 342       0.25       1.04       0       0.00       0.00     0       6       6  4.14
# ATAMPPRB    5 342       0.18       1.00       0       0.00       0.00     0       7       7  5.68
# ATAMPEXP    6 342       0.23       1.20       0       0.00       0.00     0       8       8  5.31
# ATAMPWOR    7 342       0.70       1.55       0       0.34       0.00     0       6       6  2.14
# ATAMPTRY    8 342       0.58       1.43       0       0.22       0.00     0       6       6  2.46
# TOB_TOT     9 342      13.75      10.55      14      13.38      14.83     0      31      31  0.13
# ATTOB3M    10 342       3.39       2.77       4       3.48       2.97     0       6       6 -0.26
# ATTOBURG   11 342       3.27       2.86       5       3.34       1.48     0       6       6 -0.22
# ATTOBPRB   12 342       1.51       2.68       0       1.01       0.00     0       7       7  1.30
# ATTOBWOR   13 342       2.78       2.49       3       2.73       4.45     0       6       6  0.14
# ATTOBTRY   14 342       2.80       2.45       3       2.75       4.45     0       6       6  0.12
# GENDER     15 342       2.16       7.34       2       1.62       0.00     1      98      97 12.82
# AGE        16 342      45.23      14.44      47      45.12      14.83    18      90      72  0.03
# RACE       17 342       2.88       1.13       3       2.70       0.00     1       6       5  1.35
# EDU        18 342      14.43       2.81      15      14.58       2.97     4      21      17 -0.51
# JOB        19 342       3.99       9.17       3       2.99       2.97     1      99      98  9.75
# MARTL      20 342       3.65       1.70       5       3.76       1.48     1       6       5 -0.56
# kurtosis        se
# PROJID      -1.29 161020.44
# AMP_TOT     20.53      0.30
# ATAMP3M     19.70      0.05
# ATAMPURG    16.23      0.06
# ATAMPPRB    31.66      0.05
# ATAMPEXP    27.33      0.07
# ATAMPWOR     3.69      0.08
# ATAMPTRY     5.32      0.08
# TOB_TOT     -1.43      0.57
# ATTOB3M     -1.79      0.15
# ATTOBURG    -1.89      0.15
# ATTOBPRB    -0.14      0.14
# ATTOBWOR    -1.54      0.13
# ATTOBTRY    -1.49      0.13
# GENDER     163.54      0.40
# AGE         -0.56      0.78
# RACE         2.19      0.06
# EDU          0.69      0.15
# JOB         98.14      0.50
# MARTL       -1.28      0.09
# > 

##################### Merge TOB.DEMS & OPS Data Sets ###############################

data.tob_ops.dems <- merge.data.frame(data.ops, data.tob.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.tob_ops.dems)
# > psych::describe(data.tob_ops.dems)
#          vars   n       mean         sd    median    trimmed        mad   min     max   range  skew
# PROJID      1 384 4854489.91 2928789.97 4654184.0 4826403.23 3725407.60 16534 9937226 9920692  0.09
# OPS_TOT     2 384       7.96      11.00       4.0       5.55       5.93     0      44      44  1.72
# ATOPS3M     3 384       0.97       1.86       0.0       0.52       0.00     0       6       6  1.76
# ATOPSURG    4 384       1.01       2.10       0.0       0.52       0.00     0       6       6  1.67
# ATOPSPRB    5 384       0.71       1.92       0.0       0.13       0.00     0       7       7  2.47
# ATOPSEXP    6 384       0.84       2.23       0.0       0.18       0.00     0       8       8  2.40
# ATOPSWOR    7 384       1.67       2.13       0.0       1.34       0.00     0       6       6  0.87
# ATOPSTRY    8 384       1.69       2.18       0.0       1.36       0.00     0       6       6  0.88
# ATDRGINJ    9 384       0.37       0.58       0.0       0.28       0.00     0       2       2  1.30
# ATINJFRQ   10 384       0.70       1.25       0.0       0.50       0.00     0       3       3  1.25
# TOB_TOT    11 384      16.01      10.21      18.0      16.14      11.86     0      31      31 -0.21
# ATTOB3M    12 384       3.92       2.67       6.0       4.15       0.00     0       6       6 -0.64
# ATTOBURG   13 384       3.81       2.77       6.0       4.01       0.00     0       6       6 -0.59
# ATTOBPRB   14 384       1.85       2.87       0.0       1.44       0.00     0       7       7  0.99
# ATTOBWOR   15 384       3.12       2.55       3.0       3.15       4.45     0       6       6 -0.07
# ATTOBTRY   16 384       3.31       2.42       3.0       3.39       4.45     0       6       6 -0.19
# GENDER     17 384       2.06       6.93       2.0       1.57       0.00     1      98      97 13.59
# AGE        18 384      46.12      14.49      49.0      46.17      14.83    18      89      71 -0.06
# RACE       19 384       2.81       1.11       3.0       2.68       0.00     1       7       6  1.35
# EDU        20 384      14.35       2.94      15.0      14.48       2.97     4      21      17 -0.35
# JOB        21 384       4.37      11.06       3.0       2.98       2.97     1      99      98  8.15
# MARTL      22 384       3.59       1.70       4.5       3.69       0.74     1       6       5 -0.49
# kurtosis        se
# PROJID      -1.24 149459.19
# OPS_TOT      2.08      0.56
# ATOPS3M      1.75      0.10
# ATOPSURG     0.99      0.11
# ATOPSPRB     4.47      0.10
# ATOPSEXP     4.07      0.11
# ATOPSWOR    -0.55      0.11
# ATOPSTRY    -0.61      0.11
# ATDRGINJ     0.66      0.03
# ATINJFRQ    -0.39      0.06
# TOB_TOT     -1.29      0.52
# ATTOB3M     -1.45      0.14
# ATTOBURG    -1.58      0.14
# ATTOBPRB    -0.86      0.15
# ATTOBWOR    -1.61      0.13
# ATTOBTRY    -1.45      0.12
# GENDER     184.11      0.35
# AGE         -0.58      0.74
# RACE         2.69      0.06
# EDU          0.08      0.15
# JOB         66.87      0.56
# MARTL       -1.35      0.09
# > 
  

##### TOB_OPS based on GENDER
table(data.tob_ops.dems$GENDER)
# > table(data.tob_ops.dems$GENDER)
# 
# 1   2  97  98 
# 169 213   1   1 
# >

##### TOB_OPS based on RACE
table(data.tob_ops.dems$RACE)
# > table(data.tob_ops.dems$RACE)
# 
# 1   2   3   4   5   6   7 
# 28 110 211   3   7  24   1 
# > 

#########################################################################################################
#########################################################################################################
#####################   OPS Datasets
#########################################################################################################

##################### Merge OPS.DEMS Dataset ###############################

data.ops.dems <- merge.data.frame(data.ops, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)

psych::describe(data.ops.dems)
# > psych::describe(data.ops.dems)
#          vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 412 4851230.42 2953032.91 4680074 4821874.26 3775632.16 10115 9937226 9927111  0.09
# OPS_TOT     2 412       7.99      10.96       4       5.57       5.93     0      44      44  1.72
# ATOPS3M     3 412       1.00       1.87       0       0.54       0.00     0       6       6  1.73
# ATOPSURG    4 412       1.02       2.09       0       0.53       0.00     0       6       6  1.66
# ATOPSPRB    5 412       0.70       1.91       0       0.12       0.00     0       7       7  2.49
# ATOPSEXP    6 412       0.83       2.24       0       0.17       0.00     0       8       8  2.41
# ATOPSWOR    7 412       1.70       2.14       0       1.37       0.00     0       6       6  0.85
# ATOPSTRY    8 412       1.70       2.19       0       1.37       0.00     0       6       6  0.88
# ATDRGINJ    9 412       0.36       0.58       0       0.27       0.00     0       2       2  1.33
# ATINJFRQ   10 412       0.68       1.23       0       0.47       0.00     0       3       3  1.30
# GENDER     11 412       2.02       6.69       2       1.58       0.00     1      98      97 14.08
# AGE        12 412      46.19      14.61      49      46.24      14.83    18      89      71 -0.07
# RACE       13 412       2.81       1.12       3       2.68       0.00     1       7       6  1.34
# EDU        14 412      14.35       2.92      15      14.48       2.97     4      21      17 -0.32
# JOB        15 412       4.56      11.66       3       3.05       2.97     1      99      98  7.73
# MARTL      16 412       3.58       1.71       4       3.67       1.48     1       6       5 -0.47

#          kurtosis        se
# PROJID      -1.25 145485.49
# OPS_TOT      2.07      0.54
# ATOPS3M      1.66      0.09
# ATOPSURG     0.96      0.10
# ATOPSPRB     4.56      0.09
# ATOPSEXP     4.12      0.11
# ATOPSWOR    -0.59      0.11
# ATOPSTRY    -0.63      0.11
# ATDRGINJ     0.76      0.03
# ATINJFRQ    -0.25      0.06
# GENDER     197.82      0.33
# AGE         -0.63      0.72
# RACE         2.60      0.06
# EDU          0.07      0.14
# JOB         59.69      0.57
# MARTL       -1.37      0.08
# >


##### OPS by GENDER
table(data.ops.dems$GENDER)
# > table(data.ops.dems$GENDER)
# 
# 1   2  97  98 
# 181 229   1   1 
# >


##### OPS by RACE
table(data.ops.dems$RACE)
# > table(data.ops.dems$RACE)
# 
# 1   2   3   4   5   6   7 
# 31 117 226   3   7  27   1 
# >

##### OPS Frequencies of drug injection
table(data.ops.dems$ATDRGINJ)
# > table(data.ops.dems$ATDRGINJ)
# 
#   0   1   2 
# 283 108  21 
# >

##### OPS Frequencies of injection frequency
table(data.ops.dems$ATINJFRQ)
# > table(data.ops.dems$ATINJFRQ)
# 
#   0   1   2   3 
# 313   5   8  86 
# >
  
##################### Merge OPS.DEMS & COC Data Sets ###############################

data.ops_coc.dems <- merge.data.frame(data.coc, data.ops.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)


psych::describe(data.ops_coc.dems)
# > psych::describe(data.ops_coc.dems)
#          vars   n       mean         sd  median    trimmed        mad   min     max   range  skew
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

#          kurtosis        se
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
 
##################### Merge OPS_COC & ETOH Data Sets ###############################

data.ops_coc_etoh.dems <- merge.data.frame(data.etoh, data.ops_coc.dems, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                      by.y = c("PROJID"), all = FALSE, all.x = FALSE, all.y = FALSE, sort = TRUE,
                                      suffixes = c(".x", ".y"), incomparables = NULL)

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

#          kurtosis        se
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

###############################################################################################
################# colnames() for the data.frames will be used #################################
###############################################################################################


##### Provide colnames to dataframes
colnames(data.ops_coc.dems) <- c("PROJID", "COC_TOT", "ATCOC3M", "ATCOCURG", "ATCOCPRB",
                                 "ATCOCEXP", "ATCOCWOR", "ATCOCTRY", "OPS_TOT", "ATOPS3M",
                                 "ATOPSURG", "ATOPSPRB", "ATOPSEXP", "ATOPSWOR", "ATOPSTRY",
                                 "ATDRGINJ", "ATINJFRQ", "GENDER", "AGE", "RACE", "EDU", "JOB", "MARTL")

###############################################################################################
################# subset() the data.frames that will be used for Co-use analysis ##############
###############################################################################################


##### Create data sets for the ESEM and invariance testing

(data.ops_coc.dems  <- subset(data.ops_coc.dems, select = c(ATCOC3M, ATCOCURG, ATCOCPRB,
                                                            ATCOCEXP, ATCOCWOR, ATCOCTRY, ATOPS3M, ATOPSURG, ATOPSPRB,
                                                            ATOPSEXP, ATOPSWOR, ATOPSTRY, ATDRGINJ, ATINJFRQ, GENDER,
                                                            AGE, RACE, EDU, JOB, MARTL)))


##### Export the data 

write.table(data.ops_coc.dems, file = "data.ops_coc.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)
# > write.table(data.ops_coc.dems, file = "data.ops_coc.dems.dat", row.names=FALSE, sep = "\t", quote = FALSE)
# >



