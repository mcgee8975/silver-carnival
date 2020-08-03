install.packages("MplusAutomation")
install.packages("multiplex")  #Write a .dat file

library(multiplex)
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

library(MplusAutomation)
# Hallquist, M. N. & Wiley, J. F. (2018). MplusAutomation: An R Package for Facilitating
#     Large-Scale Latent Variable Analyses in Mplus. Structural Equation Modeling, 25,
#     621-638. doi: 10.1080/10705511.2017.1402334.

# -- see citation("MplusAutomation").
# >


#################################### Packages added in this sytax #



##### Loaded
library(tidyverse)
library(psych)
library(Hmisc)
library(lavaan)
library(QuantPsyc)
library(ppcor)
library(exploratory)
library(sem)
library(semTools)
library(semPlot)

##### Set the working directory
setwd("/Volumes/PROCESS/The Dream/University of Cincinnati/Semester 17 - Spring 2020/Doctoral Internship/Research & Scholarship/Research_Scholarships_Grants/Research Projects & Manuscripts/McGee & Richardson_n.d._LH ESEM manuscript/Submitting documents  copy")


######################################################################
##### Load data
######################################################################

### AMP data set
data.etoh <- read.csv("ASSIST_ETOH_Filtered_06.18.20.csv")
options(scipen = 999)

data.etoh
# > data.etoh
#      PROJID ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY
# 1   9996789       2       2        0        0        0        0        0
# 2   9996118       9       2        3        4        0        0        0
# 3   9994871       7       4        0        0        0        3        0
# .
# .
# .
# 123 9331155       0       0        0        0        0        0        0
# 124 9315032       3       0        0        0        0        0        3
# 125 9312584       6       0        0        0        0        3        3
# [ reached 'max' / getOption("max.print") -- omitted 1704 rows ]
# > 

##################### AMP Data Set ###############################

data.dem <- read.csv("AST_DEM.csv")
options(scipen = 999)

### Convert to dataframe

data.etoh <- data.frame(data.etoh)
# > data.etoh <- data.frame(data.etoh)
# > 

data.dem <- data.frame(data.dem)
# > data.dem <- data.frame(data.dem)
# > 

data.etoh

### Provide colnames to dataframes

colnames(data.etoh) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY")

# > colnames(data.etoh) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY")
# >                         

colnames(data.dem) <- c("PROJID", "GENDER", "AGE", "RACE", "EDU", "JOB", "MARTL")

# > colnames(data.dem) <- c("PROJID", "GENDER", "AGE", "RACE", "EDU", "JOB", "MARTL")
# >

##################### Merge AMP & DEM Data Sets ###############################

data.etoh.dems <- merge.data.frame(data.etoh, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
                                  by.y = c("PROJID"), all = FALSE, all.x = TRUE, all.y = FALSE, sort = TRUE,
                                  suffixes = c(".x", ".y"), incomparables = NULL)
# > data.etoh.dems <- merge.data.frame(data.etoh, data.dem, by = intersect(names(x), names(y)), by.x = c("PROJID"),
#                                      +                                   by.y = c("PROJID"), all = FALSE, all.x = TRUE, all.y = FALSE, sort = TRUE,
#                                      +                                   suffixes = c(".x", ".y"), incomparables = NULL)
# >


data.etoh.dems
#    PROJID ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY GENDER AGE RACE EDU JOB MARTL
# 1   10115      33       6        0        7        8        6        6      2  21    3  15   1     5
# 2   10594      20       3        5        6        6        0        0      2  25    2  18   1     5
# 3   16375       3       3        0        0        0        0        0      2  53    3   9   5     5
# .
# .
# .
# 69 378596       0       0        0        0        0        0        0      2  68    3  13   4     5
# 70 379846       2       2        0        0        0        0        0      1  50    3  12   3     5
# 71 383035       0       0        0        0        0        0        0      2  56    6  15   1     5
# [ reached 'max' / getOption("max.print") -- omitted 1758 rows ]
# >

### Convert to data.frame

data.etoh.dems <- data.frame(data.etoh.dems)
# > data.etoh.dems <- data.frame(data.etoh.dems)
# >
  
### Provide colnames to dataframes

colnames(data.etoh.dems) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB",
                              "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
                              "RACE", "EDU", "JOB", "MARTL")

# > colnames(data.etoh.dems) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB",
#                                 +                               "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
#                                 +                               "RACE", "EDU", "JOB", "MARTL")
# >

##################### DESCRIPTIVES ###############################

psych::describe(data.etoh.dems)

# > psych::describe(data.etoh.dems)
#          vars    n       mean         sd  median    trimmed        mad   min     max   range  skew kurtosis       se
# PROJID      1 1829 4922430.87 2877845.58 4813233 4900345.85 3719444.58 10115 9996789 9986674  0.07    -1.21 67291.57
# ALC_TOT     2 1829       5.72       7.48       3       4.08       4.45     0      39      39  2.16     4.87     0.17
# ATALC3M     3 1829       1.94       1.84       2       1.77       2.97     0       6       6  0.47    -0.73     0.04
# ATALCURG    4 1829       1.23       2.05       0       0.84       0.00     0       6       6  1.24    -0.10     0.05
# ATALCPRB    5 1829       0.45       1.51       0       0.00       0.00     0       7       7  3.22     8.89     0.04
# ATALCEXP    6 1829       0.41       1.57       0       0.00       0.00     0       8       8  3.66    12.04     0.04
# ATALCWOR    7 1829       0.92       1.81       0       0.49       0.00     0       6       6  1.81     2.04     0.04
# ATALCTRY    8 1829       0.76       1.71       0       0.33       0.00     0       6       6  2.13     3.32     0.04
# GENDER      9 1829       1.67       3.21       2       1.58       0.00     1      98      97 29.09   865.67     0.08
# AGE        10 1829      45.87      14.79      48      45.87      16.31    18      94      76 -0.04    -0.66     0.35
# RACE       11 1829       2.84       1.17       3       2.67       0.00     1       7       6  1.32     2.15     0.03
# EDU        12 1829      14.33       3.55      15      14.45       2.97     0      97      97  6.60   158.93     0.08
# JOB        13 1829       5.13      13.81       3       3.05       2.97     1      99      98  6.51    41.35     0.32
# MARTL      14 1829       3.65       2.80       4       3.70       1.48     1      99      98 21.50   734.04     0.07
# > 

##################### ETOH scoring ###############################
  
# ETOH
# Q2b + Q3b + Q4b + Q5b + Q6b + Q7b


##################### SEM ASSUMPTIONS ###############################

### MULTIVARIATE ASSUMPTIONS


### Correlation table w/ sig. below

rcorr(as.matrix(data.etoh.dems[ , c("ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB",
                                    "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
                                    "RACE", "EDU", "JOB", "MARTL")]))

# > rcorr(as.matrix(data.etoh.dems[ , c("ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB",
#                                       +                                     "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
#                                       +                                     "RACE", "EDU", "JOB", "MARTL")]))

#          ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY GENDER   AGE  RACE   EDU   JOB MARTL
# ALC_TOT     1.00    0.68     0.77     0.72     0.74     0.69     0.68  -0.01 -0.03  0.00  0.02  0.00 -0.02
# ATALC3M     0.68    1.00     0.64     0.37     0.35     0.24     0.21   0.02 -0.03 -0.02  0.04  0.00  0.00
# ATALCURG    0.77    0.64     1.00     0.42     0.42     0.35     0.33  -0.02 -0.01 -0.01  0.01  0.00 -0.02
# ATALCPRB    0.72    0.37     0.42     1.00     0.63     0.38     0.39  -0.01 -0.03  0.01  0.01  0.00  0.00
# ATALCEXP    0.74    0.35     0.42     0.63     1.00     0.40     0.45  -0.01 -0.03  0.01 -0.01  0.00 -0.02
# ATALCWOR    0.69    0.24     0.35     0.38     0.40     1.00     0.55  -0.02 -0.01  0.01  0.02 -0.02 -0.01
# ATALCTRY    0.68    0.21     0.33     0.39     0.45     0.55     1.00  -0.01 -0.03  0.00  0.02  0.02 -0.03
# GENDER     -0.01    0.02    -0.02    -0.01    -0.01    -0.02    -0.01   1.00 -0.04 -0.02  0.04  0.00  0.00
# AGE        -0.03   -0.03    -0.01    -0.03    -0.03    -0.01    -0.03  -0.04  1.00  0.00 -0.06  0.02 -0.20
# RACE        0.00   -0.02    -0.01     0.01     0.01     0.01     0.00  -0.02  0.00  1.00 -0.01  0.01  0.01
# EDU         0.02    0.04     0.01     0.01    -0.01     0.02     0.02   0.04 -0.06 -0.01  1.00 -0.05 -0.04
# JOB         0.00    0.00     0.00     0.00     0.00    -0.02     0.02   0.00  0.02  0.01 -0.05  1.00 -0.01
# MARTL      -0.02    0.00    -0.02     0.00    -0.02    -0.01    -0.03   0.00 -0.20  0.01 -0.04 -0.01  1.00
# 
# n= 1829 
# 
# 
# P
#         ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY GENDER AGE    RACE   EDU    JOB    MARTL 
# ALC_TOT          0.0000  0.0000   0.0000   0.0000   0.0000   0.0000   0.6733 0.1756 0.9214 0.3791 0.9804 0.3946
# ATALC3M  0.0000          0.0000   0.0000   0.0000   0.0000   0.0000   0.3619 0.2363 0.4300 0.0840 0.8874 0.8743
# ATALCURG 0.0000  0.0000           0.0000   0.0000   0.0000   0.0000   0.5063 0.7828 0.6099 0.7315 0.9566 0.3874
# ATALCPRB 0.0000  0.0000  0.0000            0.0000   0.0000   0.0000   0.7292 0.1488 0.8159 0.8248 0.9752 0.9627
# ATALCEXP 0.0000  0.0000  0.0000   0.0000            0.0000   0.0000   0.6719 0.1764 0.6611 0.7731 0.9211 0.4395
# ATALCWOR 0.0000  0.0000  0.0000   0.0000   0.0000            0.0000   0.4863 0.5873 0.5620 0.4392 0.4868 0.7316
# ATALCTRY 0.0000  0.0000  0.0000   0.0000   0.0000   0.0000            0.5497 0.2195 0.8468 0.4084 0.4869 0.1592
# GENDER   0.6733  0.3619  0.5063   0.7292   0.6719   0.4863   0.5497          0.0821 0.5117 0.1277 0.8532 0.8378
# AGE      0.1756  0.2363  0.7828   0.1488   0.1764   0.5873   0.2195   0.0821        0.9228 0.0127 0.3407 0.0000
# RACE     0.9214  0.4300  0.6099   0.8159   0.6611   0.5620   0.8468   0.5117 0.9228        0.6254 0.6114 0.7530
# EDU      0.3791  0.0840  0.7315   0.8248   0.7731   0.4392   0.4084   0.1277 0.0127 0.6254        0.0260 0.0939
# JOB      0.9804  0.8874  0.9566   0.9752   0.9211   0.4868   0.4869   0.8532 0.3407 0.6114 0.0260        0.5965
# MARTL    0.3946  0.8743  0.3874   0.9627   0.4395   0.7316   0.1592   0.8378 0.0000 0.7530 0.0939 0.5965       
# > 

### Get some frequency stats

table(data.etoh.dems$RACE)

# > table(data.etoh.dems$RACE)
# 
#   1   2   3   4   5   6   7 
# 140 527 975   8  35 140   4 
# >

table(data.etoh.dems$GENDER)
# > table(data.etoh.dems$GENDER)
# 
# 1    2   97   98 
# 794 1033    1    1 
# >

boxplot(data.etoh.dems[ ,c("GENDER")])
# > boxplot(data.etoh.dems[ ,c("GENDER")])
# >

summary(data.etoh.dems)

### Subset the data to remove outliers

data.etoh.dems.02 <- subset(data.etoh.dems,GENDER<=2 & RACE<=6 & EDU<=21 & MARTL<=6)

psych::describe(data.etoh.dems.02)

# > psych::describe(data.etoh.dems.02)
#          vars    n       mean         sd  median    trimmed        mad   min     max   range  skew
# PROJID      1 1821 4924343.80 2878383.90 4813233 4902339.95 3714620.20 10115 9996789 9986674  0.07
# ALC_TOT     2 1821       5.73       7.49       3       4.09       4.45     0      39      39  2.16
# ATALC3M     3 1821       1.94       1.84       2       1.76       2.97     0       6       6  0.47
# ATALCURG    4 1821       1.24       2.05       0       0.85       0.00     0       6       6  1.24
# ATALCPRB    5 1821       0.45       1.52       0       0.00       0.00     0       7       7  3.21
# ATALCEXP    6 1821       0.42       1.57       0       0.00       0.00     0       8       8  3.65
# ATALCWOR    7 1821       0.92       1.81       0       0.49       0.00     0       6       6  1.81
# ATALCTRY    8 1821       0.77       1.71       0       0.33       0.00     0       6       6  2.12
# GENDER      9 1821       1.57       0.50       2       1.58       0.00     1       2       1 -0.26
# AGE        10 1821      45.89      14.79      48      45.89      16.31    18      94      76 -0.04
# RACE       11 1821       2.83       1.16       3       2.67       0.00     1       6       5  1.30
# EDU        12 1821      14.28       2.98      15      14.44       2.97     0      21      21 -0.44
# JOB        13 1821       5.14      13.84       3       3.05       2.97     1      99      98  6.50
# MARTL      14 1821       3.60       1.69       4       3.70       1.48     1       6       5 -0.49

#          kurtosis       se
# PROJID      -1.21 67451.83
# ALC_TOT      4.83     0.18
# ATALC3M     -0.73     0.04
# ATALCURG    -0.12     0.05
# ATALCPRB     8.83     0.04
# ATALCEXP    11.97     0.04
# ATALCWOR     2.02     0.04
# ATALCTRY     3.29     0.04
# GENDER      -1.93     0.01
# AGE         -0.66     0.35
# RACE         2.13     0.03
# EDU          0.31     0.07
# JOB         41.16     0.32
# MARTL       -1.33     0.04
# > 
  
### Correlation table w/ sig. below of subsetted data 

rcorr(as.matrix(data.etoh.dems.02[ , c("ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB",
                                    "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
                                    "RACE", "EDU", "JOB", "MARTL")]))
  
# > rcorr(as.matrix(data.etoh.dems.02[ , c("ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB",
#                                          +                                     "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
#                                          +                                     "RACE", "EDU", "JOB", "MARTL")]))
#          ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY GENDER   AGE  RACE   EDU
# ALC_TOT     1.00    0.68     0.77     0.72     0.74     0.69     0.68   0.01 -0.03  0.00  0.03
# ATALC3M     0.68    1.00     0.64     0.38     0.35     0.24     0.21   0.02 -0.03 -0.02  0.04
# ATALCURG    0.77    0.64     1.00     0.42     0.42     0.35     0.33   0.03 -0.01 -0.01  0.02
# ATALCPRB    0.72    0.38     0.42     1.00     0.63     0.38     0.39   0.01 -0.03  0.01  0.01
# ATALCEXP    0.74    0.35     0.42     0.63     1.00     0.40     0.45  -0.01 -0.03  0.01  0.00
# ATALCWOR    0.69    0.24     0.35     0.38     0.40     1.00     0.55   0.00 -0.01  0.01  0.03
# ATALCTRY    0.68    0.21     0.33     0.39     0.45     0.55     1.00   0.00 -0.03  0.00  0.03
# GENDER      0.01    0.02     0.03     0.01    -0.01     0.00     0.00   1.00 -0.17 -0.04  0.07
# AGE        -0.03   -0.03    -0.01    -0.03    -0.03    -0.01    -0.03  -0.17  1.00  0.00 -0.09
# RACE        0.00   -0.02    -0.01     0.01     0.01     0.01     0.00  -0.04  0.00  1.00 -0.05
# EDU         0.03    0.04     0.02     0.01     0.00     0.03     0.03   0.07 -0.09 -0.05  1.00
# JOB         0.00    0.00     0.00     0.00     0.00    -0.02     0.02   0.05  0.02  0.01 -0.06
# MARTL      -0.01    0.03    -0.02     0.01    -0.02     0.00    -0.04  -0.06 -0.27  0.06 -0.04
#            JOB MARTL
# ALC_TOT   0.00 -0.01
# ATALC3M   0.00  0.03
# ATALCURG  0.00 -0.02
# ATALCPRB  0.00  0.01
# ATALCEXP  0.00 -0.02
# ATALCWOR -0.02  0.00
# ATALCTRY  0.02 -0.04
# GENDER    0.05 -0.06
# AGE       0.02 -0.27
# RACE      0.01  0.06
# EDU      -0.06 -0.04
# JOB       1.00 -0.02
# MARTL    -0.02  1.00
# 
# n= 1821 
# 
# 
# P
#         ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY GENDER AGE    RACE   EDU   
# ALC_TOT          0.0000  0.0000   0.0000   0.0000   0.0000   0.0000   0.5405 0.1722 0.9931 0.1853
# ATALC3M  0.0000          0.0000   0.0000   0.0000   0.0000   0.0000   0.4111 0.2436 0.3314 0.1033
# ATALCURG 0.0000  0.0000           0.0000   0.0000   0.0000   0.0000   0.2491 0.7608 0.7499 0.3839
# ATALCPRB 0.0000  0.0000  0.0000            0.0000   0.0000   0.0000   0.6515 0.1448 0.7383 0.6231
# ATALCEXP 0.0000  0.0000  0.0000   0.0000            0.0000   0.0000   0.7331 0.1724 0.5948 0.8880
# ATALCWOR 0.0000  0.0000  0.0000   0.0000   0.0000            0.0000   0.8950 0.5955 0.5378 0.1972
# ATALCTRY 0.0000  0.0000  0.0000   0.0000   0.0000   0.0000            0.8525 0.2114 0.9617 0.1837
# GENDER   0.5405  0.4111  0.2491   0.6515   0.7331   0.8950   0.8525          0.0000 0.1113 0.0019
# AGE      0.1722  0.2436  0.7608   0.1448   0.1724   0.5955   0.2114   0.0000        0.9935 0.0003
# RACE     0.9931  0.3314  0.7499   0.7383   0.5948   0.5378   0.9617   0.1113 0.9935        0.0278
# EDU      0.1853  0.1033  0.3839   0.6231   0.8880   0.1972   0.1837   0.0019 0.0003 0.0278       
# JOB      0.9843  0.8840  0.9509   0.9725   0.9237   0.4838   0.4910   0.0520 0.3367 0.5750 0.0087
# MARTL    0.6976  0.2479  0.5170   0.7571   0.3499   0.8796   0.0804   0.0111 0.0000 0.0080 0.1076
#             JOB    MARTL 
# ALC_TOT  0.9843 0.6976
# ATALC3M  0.8840 0.2479
# ATALCURG 0.9509 0.5170
# ATALCPRB 0.9725 0.7571
# ATALCEXP 0.9237 0.3499
# ATALCWOR 0.4838 0.8796
# ATALCTRY 0.4910 0.0804
# GENDER   0.0520 0.0111
# AGE      0.3367 0.0000
# RACE     0.5750 0.0080
# EDU      0.0087 0.1076
# JOB             0.2930
# MARTL    0.2930       
# >


##### Mutate the ETOH item 6 (ATALCWOR) & item 7 (ATALCTRY) to categroical items

view(data.etoh.dems.02)

table(data.etoh.dems.02$ATALCWOR)
# > table(data.etoh.dems.02$ATALCWOR)
# 0    3    6 
# 1402  280  139 
# >

table(data.etoh.dems.02$ATALCTRY)
# > table(data.etoh.dems.02$ATALCTRY)
# 
# 0    3    6 
# 1478  221  122 
# >

### Use mutate() to create a new variable ATALCWOR.3M = ALC Worry past 3 months
data.etoh.dems.03 <- mutate(data.etoh.dems.02, ATALCWOR.3M = ATALCWOR)

### Use mutate() to create a new variable ATALCWOR.N3M = ALC Worry not past 3 months
data.etoh.dems.04 <- mutate(data.etoh.dems.03, ATALCWOR.N3M = ATALCWOR)

### Use mutate() to create a new variable ATALCTRY.3M = ALC Try to stop in past 3 months
data.etoh.dems.05 <- mutate(data.etoh.dems.04, ATALCTRY.3M = ATALCTRY)

### Use mutate() to create a new variable ATALCTRY.N3M = ALC Worry not past 3 months
data.etoh.dems.06 <- mutate(data.etoh.dems.05, ATALCTRY.N3M = ATALCTRY)

### Look the new data set over
head(data.etoh.dems.06)
# > head(data.etoh.dems.06)
# PROJID ALC_TOT ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY GENDER AGE RACE EDU JOB
# 1  10115      33       6        0        7        8        6        6      2  21    3  15   1
# 2  10594      20       3        5        6        6        0        0      2  25    2  18   1
# 3  16375       3       3        0        0        0        0        0      2  53    3   9   5
# 4  16534       6       0        0        0        0        3        3      2  59    6  17   5
# 5  35098       0       0        0        0        0        0        0      2  50    3  17   5
# 6  37119      22       4        5        5        5        3        0      2  25    3  13   3
# MARTL ATALCWOR.3M ATALCWOR.N3M ATALCTRY.3M ATALCTRY.N3M
# 1     5           6            6           6            6
# 2     5           0            0           0            0
# 3     5           0            0           0            0
# 4     4           3            3           3            3
# 5     1           0            0           0            0
# 6     5           3            3           0            0
# >

colnames(data.etoh.dems.06) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB", 
                                 "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
                                 "RACE", "EDU", "JOB", "MARTL", "ATALCWOR.3M",
                                 "ATALCWOR.N3M", "ATALCTRY.3M", "ATALCTRY.N3M")

# > colnames(data.etoh.dems.06) <- c("PROJID", "ALC_TOT", "ATALC3M", "ATALCURG", "ATALCPRB", 
# +                                  "ATALCEXP", "ATALCWOR", "ATALCTRY", "GENDER", "AGE",
# +                                  "RACE", "EDU", "JOB", "MARTL", "ATALCWOR.3M",
# +                                  "ATALCWOR.N3M", "ATALCTRY.3M", "ATALCTRY.N3M")
# >
  
table(data.etoh.dems.06$GENDER)

data.frame(data.etoh.dems.06)
#> data.frame(data.etoh.dems.06)

###### Assign levels for each of the items to the new "data" data.frame 

# Note: Dummy coding was based on study protocal and CTN0059-CRF_0.pdf survey.

### GENDER 
table(data.etoh.dems.06$GENDER)
# 
# 1    2 
# 792 1029 
# >

levels.GENDER = c("Male", "Female")

### RACE
table(data.etoh.dems.06$RACE)
# > table(data.etoh.dems.06$RACE)
# 
# 1   2   3   4   5   6 
# 139 526 974   8  34 140 
# > 

levels.RACE = c("Latinx", "White", "AA", "NA/I", "Asian", "MultiR")

### EDU
table(data.etoh.dems.06$EDU)
# > table(data.etoh.dems.06$EDU)
# 
# 0   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21 
# 1   1   2   6  10   9  44  52  77 115  33 415 115 393 105  98 246  74  14  11 
# >

levels.EDU = c("5thgrade", "6thgrade", "7thgrade", "8thgrade", "9thgrade", 
               "10thgrade", "11thgrade", "12thgrade", "HSgrad", "GED",
               "Somecolege", "Asociate", "AsociateAP", "Bachelor", "Master",
               "Professional", "Doctoral")


#################### Control for employment status
### JOB
# table(data.etoh.dems.06$JOB)
# # > table(data.etoh.dems.06$JOB)
# # 
# # 1   2   3   4   5   6   7  99 
# # 650  46 349 160 413  58 107  38 
# # > 
# 
# levels.JOB = c("working", "lookingforwork", "retired", "Keepinghouse", "Student", "ORwhat", "Other")

################

### ATALCWOR.3M & ATALCWOR.N3M

table(data.etoh.dems.06$ATALCWOR.3M)
# > table(data.etoh.dems.06$ATALCWOR.3M)
# 
# 0    3    6 
# 1402  280  139 
# >

levels.ATALCWOR.3M = c("1", "0", "2")

levels.ATALCWOR.N3M = c("1", "2", "0")

### ATALCTRY.3M  & ATALCTRY.N3M

table(data.etoh.dems.06$ATALCTRY.3M)
# > table(data.etoh.dems.06$ATALCTRY.3M)
# 
# 0    3    6 
# 1478  221  122 
# >

levels.ATALCTRY.3M = c("1", "0", "2")

levels.ATALCTRY.N3M = c("1", "2", "0")

## assign individual levels

levels.2to5 = c("Never", "Once or Twice", 
                "Monthy", "Weekly", "Daily or almost daily")

levels.6to7 = c("No, never", "Yes, in the past 3 months", 
                "Yes, but not in the past 3 months")

psych::describe(data.etoh.dems.06)

data.etoh.dems.06 <- data.frame(data.etoh.dems.06,
                   ATALC3M = sample(levels.2to5, 1821, replace = T), 
                   ATALCURG = sample(levels.2to5, 1821, replace = T), 
                   ATALCPRB = sample(levels.2to5, 1821, replace = T),
                   ATALCEXP = sample(levels.2to5, 1821, replace = T),
                   ATALCWOR = sample(levels.6to7, 1821, replace = T),
                   ATALCTRY = sample(levels.6to7, 1821, replace = T),
                     GENDER = sample(levels.GENDER, 1821, replace = T),
                       RACE = sample(levels.RACE, 1821, replace = T),
                        EDU = sample(levels.EDU, 1821, replace = T),
                ATALCWOR.3M = sample(levels.ATALCWOR.3M, 1821, replace = T),
               ATALCWOR.N3M = sample(levels.ATALCWOR.N3M, 1821, replace = T),
                ATALCTRY.3M = sample(levels.ATALCTRY.3M, 1821, replace = T),
               ATALCTRY.N3M = sample(levels.ATALCTRY.N3M, 1821, replace = T))

# > data.etoh.dems.06 <- data.frame(data.etoh.dems.06,
#                                   +                    ATALC3M = sample(levels.2to5, 1821, replace = T), 
#                                   +                    ATALCURG = sample(levels.2to5, 1821, replace = T), 
#                                   +                    ATALCPRB = sample(levels.2to5, 1821, replace = T),
#                                   +                    ATALCEXP = sample(levels.2to5, 1821, replace = T),
#                                   +                    ATALCWOR = sample(levels.6to7, 1821, replace = T),
#                                   +                    ATALCTRY = sample(levels.6to7, 1821, replace = T),
#                                   +                      GENDER = sample(levels.GENDER, 1821, replace = T),
#                                   +                        RACE = sample(levels.RACE, 1821, replace = T),
#                                   +                         EDU = sample(levels.EDU, 1821, replace = T),
#                                   +                 ATALCWOR.3M = sample(levels.ATALCWOR.3M, 1821, replace = T),
#                                   +                ATALCWOR.N3M = sample(levels.ATALCWOR.N3M, 1821, replace = T),
#                                   +                 ATALCTRY.3M = sample(levels.ATALCTRY.3M, 1821, replace = T),
#                                   +                ATALCTRY.N3M = sample(levels.ATALCTRY.N3M, 1821, replace = T))
# >

##################################################################################################
##################################################################################################
##################################################################################################

data.etoh.dems.07 <- subset.data.frame(data.etoh.dems.06, 
                                       select = c(ATALC3M, ATALCURG, ATALCPRB, 
                                                  ATALCEXP, ATALCWOR, ATALCTRY, 
                                                  GENDER, AGE, RACE, EDU, JOB, MARTL))

       
ls(data.etoh.dems.07)
# > ls(data.etoh.dems.07)
# [1] "AGE"      "ATALC3M"  "ATALCEXP" "ATALCPRB" "ATALCTRY" "ATALCURG" "ATALCWOR" "EDU"      "GENDER"  
# [10] "JOB"      "MARTL"    "RACE"    
# >


summary(data.etoh.dems.07)
# > summary(data.etoh.dems.07)

#       ATALC3M         ATALCURG        ATALCPRB         ATALCEXP         ATALCWOR         ATALCTRY     
# Min.   :0.000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :2.000   Median :0.000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
# Mean   :1.941   Mean   :1.236   Mean   :0.4514   Mean   :0.4163   Mean   :0.9193   Mean   :0.7661  
# 3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
# Max.   :6.000   Max.   :6.000   Max.   :7.0000   Max.   :8.0000   Max.   :6.0000   Max.   :6.0000  

#        GENDER           AGE             RACE            EDU             JOB             MARTL      
# Min.   :1.000   Min.   :18.00   Min.   :1.000   Min.   : 0.00   Min.   : 1.000   Min.   :1.000  
# 1st Qu.:1.000   1st Qu.:33.00   1st Qu.:2.000   1st Qu.:13.00   1st Qu.: 1.000   1st Qu.:2.000  
# Median :2.000   Median :48.00   Median :3.000   Median :15.00   Median : 3.000   Median :4.000  
# Mean   :1.565   Mean   :45.89   Mean   :2.831   Mean   :14.28   Mean   : 5.136   Mean   :3.597  
# 3rd Qu.:2.000   3rd Qu.:57.00   3rd Qu.:3.000   3rd Qu.:16.00   3rd Qu.: 5.000   3rd Qu.:5.000  
# Max.   :2.000   Max.   :94.00   Max.   :6.000   Max.   :21.00   Max.   :99.000   Max.   :6.000  
# >
##################################################################################################
##################################################################################################
##################################################################################################

############## ESEM model run in MPlus and copy & pasted into R

# Mplus VERSION 8.4 (Mac)
# MUTHEN & MUTHEN
# 07/26/2020   3:18 AM
# 
# INPUT INSTRUCTIONS
# 
# TITLE: ESEM of ASSIST ETOH Model
# 
# DATA: FILE = data.etoh.dems.07.dat;
# 
# 
# !Old location sequence
# !c:\Users\ndmcgee\Desktop\data.etoh.dems.07.dat;
# 
# 
# 
# VARIABLE: NAMES = ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY;
# 
# 
# CATEGORICAL = ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY;
# 
# missing are all (-999);
# 
# ANALYSIS:
#   TYPE = GENERAL;
# ESTIMATOR = WLSMV;
# ITERATIONS = 10000;
# CONVERGENCE = 0.00005;
# 
# MODEL:
#   f1 f2 by ATALC3M ATALCURG ATALCPRB ATALCEXP ATALCWOR ATALCTRY (*1);
# 
# 
# 
# OUTPUT: STANDARDIZED MODINDICES (ALL);
# 
# 
# 
# 
# INPUT READING TERMINATED NORMALLY
# 
# 
# 
# ESEM of ASSIST ETOH Model
# 
# SUMMARY OF ANALYSIS
# 
# Number of groups                                                 1
# Number of observations                                        1821
# 
# Number of dependent variables                                    6
# Number of independent variables                                  0
# Number of continuous latent variables                            2
# 
# Observed dependent variables
# 
# Binary and ordered categorical (ordinal)
# ATALC3M     ATALCURG    ATALCPRB    ATALCEXP    ATALCWOR    ATALCTRY
# 
# Continuous latent variables
# 
# EFA factors
# *1:   F1          F2
# 
# 
# Estimator                                                    WLSMV
# Rotation                                                    GEOMIN
# Row standardization                                    CORRELATION
# Type of rotation                                           OBLIQUE
# Epsilon value                                               Varies
# Maximum number of iterations                                 10000
# Convergence criterion                                    0.500D-04
# Maximum number of steepest descent iterations                   20
# Maximum number of iterations for H1                           2000
# Convergence criterion for H1                             0.100D-03
# Optimization Specifications for the Exploratory Factor Analysis
# Rotation Algorithm
# Number of random starts                                       30
# Maximum number of iterations                               10000
# Derivative convergence criterion                       0.100D-04
# Parameterization                                             DELTA
# Link                                                        PROBIT
# 
# Input data file(s)
# data.etoh.dems.07.dat
# 
# Input data format  FREE
# 
# 
# SUMMARY OF DATA
# 
# Number of missing data patterns             1
# 
# 
# COVARIANCE COVERAGE OF DATA
# 
# Minimum covariance coverage value   0.100
# 
# 
# PROPORTION OF DATA PRESENT
# 
# 
# Covariance Coverage
# ATALC3M       ATALCURG      ATALCPRB      ATALCEXP      ATALCWOR
# ________      ________      ________      ________      ________
# ATALC3M        1.000
# ATALCURG       1.000         1.000
# ATALCPRB       1.000         1.000         1.000
# ATALCEXP       1.000         1.000         1.000         1.000
# ATALCWOR       1.000         1.000         1.000         1.000         1.000
# ATALCTRY       1.000         1.000         1.000         1.000         1.000
# 
# 
# Covariance Coverage
# ATALCTRY
# ________
# ATALCTRY       1.000
# 
# 
# UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES
# 
# ATALC3M
# Category 1    0.396          721.000
# Category 2    0.215          392.000
# Category 3    0.175          318.000
# Category 4    0.149          272.000
# Category 5    0.065          118.000
# ATALCURG
# Category 1    0.715         1302.000
# Category 2    0.094          172.000
# Category 3    0.059          108.000
# Category 4    0.072          131.000
# Category 5    0.059          108.000
# ATALCPRB
# Category 1    0.915         1667.000
# Category 2    0.023           42.000
# Category 3    0.026           47.000
# Category 4    0.020           36.000
# Category 5    0.016           29.000
# ATALCEXP
# Category 1    0.932         1697.000
# Category 2    0.030           55.000
# Category 3    0.012           22.000
# Category 4    0.014           25.000
# Category 5    0.012           22.000
# ATALCWOR
# Category 1    0.770         1402.000
# Category 2    0.154          280.000
# Category 3    0.076          139.000
# ATALCTRY
# Category 1    0.812         1478.000
# Category 2    0.121          221.000
# Category 3    0.067          122.000
# 
# 
# 
# THE MODEL ESTIMATION TERMINATED NORMALLY
# 
# 
# 
# MODEL FIT INFORMATION
# 
# Number of Free Parameters                       31
# 
# Chi-Square Test of Model Fit
# 
# Value                             24.401*
#   Degrees of Freedom                     4
# P-Value                           0.0001
# 
# *   The chi-square value for MLM, MLMV, MLR, ULSMV, WLSM and WLSMV cannot be used
# for chi-square difference testing in the regular way.  MLM, MLR and WLSM
# chi-square difference testing is described on the Mplus website.  MLMV, WLSMV,
# and ULSMV difference testing is done using the DIFFTEST option.
# 
# RMSEA (Root Mean Square Error Of Approximation)
# 
# Estimate                           0.053
# 90 Percent C.I.                    0.034  0.074
# Probability RMSEA <= .05           0.366
# 
# CFI/TLI
# 
# CFI                                0.997
# TLI                                0.990
# 
# Chi-Square Test of Model Fit for the Baseline Model
# 
# Value                           7914.966
# Degrees of Freedom                    15
# P-Value                           0.0000
# 
# SRMR (Standardized Root Mean Square Residual)
# 
# Value                              0.016
# 
# Optimum Function Value for Weighted Least-Squares Estimator
# 
# Value                     0.35370652D-02
# 
# 
# 
# MODEL RESULTS
# 
# Two-Tailed
# Estimate       S.E.  Est./S.E.    P-Value
# 
# F1       BY
# ATALC3M            0.998      0.034     29.448      0.000
# ATALCURG           0.702      0.035     20.291      0.000
# ATALCPRB           0.492      0.034     14.284      0.000
# ATALCEXP           0.485      0.039     12.488      0.000
# ATALCWOR           0.005      0.003      1.743      0.081
# ATALCTRY          -0.074      0.038     -1.927      0.054
# 
# F2       BY
# ATALC3M           -0.009      0.003     -2.687      0.007
# ATALCURG           0.305      0.038      8.129      0.000
# ATALCPRB           0.592      0.034     17.514      0.000
# ATALCEXP           0.661      0.034     19.638      0.000
# ATALCWOR           0.823      0.023     35.111      0.000
# ATALCTRY           0.927      0.031     29.481      0.000
# 
# F2       WITH
# F1                 0.338      0.032     10.443      0.000
# 
# Thresholds
# ATALC3M$1         -0.264      0.030     -8.871      0.000
# ATALC3M$2          0.282      0.030      9.478      0.000
# ATALC3M$3          0.792      0.033     24.019      0.000
# ATALC3M$4          1.516      0.046     33.234      0.000
# ATALCURG$1         0.568      0.031     18.230      0.000
# ATALCURG$2         0.876      0.034     25.871      0.000
# ATALCURG$3         1.121      0.037     30.154      0.000
# ATALCURG$4         1.561      0.047     33.283      0.000
# ATALCPRB$1         1.375      0.042     32.690      0.000
# ATALCPRB$2         1.542      0.046     33.270      0.000
# ATALCPRB$3         1.803      0.055     32.565      0.000
# ATALCPRB$4         2.146      0.074     29.168      0.000
# ATALCEXP$1         1.490      0.045     33.180      0.000
# ATALCEXP$2         1.776      0.054     32.724      0.000
# ATALCEXP$3         1.946      0.062     31.441      0.000
# ATALCEXP$4         2.255      0.081     27.667      0.000
# ATALCWOR$1         0.739      0.032     22.742      0.000
# ATALCWOR$2         1.430      0.043     32.975      0.000
# ATALCTRY$1         0.884      0.034     26.040      0.000
# ATALCTRY$2         1.499      0.045     33.200      0.000
# 
# Variances
# F1                 1.000      0.000    999.000    999.000
# F2                 1.000      0.000    999.000    999.000
# 
# 
# STANDARDIZED MODEL RESULTS
# 
# 
# STDYX Standardization
# 
# Two-Tailed
# Estimate       S.E.  Est./S.E.    P-Value
# 
# F1       BY
# ATALC3M            0.998      0.034     29.448      0.000
# ATALCURG           0.702      0.035     20.291      0.000
# ATALCPRB           0.492      0.034     14.284      0.000
# ATALCEXP           0.485      0.039     12.488      0.000
# ATALCWOR           0.005      0.003      1.743      0.081
# ATALCTRY          -0.074      0.038     -1.927      0.054
# 
# F2       BY
# ATALC3M           -0.009      0.003     -2.687      0.007
# ATALCURG           0.305      0.038      8.129      0.000
# ATALCPRB           0.592      0.034     17.514      0.000
# ATALCEXP           0.661      0.034     19.638      0.000
# ATALCWOR           0.823      0.023     35.111      0.000
# ATALCTRY           0.927      0.031     29.481      0.000
# 
# F2       WITH
# F1                 0.338      0.032     10.443      0.000
# 
# Thresholds
# ATALC3M$1         -0.264      0.030     -8.871      0.000
# ATALC3M$2          0.282      0.030      9.478      0.000
# ATALC3M$3          0.792      0.033     24.019      0.000
# ATALC3M$4          1.516      0.046     33.234      0.000
# ATALCURG$1         0.568      0.031     18.230      0.000
# ATALCURG$2         0.876      0.034     25.871      0.000
# ATALCURG$3         1.121      0.037     30.154      0.000
# ATALCURG$4         1.561      0.047     33.283      0.000
# ATALCPRB$1         1.375      0.042     32.690      0.000
# ATALCPRB$2         1.542      0.046     33.270      0.000
# ATALCPRB$3         1.803      0.055     32.565      0.000
# ATALCPRB$4         2.146      0.074     29.168      0.000
# ATALCEXP$1         1.490      0.045     33.180      0.000
# ATALCEXP$2         1.776      0.054     32.724      0.000
# ATALCEXP$3         1.946      0.062     31.441      0.000
# ATALCEXP$4         2.255      0.081     27.667      0.000
# ATALCWOR$1         0.739      0.032     22.742      0.000
# ATALCWOR$2         1.430      0.043     32.975      0.000
# ATALCTRY$1         0.884      0.034     26.040      0.000
# ATALCTRY$2         1.499      0.045     33.200      0.000
# 
# Variances
# F1                 1.000      0.000    999.000    999.000
# F2                 1.000      0.000    999.000    999.000
# 
# 
# STDY Standardization
# 
# Two-Tailed
# Estimate       S.E.  Est./S.E.    P-Value
# 
# F1       BY
# ATALC3M            0.998      0.034     29.448      0.000
# ATALCURG           0.702      0.035     20.291      0.000
# ATALCPRB           0.492      0.034     14.284      0.000
# ATALCEXP           0.485      0.039     12.488      0.000
# ATALCWOR           0.005      0.003      1.743      0.081
# ATALCTRY          -0.074      0.038     -1.927      0.054
# 
# F2       BY
# ATALC3M           -0.009      0.003     -2.687      0.007
# ATALCURG           0.305      0.038      8.129      0.000
# ATALCPRB           0.592      0.034     17.514      0.000
# ATALCEXP           0.661      0.034     19.638      0.000
# ATALCWOR           0.823      0.023     35.111      0.000
# ATALCTRY           0.927      0.031     29.481      0.000
# 
# F2       WITH
# F1                 0.338      0.032     10.443      0.000
# 
# Thresholds
# ATALC3M$1         -0.264      0.030     -8.871      0.000
# ATALC3M$2          0.282      0.030      9.478      0.000
# ATALC3M$3          0.792      0.033     24.019      0.000
# ATALC3M$4          1.516      0.046     33.234      0.000
# ATALCURG$1         0.568      0.031     18.230      0.000
# ATALCURG$2         0.876      0.034     25.871      0.000
# ATALCURG$3         1.121      0.037     30.154      0.000
# ATALCURG$4         1.561      0.047     33.283      0.000
# ATALCPRB$1         1.375      0.042     32.690      0.000
# ATALCPRB$2         1.542      0.046     33.270      0.000
# ATALCPRB$3         1.803      0.055     32.565      0.000
# ATALCPRB$4         2.146      0.074     29.168      0.000
# ATALCEXP$1         1.490      0.045     33.180      0.000
# ATALCEXP$2         1.776      0.054     32.724      0.000
# ATALCEXP$3         1.946      0.062     31.441      0.000
# ATALCEXP$4         2.255      0.081     27.667      0.000
# ATALCWOR$1         0.739      0.032     22.742      0.000
# ATALCWOR$2         1.430      0.043     32.975      0.000
# ATALCTRY$1         0.884      0.034     26.040      0.000
# ATALCTRY$2         1.499      0.045     33.200      0.000
# 
# Variances
# F1                 1.000      0.000    999.000    999.000
# F2                 1.000      0.000    999.000    999.000
# 
# 
# STD Standardization
# 
# Two-Tailed
# Estimate       S.E.  Est./S.E.    P-Value
# 
# F1       BY
# ATALC3M            0.998      0.034     29.448      0.000
# ATALCURG           0.702      0.035     20.291      0.000
# ATALCPRB           0.492      0.034     14.284      0.000
# ATALCEXP           0.485      0.039     12.488      0.000
# ATALCWOR           0.005      0.003      1.743      0.081
# ATALCTRY          -0.074      0.038     -1.927      0.054
# 
# F2       BY
# ATALC3M           -0.009      0.003     -2.687      0.007
# ATALCURG           0.305      0.038      8.129      0.000
# ATALCPRB           0.592      0.034     17.514      0.000
# ATALCEXP           0.661      0.034     19.638      0.000
# ATALCWOR           0.823      0.023     35.111      0.000
# ATALCTRY           0.927      0.031     29.481      0.000
# 
# F2       WITH
# F1                 0.338      0.032     10.443      0.000
# 
# Thresholds
# ATALC3M$1         -0.264      0.030     -8.871      0.000
# ATALC3M$2          0.282      0.030      9.478      0.000
# ATALC3M$3          0.792      0.033     24.019      0.000
# ATALC3M$4          1.516      0.046     33.234      0.000
# ATALCURG$1         0.568      0.031     18.230      0.000
# ATALCURG$2         0.876      0.034     25.871      0.000
# ATALCURG$3         1.121      0.037     30.154      0.000
# ATALCURG$4         1.561      0.047     33.283      0.000
# ATALCPRB$1         1.375      0.042     32.690      0.000
# ATALCPRB$2         1.542      0.046     33.270      0.000
# ATALCPRB$3         1.803      0.055     32.565      0.000
# ATALCPRB$4         2.146      0.074     29.168      0.000
# ATALCEXP$1         1.490      0.045     33.180      0.000
# ATALCEXP$2         1.776      0.054     32.724      0.000
# ATALCEXP$3         1.946      0.062     31.441      0.000
# ATALCEXP$4         2.255      0.081     27.667      0.000
# ATALCWOR$1         0.739      0.032     22.742      0.000
# ATALCWOR$2         1.430      0.043     32.975      0.000
# ATALCTRY$1         0.884      0.034     26.040      0.000
# ATALCTRY$2         1.499      0.045     33.200      0.000
# 
# Variances
# F1                 1.000      0.000    999.000    999.000
# F2                 1.000      0.000    999.000    999.000
# 
# 
# R-SQUARE
# 
# Observed                                        Two-Tailed   Residual
# Variable        Estimate       S.E.  Est./S.E.    P-Value    Variance
# 
# ATALC3M            0.989      0.069     14.315      0.000      0.011
# ATALCURG           0.731      0.032     23.064      0.000      0.269
# ATALCPRB           0.790      0.032     24.742      0.000      0.210
# ATALCEXP           0.890      0.032     27.771      0.000      0.110
# ATALCWOR           0.680      0.038     17.983      0.000      0.320
# ATALCTRY           0.818      0.043     19.217      0.000      0.182
# 
# 
# QUALITY OF NUMERICAL RESULTS
# 
# Condition Number for the Information Matrix              0.100E-01
# (ratio of smallest to largest eigenvalue)
# 
# 
# MODEL MODIFICATION INDICES
# 
# Minimum M.I. value for printing the modification index    10.000
# 
# M.I.     E.P.C.  Std E.P.C.  StdYX E.P.C.
# 
# ON Statements
# 
# ATALC3M  ON ATALCURG              22.121     0.335      0.335        0.335
# ATALCURG ON ATALC3M               22.105     0.335      0.335        0.335
# ATALCPRB ON ATALCEXP              22.524     0.125      0.125        0.125
# ATALCEXP ON ATALCPRB              22.583     0.125      0.125        0.125
# ATALCWOR ON ATALCTRY              22.036     0.365      0.365        0.365
# ATALCTRY ON ATALCWOR              22.125     0.366      0.366        0.366
# 
# WITH Statements
# 
# ATALCURG WITH ATALC3M             22.119     0.335      0.335        6.209
# ATALCEXP WITH ATALCPRB            22.542     0.125      0.125        0.818
# ATALCTRY WITH ATALCWOR            22.071     0.366      0.366        1.516
# 
# 
# Beginning Time:  03:18:38
# Ending Time:  03:18:39
# Elapsed Time:  00:00:01
# 
# 
# 
# MUTHEN & MUTHEN
# 3463 Stoner Ave.
# Los Angeles, CA  90066
# 
# Tel: (310) 391-9971
# Fax: (310) 391-8971
# Web: www.StatModel.com
# Support: Support@StatModel.com
# 
# Copyright (c) 1998-2019 Muthen & Muthen
# 


##########################################################################################
##########################################################################################
##########################################################################################


############### CFA model of MPlus model confirmed in R

############### MODEL.etoh

AST.etoh.model <- 'ETOH1 =~ NA*ATALC3M + ATALC3M +  ATALCURG + ATALCPRB + ATALCEXP
                   ETOH2 =~ NA*ATALCURG + ATALCURG + ATALCPRB + ATALCEXP +  ATALCWOR +  ATALCTRY
                    
                    ETOH1~~1*ETOH1
                    ETOH2~~1*ETOH2'


############### MODEL.fit

AST.etoh.model.fit <- lavaan::cfa(AST.etoh.model, data.etoh.dems.07, std.lv=TRUE, estimator = "WLSMV", ordered = c("ATALC3M", "ATALCURG", "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY")) 
# > AST.etoh.model.fit <- lavaan::cfa(AST.etoh.model, data.etoh.dems.07, std.lv=TRUE, estimator = "WLSMV", ordered = c("ATALC3M", "ATALCURG", "ATALCPRB", "ATALCEXP", "ATALCWOR", "ATALCTRY")) 
# >

##### Syntax below was run w/ MLR estimator; thus, treating indicators as continuous
# AST.etoh.model.fit.03 <- lavaan::cfa(AST.etoh.model, data.etoh.dems.06, std.lv=TRUE, estimator = "MLR") 


############### MODEL.summary

summary(AST.etoh.model.fit, standardized = TRUE, fit.measures = TRUE)

# > summary(AST.etoh.model.fit, standardized = TRUE, fit.measures = TRUE)
# lavaan 0.6-6 ended normally after 23 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         30
# 
# Number of observations                          1821
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                15.159      28.521
# Degrees of freedom                                 5           5
# P-value (Chi-square)                           0.010       0.000
# Scaling correction factor                                  0.547
# Shift parameter                                            0.815
# simple second-order correction                             
# 
# Model Test Baseline Model:
#   
#   Test statistic                             11851.222    7910.604
# Degrees of freedom                                15          15
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.499
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.999       0.997
# Tucker-Lewis Index (TLI)                       0.997       0.991
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.033       0.051
# 90 Percent confidence interval - lower         0.015       0.034
# 90 Percent confidence interval - upper         0.053       0.070
# P-value RMSEA <= 0.05                          0.911       0.432
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.024       0.024
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M           0.997    0.035   28.194    0.000    0.997    0.997
# ATALCURG          0.709    0.033   21.756    0.000    0.709    0.709
# ATALCPRB          0.512    0.031   16.747    0.000    0.512    0.512
# ATALCEXP          0.509    0.033   15.246    0.000    0.509    0.509
# ETOH2 =~                                                              
#   ATALCURG          0.310    0.035    8.754    0.000    0.310    0.310
# ATALCPRB          0.590    0.032   18.457    0.000    0.590    0.590
# ATALCEXP          0.658    0.032   20.723    0.000    0.658    0.658
# ATALCWOR          0.856    0.021   40.835    0.000    0.856    0.856
# ATALCTRY          0.868    0.020   42.598    0.000    0.868    0.868
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.295    0.028   10.657    0.000    0.295    0.295
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.264    0.030   -8.869    0.000   -0.264   -0.264
# ATALC3M|t2        0.282    0.030    9.476    0.000    0.282    0.282
# ATALC3M|t3        0.792    0.033   24.012    0.000    0.792    0.792
# ATALC3M|t4        1.516    0.046   33.225    0.000    1.516    1.516
# ATALCURG|t1       0.568    0.031   18.225    0.000    0.568    0.568
# ATALCURG|t2       0.876    0.034   25.864    0.000    0.876    0.876
# ATALCURG|t3       1.121    0.037   30.146    0.000    1.121    1.121
# ATALCURG|t4       1.561    0.047   33.273    0.000    1.561    1.561
# ATALCPRB|t1       1.375    0.042   32.681    0.000    1.375    1.375
# ATALCPRB|t2       1.542    0.046   33.261    0.000    1.542    1.542
# ATALCPRB|t3       1.803    0.055   32.556    0.000    1.803    1.803
# ATALCPRB|t4       2.146    0.074   29.160    0.000    2.146    2.146
# ATALCEXP|t1       1.490    0.045   33.171    0.000    1.490    1.490
# ATALCEXP|t2       1.776    0.054   32.715    0.000    1.776    1.776
# ATALCEXP|t3       1.946    0.062   31.432    0.000    1.946    1.946
# ATALCEXP|t4       2.255    0.082   27.659    0.000    2.255    2.255
# ATALCWOR|t1       0.739    0.032   22.736    0.000    0.739    0.739
# ATALCWOR|t2       1.430    0.043   32.966    0.000    1.430    1.430
# ATALCTRY|t1       0.884    0.034   26.033    0.000    0.884    0.884
# ATALCTRY|t2       1.499    0.045   33.191    0.000    1.499    1.499
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.006                               0.006    0.006
# .ATALCURG          0.271                               0.271    0.271
# .ATALCPRB          0.211                               0.211    0.211
# .ATALCEXP          0.110                               0.110    0.110
# .ATALCWOR          0.267                               0.267    0.267
# .ATALCTRY          0.246                               0.246    0.246
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# >

############### MODEL.modification indices

modindices(AST.etoh.model.fit)
# > modindices(AST.etoh.model.fit)
# lhs op      rhs     mi    epc sepc.lv sepc.all sepc.nox
# 53    ETOH1 =~ ATALCWOR  2.450  0.079   0.079    0.079    0.079
# 54    ETOH1 =~ ATALCTRY  2.450 -0.080  -0.080   -0.080   -0.080
# 56  ATALC3M ~~ ATALCURG 12.219  0.338   0.338    8.419    8.419
# 57  ATALC3M ~~ ATALCPRB  2.132 -0.097  -0.097   -2.743   -2.743
# 58  ATALC3M ~~ ATALCEXP  3.380 -0.119  -0.119   -4.662   -4.662
# 59  ATALC3M ~~ ATALCWOR  2.769  0.062   0.062    1.561    1.561
# 60  ATALC3M ~~ ATALCTRY  0.599 -0.029  -0.029   -0.767   -0.767
# 61 ATALCURG ~~ ATALCPRB  3.380 -0.085  -0.085   -0.358   -0.358
# 62 ATALCURG ~~ ATALCEXP  2.132 -0.069  -0.069   -0.398   -0.398
# 63 ATALCURG ~~ ATALCWOR  4.015  0.088   0.088    0.329    0.329
# 64 ATALCURG ~~ ATALCTRY  0.065  0.012   0.012    0.045    0.045
# 65 ATALCPRB ~~ ATALCEXP 12.219  0.124   0.124    0.817    0.817
# 66 ATALCPRB ~~ ATALCWOR  2.746 -0.082  -0.082   -0.346   -0.346
# 67 ATALCPRB ~~ ATALCTRY  1.969 -0.072  -0.072   -0.316   -0.316
# 68 ATALCEXP ~~ ATALCWOR  6.684 -0.133  -0.133   -0.776   -0.776
# 69 ATALCEXP ~~ ATALCTRY  0.102 -0.017  -0.017   -0.101   -0.101
# 70 ATALCWOR ~~ ATALCTRY 12.241  0.370   0.370    1.444    1.444
# >

############### CFA model.02 of MPlus model confirmed in R

############### MODEL.etoh

AST.etoh.model.02 <- 'ETOH1 =~ NA*ATALC3M + ATALC3M +  ATALCURG + ATALCPRB + ATALCEXP
                   ETOH2 =~ NA*ATALCPRB + ATALCPRB + ATALCEXP +  ATALCWOR +  ATALCTRY
                    
                    ETOH1~~1*ETOH1
                    ETOH2~~1*ETOH2'


############### MODEL.fit

AST.etoh.model.fit.02 <- lavaan::cfa(AST.etoh.model.02,
                                     data.etoh.dems.07,
                                     std.lv=TRUE,
                                     estimator = "WLSMV",
                                     ordered = c("ATALC3M", "ATALCURG",
                                                 "ATALCPRB", "ATALCEXP",
                                                 "ATALCWOR", "ATALCTRY")) 

##### Syntax below was run w/ MLR estimator; thus, treating indicators as continuous
# AST.etoh.model.fit.03 <- lavaan::cfa(AST.etoh.model, data.etoh.dems.06, std.lv=TRUE, estimator = "MLR") 


############### MODEL.summary

summary(AST.etoh.model.fit.02, standardized = TRUE, fit.measures = TRUE)
# > summary(AST.etoh.model.fit.02, standardized = TRUE, fit.measures = TRUE)
# lavaan 0.6-6 ended normally after 16 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         29
# 
# Number of observations                          1821
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                49.068      89.038
# Degrees of freedom                                 6           6
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.557
# Shift parameter                                            0.915
# simple second-order correction                             
# 
# Model Test Baseline Model:
#   
#   Test statistic                             11851.222    7910.604
# Degrees of freedom                                15          15
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.499
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.996       0.989
# Tucker-Lewis Index (TLI)                       0.991       0.974
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.063       0.087
# 90 Percent confidence interval - lower         0.047       0.072
# 90 Percent confidence interval - upper         0.080       0.104
# P-value RMSEA <= 0.05                          0.086       0.000
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
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M           0.823    0.017   49.215    0.000    0.823    0.823
# ATALCURG          0.971    0.018   54.840    0.000    0.971    0.971
# ATALCPRB          0.547    0.031   17.916    0.000    0.547    0.547
# ATALCEXP          0.546    0.034   16.242    0.000    0.546    0.546
# ETOH2 =~                                                              
#   ATALCPRB          0.498    0.032   15.598    0.000    0.498    0.498
# ATALCEXP          0.563    0.033   17.090    0.000    0.563    0.563
# ATALCWOR          0.856    0.021   40.415    0.000    0.856    0.856
# ATALCTRY          0.867    0.021   42.194    0.000    0.867    0.867
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.449    0.027   16.874    0.000    0.449    0.449
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.264    0.030   -8.869    0.000   -0.264   -0.264
# ATALC3M|t2        0.282    0.030    9.476    0.000    0.282    0.282
# ATALC3M|t3        0.792    0.033   24.012    0.000    0.792    0.792
# ATALC3M|t4        1.516    0.046   33.225    0.000    1.516    1.516
# ATALCURG|t1       0.568    0.031   18.225    0.000    0.568    0.568
# ATALCURG|t2       0.876    0.034   25.864    0.000    0.876    0.876
# ATALCURG|t3       1.121    0.037   30.146    0.000    1.121    1.121
# ATALCURG|t4       1.561    0.047   33.273    0.000    1.561    1.561
# ATALCPRB|t1       1.375    0.042   32.681    0.000    1.375    1.375
# ATALCPRB|t2       1.542    0.046   33.261    0.000    1.542    1.542
# ATALCPRB|t3       1.803    0.055   32.556    0.000    1.803    1.803
# ATALCPRB|t4       2.146    0.074   29.160    0.000    2.146    2.146
# ATALCEXP|t1       1.490    0.045   33.171    0.000    1.490    1.490
# ATALCEXP|t2       1.776    0.054   32.715    0.000    1.776    1.776
# ATALCEXP|t3       1.946    0.062   31.432    0.000    1.946    1.946
# ATALCEXP|t4       2.255    0.082   27.659    0.000    2.255    2.255
# ATALCWOR|t1       0.739    0.032   22.736    0.000    0.739    0.739
# ATALCWOR|t2       1.430    0.043   32.966    0.000    1.430    1.430
# ATALCTRY|t1       0.884    0.034   26.033    0.000    0.884    0.884
# ATALCTRY|t2       1.499    0.045   33.191    0.000    1.499    1.499
# 
# Variances:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.323                               0.323    0.323
# .ATALCURG          0.057                               0.057    0.057
# .ATALCPRB          0.210                               0.210    0.210
# .ATALCEXP          0.109                               0.109    0.109
# .ATALCWOR          0.266                               0.266    0.266
# .ATALCTRY          0.249                               0.249    0.249
# 
# Scales y*:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# > 


############### MODEL.modification indices

modindices(AST.etoh.model.fit.02)
# > modindices(AST.etoh.model.fit.02)
# lhs op      rhs     mi    epc sepc.lv sepc.all sepc.nox
# 52    ETOH1 =~ ATALCWOR  2.903  0.112   0.112    0.112    0.112
# 53    ETOH1 =~ ATALCTRY  2.903 -0.113  -0.113   -0.113   -0.113
# 54    ETOH2 =~  ATALC3M 37.469 -0.372  -0.372   -0.372   -0.372
# 55    ETOH2 =~ ATALCURG 37.469  0.439   0.439    0.439    0.439
# 56  ATALC3M ~~ ATALCURG 11.471  0.324   0.324    2.385    2.385
# 57  ATALC3M ~~ ATALCPRB  1.914  0.064   0.064    0.244    0.244
# 58  ATALC3M ~~ ATALCEXP  0.263  0.025   0.025    0.131    0.131
# 59  ATALC3M ~~ ATALCWOR  3.253 -0.057  -0.057   -0.194   -0.194
# 60  ATALC3M ~~ ATALCTRY 13.746 -0.121  -0.121   -0.428   -0.428
# 61 ATALCURG ~~ ATALCPRB 17.684 -0.225  -0.225   -2.050   -2.050
# 62 ATALCURG ~~ ATALCEXP 11.629 -0.187  -0.187   -2.363   -2.363
# 63 ATALCURG ~~ ATALCWOR 22.151  0.171   0.171    1.385    1.385
# 64 ATALCURG ~~ ATALCTRY  9.541  0.118   0.118    0.987    0.987
# 65 ATALCPRB ~~ ATALCEXP 11.471  0.121   0.121    0.802    0.802
# 66 ATALCPRB ~~ ATALCWOR  3.008 -0.087  -0.087   -0.368   -0.368
# 67 ATALCPRB ~~ ATALCTRY  2.001 -0.073  -0.073   -0.320   -0.320
# 68 ATALCEXP ~~ ATALCWOR  6.905 -0.137  -0.137   -0.806   -0.806
# 69 ATALCEXP ~~ ATALCTRY  0.061 -0.013  -0.013   -0.079   -0.079
# 70 ATALCWOR ~~ ATALCTRY 11.471  0.321   0.321    1.246    1.246
# >

############### CFA model of MPlus model confirmed in R

############### MODEL.etoh

AST.etoh.model.03 <- 'ETOH1 =~ NA*ATALC3M + ATALC3M +  ATALCURG + ATALCPRB + ATALCEXP
                   ETOH2 =~ NA*ATALCPRB + ATALCPRB + ATALCEXP +  ATALCWOR +  ATALCTRY
                    
                    ETOH1~~1*ETOH1
                    ETOH2~~1*ETOH2
                    ATALCWOR ~~ ATALCTRY'


############### MODEL.fit

AST.etoh.model.fit.03 <- lavaan::cfa(AST.etoh.model.03,
                                     data.etoh.dems.07,
                                     std.lv=TRUE,
                                     estimator = "WLSMV",
                                     ordered = c("ATALC3M", "ATALCURG",
                                                 "ATALCPRB", "ATALCEXP",
                                                 "ATALCWOR", "ATALCTRY")) 

##### Syntax below was run w/ MLR estimator; thus, treating indicators as continuous
# AST.etoh.model.fit.03 <- lavaan::cfa(AST.etoh.model, data.etoh.dems.06, std.lv=TRUE, estimator = "MLR") 


############### MODEL.summary

summary(AST.etoh.model.fit.03, standardized = TRUE, fit.measures = TRUE)
# > summary(AST.etoh.model.fit.03, standardized = TRUE, fit.measures = TRUE)
# lavaan 0.6-6 ended normally after 25 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         30
# 
# Number of observations                          1821
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                37.356      95.150
# Degrees of freedom                                 5           5
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.394
# Shift parameter                                            0.410
# simple second-order correction                             
# 
# Model Test Baseline Model:
#   
#   Test statistic                             11851.222    7910.604
# Degrees of freedom                                15          15
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.499
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.989
# Tucker-Lewis Index (TLI)                       0.992       0.966
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
# RMSEA                                          0.060       0.100
# 90 Percent confidence interval - lower         0.043       0.083
# 90 Percent confidence interval - upper         0.078       0.117
# P-value RMSEA <= 0.05                          0.166       0.000
# 
# Robust RMSEA                                                  NA
# 90 Percent confidence interval - lower                        NA
# 90 Percent confidence interval - upper                        NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.038       0.038
# 
# Parameter Estimates:
#   
#   Standard errors                           Robust.sem
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# 
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M           0.815    0.017   47.462    0.000    0.815    0.815
# ATALCURG          0.988    0.018   53.611    0.000    0.988    0.988
# ATALCPRB          0.395    0.057    6.962    0.000    0.395    0.395
# ATALCEXP          0.359    0.070    5.128    0.000    0.359    0.359
# ETOH2 =~                                                              
#   ATALCPRB          0.615    0.042   14.633    0.000    0.615    0.615
# ATALCEXP          0.727    0.058   12.636    0.000    0.727    0.727
# ATALCWOR          0.721    0.040   18.062    0.000    0.721    0.721
# ATALCTRY          0.728    0.040   18.364    0.000    0.728    0.728
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALCWOR ~~                                                           
#   .ATALCTRY          0.231    0.050    4.647    0.000    0.231    0.485
# ETOH1 ~~                                                              
#   ETOH2             0.554    0.040   14.016    0.000    0.554    0.554
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.264    0.030   -8.869    0.000   -0.264   -0.264
# ATALC3M|t2        0.282    0.030    9.476    0.000    0.282    0.282
# ATALC3M|t3        0.792    0.033   24.012    0.000    0.792    0.792
# ATALC3M|t4        1.516    0.046   33.225    0.000    1.516    1.516
# ATALCURG|t1       0.568    0.031   18.225    0.000    0.568    0.568
# ATALCURG|t2       0.876    0.034   25.864    0.000    0.876    0.876
# ATALCURG|t3       1.121    0.037   30.146    0.000    1.121    1.121
# ATALCURG|t4       1.561    0.047   33.273    0.000    1.561    1.561
# ATALCPRB|t1       1.375    0.042   32.681    0.000    1.375    1.375
# ATALCPRB|t2       1.542    0.046   33.261    0.000    1.542    1.542
# ATALCPRB|t3       1.803    0.055   32.556    0.000    1.803    1.803
# ATALCPRB|t4       2.146    0.074   29.160    0.000    2.146    2.146
# ATALCEXP|t1       1.490    0.045   33.171    0.000    1.490    1.490
# ATALCEXP|t2       1.776    0.054   32.715    0.000    1.776    1.776
# ATALCEXP|t3       1.946    0.062   31.432    0.000    1.946    1.946
# ATALCEXP|t4       2.255    0.082   27.659    0.000    2.255    2.255
# ATALCWOR|t1       0.739    0.032   22.736    0.000    0.739    0.739
# ATALCWOR|t2       1.430    0.043   32.966    0.000    1.430    1.430
# ATALCTRY|t1       0.884    0.034   26.033    0.000    0.884    0.884
# ATALCTRY|t2       1.499    0.045   33.191    0.000    1.499    1.499
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.336                               0.336    0.336
# .ATALCURG          0.024                               0.024    0.024
# .ATALCPRB          0.196                               0.196    0.196
# .ATALCEXP          0.053                               0.053    0.053
# .ATALCWOR          0.480                               0.480    0.480
# .ATALCTRY          0.470                               0.470    0.470
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# > 


############### MODEL.modification indices

modindices(AST.etoh.model.fit.03)


############### MODEL.etoh.model = invariance model


##################### INVARIANCE TESTS

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

data.etoh.dems.07

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AST.etoh.model.inv.GENDER.fit <- lavaan::cfa(AST.etoh.model.02, 
                                                 data.etoh.dems.07, 
                                                 std.lv=TRUE, 
                                                 estimator = "WLSMV", 
                                                 ordered = c("ATALC3M", "ATALCURG", 
                                                             "ATALCPRB", "ATALCEXP",
                                                             "ATALCWOR", "ATALCTRY"), group = "GENDER")                                                                                          

# > AST.etoh.model.cfa.inv.GENDER.fit <- lavaan::cfa(AST.etoh.model.cfa.inv, data.etoh.dems.06, std.lv=TRUE, estimator = "MLR", group = "GENDER")
# >

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.etoh.model.inv.GENDER.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.etoh.model.inv.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 28 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         58
# 
# Number of observations per group:                   
#   2                                             1029
# 1                                              792
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                56.106     100.798
# Degrees of freedom                                12          12
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.568
# Shift parameter for each group:                                 
#   2                                                      1.119
# 1                                                      0.862
# simple second-order correction                             
# Test statistic for each group:
#   2                                           46.048      82.220
# 1                                           10.059      18.578
# 
# Model Test Baseline Model:
#   
#   Test statistic                             12200.814    8087.060
# Degrees of freedom                                30          30
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.511
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.996       0.989
# Tucker-Lewis Index (TLI)                       0.991       0.972
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.064       0.090
# 90 Percent confidence interval - lower         0.047       0.074
# 90 Percent confidence interval - upper         0.081       0.107
# P-value RMSEA <= 0.05                          0.082       0.000
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
# ETOH1 =~                                                              
#   ATALC3M           0.824    0.022   37.798    0.000    0.824    0.824
# ATALCURG          0.975    0.024   41.023    0.000    0.975    0.975
# ATALCPRB          0.536    0.041   12.944    0.000    0.536    0.536
# ATALCEXP          0.546    0.044   12.416    0.000    0.546    0.546
# ETOH2 =~                                                              
#   ATALCPRB          0.523    0.042   12.476    0.000    0.523    0.523
# ATALCEXP          0.573    0.042   13.574    0.000    0.573    0.573
# ATALCWOR          0.849    0.030   28.419    0.000    0.849    0.849
# ATALCTRY          0.859    0.027   31.448    0.000    0.859    0.859
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.462    0.035   13.155    0.000    0.462    0.462
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.278    0.040   -7.002    0.000   -0.278   -0.278
# ATALC3M|t2        0.285    0.040    7.188    0.000    0.285    0.285
# ATALC3M|t3        0.754    0.043   17.366    0.000    0.754    0.754
# ATALC3M|t4        1.476    0.059   24.899    0.000    1.476    1.476
# ATALCURG|t1       0.546    0.041   13.226    0.000    0.546    0.546
# ATALCURG|t2       0.848    0.045   18.988    0.000    0.848    0.848
# ATALCURG|t3       1.059    0.048   21.970    0.000    1.059    1.059
# ATALCURG|t4       1.521    0.061   24.975    0.000    1.521    1.521
# ATALCPRB|t1       1.369    0.056   24.532    0.000    1.369    1.369
# ATALCPRB|t2       1.506    0.060   24.954    0.000    1.506    1.506
# ATALCPRB|t3       1.776    0.072   24.586    0.000    1.776    1.776
# ATALCPRB|t4       2.066    0.091   22.669    0.000    2.066    2.066
# ATALCEXP|t1       1.513    0.061   24.965    0.000    1.513    1.513
# ATALCEXP|t2       1.776    0.072   24.586    0.000    1.776    1.776
# ATALCEXP|t3       1.923    0.081   23.779    0.000    1.923    1.923
# ATALCEXP|t4       2.181    0.101   21.561    0.000    2.181    2.181
# ATALCWOR|t1       0.744    0.043   17.190    0.000    0.744    0.744
# ATALCWOR|t2       1.407    0.057   24.693    0.000    1.407    1.407
# ATALCTRY|t1       0.876    0.045   19.442    0.000    0.876    0.876
# ATALCTRY|t2       1.498    0.060   24.942    0.000    1.498    1.498
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.320                               0.320    0.320
# .ATALCURG          0.050                               0.050    0.050
# .ATALCPRB          0.181                               0.181    0.181
# .ATALCEXP          0.085                               0.085    0.085
# .ATALCWOR          0.279                               0.279    0.279
# .ATALCTRY          0.262                               0.262    0.262
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M           0.818    0.026   31.711    0.000    0.818    0.818
# ATALCURG          0.968    0.025   38.163    0.000    0.968    0.968
# ATALCPRB          0.551    0.046   12.110    0.000    0.551    0.551
# ATALCEXP          0.536    0.050   10.640    0.000    0.536    0.536
# ETOH2 =~                                                              
#   ATALCPRB          0.476    0.049    9.721    0.000    0.476    0.476
# ATALCEXP          0.562    0.050   11.320    0.000    0.562    0.562
# ATALCWOR          0.874    0.028   31.348    0.000    0.874    0.874
# ATALCTRY          0.867    0.030   28.926    0.000    0.867    0.867
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.429    0.040   10.704    0.000    0.429    0.429
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.246    0.045   -5.464    0.000   -0.246   -0.246
# ATALC3M|t2        0.279    0.045    6.172    0.000    0.279    0.279
# ATALC3M|t3        0.843    0.051   16.594    0.000    0.843    0.843
# ATALC3M|t4        1.571    0.072   21.939    0.000    1.571    1.571
# ATALCURG|t1       0.597    0.048   12.550    0.000    0.597    0.597
# ATALCURG|t2       0.913    0.052   17.557    0.000    0.913    0.913
# ATALCURG|t3       1.207    0.059   20.595    0.000    1.207    1.207
# ATALCURG|t4       1.616    0.074   21.923    0.000    1.616    1.616
# ATALCPRB|t1       1.383    0.064   21.578    0.000    1.383    1.383
# ATALCPRB|t2       1.593    0.073   21.936    0.000    1.593    1.593
# ATALCPRB|t3       1.841    0.086   21.300    0.000    1.841    1.841
# ATALCPRB|t4       2.278    0.126   18.009    0.000    2.278    2.278
# ATALCEXP|t1       1.461    0.067   21.814    0.000    1.461    1.461
# ATALCEXP|t2       1.776    0.082   21.569    0.000    1.776    1.776
# ATALCEXP|t3       1.978    0.096   20.522    0.000    1.978    1.978
# ATALCEXP|t4       2.372    0.139   17.056    0.000    2.372    2.372
# ATALCWOR|t1       0.731    0.049   14.871    0.000    0.731    0.731
# ATALCWOR|t2       1.461    0.067   21.814    0.000    1.461    1.461
# ATALCTRY|t1       0.894    0.052   17.303    0.000    0.894    0.894
# ATALCTRY|t2       1.499    0.069   21.884    0.000    1.499    1.499
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.330                               0.330    0.330
# .ATALCURG          0.064                               0.064    0.064
# .ATALCPRB          0.244                               0.244    0.244
# .ATALCEXP          0.138                               0.138    0.138
# .ATALCWOR          0.236                               0.236    0.236
# .ATALCTRY          0.248                               0.248    0.248
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# >


############### Metric invariance

AST.etoh.model.cfa.inv.GENDER.METRIC.fit  <- lavaan::cfa(AST.etoh.model.02, 
                                                         data.etoh.dems.07, 
                                                         std.lv=TRUE, 
                                                         estimator = "WLSMV", 
                                                         ordered = c("ATALC3M", "ATALCURG", 
                                                                     "ATALCPRB", "ATALCEXP",
                                                                     "ATALCWOR", "ATALCTRY"),
                                                         group = "GENDER",
                                                         group.equal=c("loadings"))

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.etoh.model.cfa.inv.GENDER.METRIC.fit , fit.measures= TRUE, standardized=TRUE)

# > summary(AST.etoh.model.cfa.inv.GENDER.METRIC.fit , fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 20 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         58
# Number of equality constraints                     8
# 
# Number of observations per group:                   
#   2                                             1029
# 1                                              792
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                58.070      82.607
# Degrees of freedom                                20          20
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  0.707
# Shift parameter for each group:                                 
#   2                                                      0.244
# 1                                                      0.187
# simple second-order correction                             
# Test statistic for each group:
#   2                                           46.793      66.462
# 1                                           11.277      16.145
# 
# Model Test Baseline Model:
#   
#   Test statistic                             12200.814    8087.060
# Degrees of freedom                                30          30
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.511
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.992
# Tucker-Lewis Index (TLI)                       0.995       0.988
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.046       0.059
# 90 Percent confidence interval - lower         0.032       0.046
# 90 Percent confidence interval - upper         0.060       0.072
# P-value RMSEA <= 0.05                          0.673       0.128
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
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.822    0.017   49.516    0.000    0.822    0.822
# ATALCUR (.p2.)    0.972    0.018   55.468    0.000    0.972    0.972
# ATALCPR (.p3.)    0.545    0.030   17.879    0.000    0.545    0.545
# ATALCEX (.p4.)    0.543    0.033   16.348    0.000    0.543    0.543
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.504    0.032   15.928    0.000    0.504    0.504
# ATALCEX (.p6.)    0.570    0.032   17.576    0.000    0.570    0.570
# ATALCWO (.p7.)    0.860    0.021   41.384    0.000    0.860    0.860
# ATALCTR (.p8.)    0.863    0.020   42.728    0.000    0.863    0.863
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.468    0.034   13.578    0.000    0.468    0.468
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.278    0.040   -7.002    0.000   -0.278   -0.278
# ATALC3M|t2        0.285    0.040    7.188    0.000    0.285    0.285
# ATALC3M|t3        0.754    0.043   17.366    0.000    0.754    0.754
# ATALC3M|t4        1.476    0.059   24.899    0.000    1.476    1.476
# ATALCURG|t1       0.546    0.041   13.226    0.000    0.546    0.546
# ATALCURG|t2       0.848    0.045   18.988    0.000    0.848    0.848
# ATALCURG|t3       1.059    0.048   21.970    0.000    1.059    1.059
# ATALCURG|t4       1.521    0.061   24.975    0.000    1.521    1.521
# ATALCPRB|t1       1.369    0.056   24.532    0.000    1.369    1.369
# ATALCPRB|t2       1.506    0.060   24.954    0.000    1.506    1.506
# ATALCPRB|t3       1.776    0.072   24.586    0.000    1.776    1.776
# ATALCPRB|t4       2.066    0.091   22.669    0.000    2.066    2.066
# ATALCEXP|t1       1.513    0.061   24.965    0.000    1.513    1.513
# ATALCEXP|t2       1.776    0.072   24.586    0.000    1.776    1.776
# ATALCEXP|t3       1.923    0.081   23.779    0.000    1.923    1.923
# ATALCEXP|t4       2.181    0.101   21.561    0.000    2.181    2.181
# ATALCWOR|t1       0.744    0.043   17.190    0.000    0.744    0.744
# ATALCWOR|t2       1.407    0.057   24.693    0.000    1.407    1.407
# ATALCTRY|t1       0.876    0.045   19.442    0.000    0.876    0.876
# ATALCTRY|t2       1.498    0.060   24.942    0.000    1.498    1.498
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.324                               0.324    0.324
# .ATALCURG          0.056                               0.056    0.056
# .ATALCPRB          0.194                               0.194    0.194
# .ATALCEXP          0.091                               0.091    0.091
# .ATALCWOR          0.260                               0.260    0.260
# .ATALCTRY          0.256                               0.256    0.256
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.822    0.017   49.516    0.000    0.822    0.822
# ATALCUR (.p2.)    0.972    0.018   55.468    0.000    0.972    0.972
# ATALCPR (.p3.)    0.545    0.030   17.879    0.000    0.545    0.545
# ATALCEX (.p4.)    0.543    0.033   16.348    0.000    0.543    0.543
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.504    0.032   15.928    0.000    0.504    0.504
# ATALCEX (.p6.)    0.570    0.032   17.576    0.000    0.570    0.570
# ATALCWO (.p7.)    0.860    0.021   41.384    0.000    0.860    0.860
# ATALCTR (.p8.)    0.863    0.020   42.728    0.000    0.863    0.863
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.419    0.040   10.346    0.000    0.419    0.419
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M|t1       -0.246    0.045   -5.464    0.000   -0.246   -0.246
# ATALC3M|t2        0.279    0.045    6.172    0.000    0.279    0.279
# ATALC3M|t3        0.843    0.051   16.594    0.000    0.843    0.843
# ATALC3M|t4        1.571    0.072   21.939    0.000    1.571    1.571
# ATALCURG|t1       0.597    0.048   12.550    0.000    0.597    0.597
# ATALCURG|t2       0.913    0.052   17.557    0.000    0.913    0.913
# ATALCURG|t3       1.207    0.059   20.595    0.000    1.207    1.207
# ATALCURG|t4       1.616    0.074   21.923    0.000    1.616    1.616
# ATALCPRB|t1       1.383    0.064   21.578    0.000    1.383    1.383
# ATALCPRB|t2       1.593    0.073   21.936    0.000    1.593    1.593
# ATALCPRB|t3       1.841    0.086   21.300    0.000    1.841    1.841
# ATALCPRB|t4       2.278    0.126   18.009    0.000    2.278    2.278
# ATALCEXP|t1       1.461    0.067   21.814    0.000    1.461    1.461
# ATALCEXP|t2       1.776    0.082   21.569    0.000    1.776    1.776
# ATALCEXP|t3       1.978    0.096   20.522    0.000    1.978    1.978
# ATALCEXP|t4       2.372    0.139   17.056    0.000    2.372    2.372
# ATALCWOR|t1       0.731    0.049   14.871    0.000    0.731    0.731
# ATALCWOR|t2       1.461    0.067   21.814    0.000    1.461    1.461
# ATALCTRY|t1       0.894    0.052   17.303    0.000    0.894    0.894
# ATALCTRY|t2       1.499    0.069   21.884    0.000    1.499    1.499
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.324                               0.324    0.324
# .ATALCURG          0.056                               0.056    0.056
# .ATALCPRB          0.220                               0.220    0.220
# .ATALCEXP          0.122                               0.122    0.122
# .ATALCWOR          0.260                               0.260    0.260
# .ATALCTRY          0.256                               0.256    0.256
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# >

################# Chi-square difference test between Configural invariance and metric invariance. 

anova(AST.etoh.model.inv.GENDER.fit, AST.etoh.model.cfa.inv.GENDER.METRIC.fit )
# > anova(AST.etoh.model.inv.GENDER.fit, AST.etoh.model.cfa.inv.GENDER.METRIC.fit )
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.etoh.model.inv.GENDER.fit            12         56.106                              
# AST.etoh.model.cfa.inv.GENDER.METRIC.fit 20         58.070     2.7405       8     0.9496
# >
  
################### scalar invariance

AST.etoh.model.cfa.inv.GENDER.SCALAR.fit  <- lavaan::cfa(AST.etoh.model.02, 
                                                         data.etoh.dems.07, 
                                                         std.lv=TRUE, 
                                                         estimator = "WLSMV", 
                                                         ordered = c("ATALC3M", "ATALCURG", 
                                                                     "ATALCPRB", "ATALCEXP",
                                                                     "ATALCWOR", "ATALCTRY"),
                                                         group = "GENDER",
                                                         group.equal=c("loadings", "intercepts"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.etoh.model.cfa.inv.GENDER.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.etoh.model.cfa.inv.GENDER.SCALAR.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 27 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         66
# Number of equality constraints                    28
# 
# Number of observations per group:                   
#   2                                             1029
# 1                                              792
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                63.745     102.084
# Degrees of freedom                                32          32
# P-value (Chi-square)                           0.001       0.000
# Scaling correction factor                                  0.642
# Shift parameter for each group:                                 
#   2                                                      1.561
# 1                                                      1.201
# simple second-order correction                             
# Test statistic for each group:
#   2                                           49.065      78.010
# 1                                           14.680      24.074
# 
# Model Test Baseline Model:
#   
#   Test statistic                             12200.814    8087.060
# Degrees of freedom                                30          30
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.511
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.991
# Tucker-Lewis Index (TLI)                       0.998       0.992
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.033       0.049
# 90 Percent confidence interval - lower         0.021       0.039
# 90 Percent confidence interval - upper         0.045       0.060
# P-value RMSEA <= 0.05                          0.992       0.537
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
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.823    0.019   44.115    0.000    0.823    0.823
# ATALCUR (.p2.)    0.970    0.021   46.848    0.000    0.970    0.970
# ATALCPR (.p3.)    0.545    0.032   17.270    0.000    0.545    0.545
# ATALCEX (.p4.)    0.547    0.034   16.006    0.000    0.547    0.547
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.501    0.032   15.908    0.000    0.501    0.501
# ATALCEX (.p6.)    0.573    0.033   17.439    0.000    0.573    0.573
# ATALCWO (.p7.)    0.851    0.026   32.764    0.000    0.851    0.851
# ATALCTR (.p8.)    0.859    0.024   35.916    0.000    0.859    0.859
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.468    0.035   13.234    0.000    0.468    0.468
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.290    0.037   -7.880    0.000   -0.290   -0.290
# ATALC3M (.12.)    0.257    0.037    6.928    0.000    0.257    0.257
# ATALC3M (.13.)    0.767    0.040   19.018    0.000    0.767    0.767
# ATALC3M (.14.)    1.492    0.053   28.375    0.000    1.492    1.492
# ATALCUR (.15.)    0.536    0.040   13.536    0.000    0.536    0.536
# ATALCUR (.16.)    0.844    0.042   20.245    0.000    0.844    0.844
# ATALCUR (.17.)    1.089    0.044   24.529    0.000    1.089    1.089
# ATALCUR (.18.)    1.528    0.053   28.885    0.000    1.528    1.528
# ATALCPR (.19.)    1.356    0.051   26.336    0.000    1.356    1.356
# ATALCPR (.20.)    1.523    0.055   27.526    0.000    1.523    1.523
# ATALCPR (.21.)    1.784    0.063   28.183    0.000    1.784    1.784
# ATALCPR (.22.)    2.122    0.081   26.326    0.000    2.122    2.122
# ATALCEX (.23.)    1.483    0.056   26.508    0.000    1.483    1.483
# ATALCEX (.24.)    1.771    0.065   27.196    0.000    1.771    1.771
# ATALCEX (.25.)    1.942    0.072   26.891    0.000    1.942    1.942
# ATALCEX (.26.)    2.246    0.090   25.013    0.000    2.246    2.246
# ATALCWO (.27.)    0.730    0.042   17.278    0.000    0.730    0.730
# ATALCWO (.28.)    1.415    0.051   27.851    0.000    1.415    1.415
# ATALCTR (.29.)    0.880    0.044   20.125    0.000    0.880    0.880
# ATALCTR (.30.)    1.492    0.054   27.782    0.000    1.492    1.492
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.323                               0.323    0.323
# .ATALCURG          0.058                               0.058    0.058
# .ATALCPRB          0.196                               0.196    0.196
# .ATALCEXP          0.079                               0.079    0.079
# .ATALCWOR          0.276                               0.276    0.276
# .ATALCTRY          0.262                               0.262    0.262
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.823    0.019   44.115    0.000    0.823    0.820
# ATALCUR (.p2.)    0.970    0.021   46.848    0.000    0.970    0.974
# ATALCPR (.p3.)    0.545    0.032   17.270    0.000    0.545    0.546
# ATALCEX (.p4.)    0.547    0.034   16.006    0.000    0.547    0.537
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.501    0.032   15.908    0.000    0.501    0.503
# ATALCEX (.p6.)    0.573    0.033   17.439    0.000    0.573    0.564
# ATALCWO (.p7.)    0.851    0.026   32.764    0.000    0.851    0.872
# ATALCTR (.p8.)    0.859    0.024   35.916    0.000    0.859    0.868
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.419    0.041   10.211    0.000    0.419    0.419
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1            -0.073    0.060   -1.221    0.222   -0.073   -0.073
# ETOH2            -0.001    0.081   -0.017    0.986   -0.001   -0.001
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.290    0.037   -7.880    0.000   -0.290   -0.290
# ATALC3M (.12.)    0.257    0.037    6.928    0.000    0.257    0.256
# ATALC3M (.13.)    0.767    0.040   19.018    0.000    0.767    0.765
# ATALC3M (.14.)    1.492    0.053   28.375    0.000    1.492    1.488
# ATALCUR (.15.)    0.536    0.040   13.536    0.000    0.536    0.538
# ATALCUR (.16.)    0.844    0.042   20.245    0.000    0.844    0.847
# ATALCUR (.17.)    1.089    0.044   24.529    0.000    1.089    1.092
# ATALCUR (.18.)    1.528    0.053   28.885    0.000    1.528    1.534
# ATALCPR (.19.)    1.356    0.051   26.336    0.000    1.356    1.359
# ATALCPR (.20.)    1.523    0.055   27.526    0.000    1.523    1.527
# ATALCPR (.21.)    1.784    0.063   28.183    0.000    1.784    1.788
# ATALCPR (.22.)    2.122    0.081   26.326    0.000    2.122    2.128
# ATALCEX (.23.)    1.483    0.056   26.508    0.000    1.483    1.458
# ATALCEX (.24.)    1.771    0.065   27.196    0.000    1.771    1.742
# ATALCEX (.25.)    1.942    0.072   26.891    0.000    1.942    1.910
# ATALCEX (.26.)    2.246    0.090   25.013    0.000    2.246    2.208
# ATALCWO (.27.)    0.730    0.042   17.278    0.000    0.730    0.748
# ATALCWO (.28.)    1.415    0.051   27.851    0.000    1.415    1.450
# ATALCTR (.29.)    0.880    0.044   20.125    0.000    0.880    0.888
# ATALCTR (.30.)    1.492    0.054   27.782    0.000    1.492    1.506
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.329                               0.329    0.327
# .ATALCURG          0.051                               0.051    0.052
# .ATALCPRB          0.219                               0.219    0.220
# .ATALCEXP          0.145                               0.145    0.140
# .ATALCWOR          0.228                               0.228    0.240
# .ATALCTRY          0.242                               0.242    0.247
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           0.997    0.026   38.692    0.000    0.997    1.000
# ATALCURG          1.003    0.025   39.871    0.000    1.003    1.000
# ATALCPRB          1.002    0.029   34.786    0.000    1.002    1.000
# ATALCEXP          0.983    0.025   38.887    0.000    0.983    1.000
# ATALCWOR          1.025    0.036   28.672    0.000    1.025    1.000
# ATALCTRY          1.010    0.034   29.848    0.000    1.010    1.000
# 
# >


############################ Test difference

anova(AST.etoh.model.cfa.inv.GENDER.METRIC.fit, AST.etoh.model.cfa.inv.GENDER.SCALAR.fit)

# > anova(AST.etoh.model.cfa.inv.GENDER.METRIC.fit, AST.etoh.model.cfa.inv.GENDER.SCALAR.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.etoh.model.cfa.inv.GENDER.METRIC.fit 20         58.070                              
# AST.etoh.model.cfa.inv.GENDER.SCALAR.fit 32         63.745     15.151      12     0.2333
# >


###### equal factor means specification

AST.etoh.model.cfa.inv.GENDER.MEANS.fit  <- lavaan::cfa(AST.etoh.model.02, 
                                                        data.etoh.dems.07, 
                                                        std.lv=TRUE, 
                                                        estimator = "WLSMV", 
                                                        group = "GENDER",
                                                        ordered = c("ATALC3M", "ATALCURG", 
                                                                    "ATALCPRB", "ATALCEXP",
                                                                    "ATALCWOR", "ATALCTRY"),
                                                        group.equal=c("loadings", "intercepts", "means"))


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.etoh.model.cfa.inv.GENDER.MEANS.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.etoh.model.cfa.inv.GENDER.MEANS.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 25 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         64
# Number of equality constraints                    28
# 
# Number of observations per group:                   
#   2                                             1029
# 1                                              792
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                70.026      77.209
# Degrees of freedom                                34          34
# P-value (Chi-square)                           0.000       0.000
# Scaling correction factor                                  1.015
# Shift parameter for each group:                                 
#   2                                                      4.655
# 1                                                      3.583
# simple second-order correction                             
# Test statistic for each group:
#   2                                           51.593      55.472
# 1                                           18.433      21.738
# 
# Model Test Baseline Model:
#   
#   Test statistic                             12200.814    8087.060
# Degrees of freedom                                30          30
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.511
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.995
# Tucker-Lewis Index (TLI)                       0.997       0.995
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.034       0.037
# 90 Percent confidence interval - lower         0.023       0.026
# 90 Percent confidence interval - upper         0.045       0.048
# P-value RMSEA <= 0.05                          0.991       0.970
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
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.822    0.019   44.185    0.000    0.822    0.822
# ATALCUR (.p2.)    0.961    0.021   45.906    0.000    0.961    0.961
# ATALCPR (.p3.)    0.540    0.031   17.200    0.000    0.540    0.540
# ATALCEX (.p4.)    0.543    0.034   15.940    0.000    0.543    0.543
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.500    0.032   15.851    0.000    0.500    0.500
# ATALCEX (.p6.)    0.572    0.033   17.292    0.000    0.572    0.572
# ATALCWO (.p7.)    0.850    0.025   33.373    0.000    0.850    0.850
# ATALCTR (.p8.)    0.858    0.024   36.318    0.000    0.858    0.858
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.477    0.039   12.088    0.000    0.477    0.477
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.264    0.030   -8.827    0.000   -0.264   -0.264
# ATALC3M (.12.)    0.282    0.030    9.404    0.000    0.282    0.282
# ATALC3M (.13.)    0.792    0.034   23.113    0.000    0.792    0.792
# ATALC3M (.14.)    1.515    0.049   31.139    0.000    1.515    1.515
# ATALCUR (.15.)    0.561    0.032   17.750    0.000    0.561    0.561
# ATALCUR (.16.)    0.865    0.035   24.544    0.000    0.865    0.865
# ATALCUR (.17.)    1.107    0.039   28.116    0.000    1.107    1.107
# ATALCUR (.18.)    1.542    0.050   31.012    0.000    1.542    1.542
# ATALCPR (.19.)    1.365    0.047   29.330    0.000    1.365    1.365
# ATALCPR (.20.)    1.531    0.051   29.751    0.000    1.531    1.531
# ATALCPR (.21.)    1.790    0.061   29.582    0.000    1.790    1.790
# ATALCPR (.22.)    2.127    0.079   26.910    0.000    2.127    2.127
# ATALCEX (.23.)    1.493    0.050   29.952    0.000    1.493    1.493
# ATALCEX (.24.)    1.779    0.061   29.396    0.000    1.779    1.779
# ATALCEX (.25.)    1.950    0.069   28.412    0.000    1.950    1.950
# ATALCEX (.26.)    2.251    0.088   25.676    0.000    2.251    2.251
# ATALCWO (.27.)    0.730    0.034   21.600    0.000    0.730    0.730
# ATALCWO (.28.)    1.414    0.047   30.207    0.000    1.414    1.414
# ATALCTR (.29.)    0.879    0.036   24.382    0.000    0.879    0.879
# ATALCTR (.30.)    1.491    0.049   30.374    0.000    1.491    1.491
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.325                               0.325    0.325
# .ATALCURG          0.077                               0.077    0.077
# .ATALCPRB          0.200                               0.200    0.200
# .ATALCEXP          0.082                               0.082    0.082
# .ATALCWOR          0.278                               0.278    0.278
# .ATALCTRY          0.265                               0.265    0.265
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.822    0.019   44.185    0.000    0.822    0.822
# ATALCUR (.p2.)    0.961    0.021   45.906    0.000    0.961    0.988
# ATALCPR (.p3.)    0.540    0.031   17.200    0.000    0.540    0.549
# ATALCEX (.p4.)    0.543    0.034   15.940    0.000    0.543    0.540
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.500    0.032   15.851    0.000    0.500    0.509
# ATALCEX (.p6.)    0.572    0.033   17.292    0.000    0.572    0.569
# ATALCWO (.p7.)    0.850    0.025   33.373    0.000    0.850    0.873
# ATALCTR (.p8.)    0.858    0.024   36.318    0.000    0.858    0.868
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.407    0.046    8.772    0.000    0.407    0.407
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.264    0.030   -8.827    0.000   -0.264   -0.264
# ATALC3M (.12.)    0.282    0.030    9.404    0.000    0.282    0.282
# ATALC3M (.13.)    0.792    0.034   23.113    0.000    0.792    0.792
# ATALC3M (.14.)    1.515    0.049   31.139    0.000    1.515    1.515
# ATALCUR (.15.)    0.561    0.032   17.750    0.000    0.561    0.577
# ATALCUR (.16.)    0.865    0.035   24.544    0.000    0.865    0.890
# ATALCUR (.17.)    1.107    0.039   28.116    0.000    1.107    1.138
# ATALCUR (.18.)    1.542    0.050   31.012    0.000    1.542    1.586
# ATALCPR (.19.)    1.365    0.047   29.330    0.000    1.365    1.388
# ATALCPR (.20.)    1.531    0.051   29.751    0.000    1.531    1.557
# ATALCPR (.21.)    1.790    0.061   29.582    0.000    1.790    1.820
# ATALCPR (.22.)    2.127    0.079   26.910    0.000    2.127    2.162
# ATALCEX (.23.)    1.493    0.050   29.952    0.000    1.493    1.486
# ATALCEX (.24.)    1.779    0.061   29.396    0.000    1.779    1.771
# ATALCEX (.25.)    1.950    0.069   28.412    0.000    1.950    1.940
# ATALCEX (.26.)    2.251    0.088   25.676    0.000    2.251    2.240
# ATALCWO (.27.)    0.730    0.034   21.600    0.000    0.730    0.750
# ATALCWO (.28.)    1.414    0.047   30.207    0.000    1.414    1.452
# ATALCTR (.29.)    0.879    0.036   24.382    0.000    0.879    0.890
# ATALCTR (.30.)    1.491    0.049   30.374    0.000    1.491    1.509
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.325                               0.325    0.325
# .ATALCURG          0.022                               0.022    0.023
# .ATALCPRB          0.206                               0.206    0.212
# .ATALCEXP          0.135                               0.135    0.134
# .ATALCWOR          0.225                               0.225    0.238
# .ATALCTRY          0.241                               0.241    0.247
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000    0.026   38.666    0.000    1.000    1.000
# ATALCURG          1.028    0.029   35.419    0.000    1.028    1.000
# ATALCPRB          1.016    0.034   30.108    0.000    1.016    1.000
# ATALCEXP          0.995    0.031   31.898    0.000    0.995    1.000
# ATALCWOR          1.027    0.033   30.968    0.000    1.027    1.000
# ATALCTRY          1.012    0.032   31.784    0.000    1.012    1.000
# 
# >

anova(AST.etoh.model.cfa.inv.GENDER.SCALAR.fit, AST.etoh.model.cfa.inv.GENDER.MEANS.fit)
# > anova(AST.etoh.model.cfa.inv.GENDER.SCALAR.fit, AST.etoh.model.cfa.inv.GENDER.MEANS.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.etoh.model.cfa.inv.GENDER.SCALAR.fit 32         63.745                              
# AST.etoh.model.cfa.inv.GENDER.MEANS.fit  34         70.026     1.7414       2     0.4187
# >

################### strict invariance

AST.etoh.model.cfa.inv.GENDER.STRICT.fit <- lavaan::cfa(AST.etoh.model.02, 
                                                        data.etoh.dems.07, 
                                                        std.lv=TRUE, 
                                                        estimator = "WLSMV", 
                                                        group = "GENDER",
                                                        ordered = c("ATALC3M", "ATALCURG", 
                                                                    "ATALCPRB", "ATALCEXP",
                                                                    "ATALCWOR", "ATALCTRY"),
                                                        group.equal=c("loadings", "intercepts", "residuals"))

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.etoh.model.cfa.inv.GENDER.STRICT.fit, fit.measures= TRUE, standardized=TRUE)
# > summary(AST.etoh.model.cfa.inv.GENDER.STRICT.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 27 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         66
# Number of equality constraints                    28
# 
# Number of observations per group:                   
#   2                                             1029
# 1                                              792
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                63.745     102.084
# Degrees of freedom                                32          32
# P-value (Chi-square)                           0.001       0.000
# Scaling correction factor                                  0.642
# Shift parameter for each group:                                 
#   2                                                      1.561
# 1                                                      1.201
# simple second-order correction                             
# Test statistic for each group:
#   2                                           49.065      78.010
# 1                                           14.680      24.074
# 
# Model Test Baseline Model:
#   
#   Test statistic                             12200.814    8087.060
# Degrees of freedom                                30          30
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.511
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.991
# Tucker-Lewis Index (TLI)                       0.998       0.992
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.033       0.049
# 90 Percent confidence interval - lower         0.021       0.039
# 90 Percent confidence interval - upper         0.045       0.060
# P-value RMSEA <= 0.05                          0.992       0.537
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
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.823    0.019   44.115    0.000    0.823    0.823
# ATALCUR (.p2.)    0.970    0.021   46.848    0.000    0.970    0.970
# ATALCPR (.p3.)    0.545    0.032   17.270    0.000    0.545    0.545
# ATALCEX (.p4.)    0.547    0.034   16.006    0.000    0.547    0.547
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.501    0.032   15.908    0.000    0.501    0.501
# ATALCEX (.p6.)    0.573    0.033   17.439    0.000    0.573    0.573
# ATALCWO (.p7.)    0.851    0.026   32.764    0.000    0.851    0.851
# ATALCTR (.p8.)    0.859    0.024   35.916    0.000    0.859    0.859
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.468    0.035   13.234    0.000    0.468    0.468
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.290    0.037   -7.880    0.000   -0.290   -0.290
# ATALC3M (.12.)    0.257    0.037    6.928    0.000    0.257    0.257
# ATALC3M (.13.)    0.767    0.040   19.018    0.000    0.767    0.767
# ATALC3M (.14.)    1.492    0.053   28.375    0.000    1.492    1.492
# ATALCUR (.15.)    0.536    0.040   13.536    0.000    0.536    0.536
# ATALCUR (.16.)    0.844    0.042   20.245    0.000    0.844    0.844
# ATALCUR (.17.)    1.089    0.044   24.529    0.000    1.089    1.089
# ATALCUR (.18.)    1.528    0.053   28.885    0.000    1.528    1.528
# ATALCPR (.19.)    1.356    0.051   26.336    0.000    1.356    1.356
# ATALCPR (.20.)    1.523    0.055   27.526    0.000    1.523    1.523
# ATALCPR (.21.)    1.784    0.063   28.183    0.000    1.784    1.784
# ATALCPR (.22.)    2.122    0.081   26.326    0.000    2.122    2.122
# ATALCEX (.23.)    1.483    0.056   26.508    0.000    1.483    1.483
# ATALCEX (.24.)    1.771    0.065   27.196    0.000    1.771    1.771
# ATALCEX (.25.)    1.942    0.072   26.891    0.000    1.942    1.942
# ATALCEX (.26.)    2.246    0.090   25.013    0.000    2.246    2.246
# ATALCWO (.27.)    0.730    0.042   17.278    0.000    0.730    0.730
# ATALCWO (.28.)    1.415    0.051   27.851    0.000    1.415    1.415
# ATALCTR (.29.)    0.880    0.044   20.125    0.000    0.880    0.880
# ATALCTR (.30.)    1.492    0.054   27.782    0.000    1.492    1.492
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.323                               0.323    0.323
# .ATALCURG          0.058                               0.058    0.058
# .ATALCPRB          0.196                               0.196    0.196
# .ATALCEXP          0.079                               0.079    0.079
# .ATALCWOR          0.276                               0.276    0.276
# .ATALCTRY          0.262                               0.262    0.262
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.823    0.019   44.115    0.000    0.823    0.820
# ATALCUR (.p2.)    0.970    0.021   46.848    0.000    0.970    0.974
# ATALCPR (.p3.)    0.545    0.032   17.270    0.000    0.545    0.546
# ATALCEX (.p4.)    0.547    0.034   16.006    0.000    0.547    0.537
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.501    0.032   15.908    0.000    0.501    0.503
# ATALCEX (.p6.)    0.573    0.033   17.439    0.000    0.573    0.564
# ATALCWO (.p7.)    0.851    0.026   32.764    0.000    0.851    0.872
# ATALCTR (.p8.)    0.859    0.024   35.916    0.000    0.859    0.868
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2             0.419    0.041   10.211    0.000    0.419    0.419
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1            -0.073    0.060   -1.221    0.222   -0.073   -0.073
# ETOH2            -0.001    0.081   -0.017    0.986   -0.001   -0.001
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.290    0.037   -7.880    0.000   -0.290   -0.290
# ATALC3M (.12.)    0.257    0.037    6.928    0.000    0.257    0.256
# ATALC3M (.13.)    0.767    0.040   19.018    0.000    0.767    0.765
# ATALC3M (.14.)    1.492    0.053   28.375    0.000    1.492    1.488
# ATALCUR (.15.)    0.536    0.040   13.536    0.000    0.536    0.538
# ATALCUR (.16.)    0.844    0.042   20.245    0.000    0.844    0.847
# ATALCUR (.17.)    1.089    0.044   24.529    0.000    1.089    1.092
# ATALCUR (.18.)    1.528    0.053   28.885    0.000    1.528    1.534
# ATALCPR (.19.)    1.356    0.051   26.336    0.000    1.356    1.359
# ATALCPR (.20.)    1.523    0.055   27.526    0.000    1.523    1.527
# ATALCPR (.21.)    1.784    0.063   28.183    0.000    1.784    1.788
# ATALCPR (.22.)    2.122    0.081   26.326    0.000    2.122    2.128
# ATALCEX (.23.)    1.483    0.056   26.508    0.000    1.483    1.458
# ATALCEX (.24.)    1.771    0.065   27.196    0.000    1.771    1.742
# ATALCEX (.25.)    1.942    0.072   26.891    0.000    1.942    1.910
# ATALCEX (.26.)    2.246    0.090   25.013    0.000    2.246    2.208
# ATALCWO (.27.)    0.730    0.042   17.278    0.000    0.730    0.748
# ATALCWO (.28.)    1.415    0.051   27.851    0.000    1.415    1.450
# ATALCTR (.29.)    0.880    0.044   20.125    0.000    0.880    0.888
# ATALCTR (.30.)    1.492    0.054   27.782    0.000    1.492    1.506
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.329                               0.329    0.327
# .ATALCURG          0.051                               0.051    0.052
# .ATALCPRB          0.219                               0.219    0.220
# .ATALCEXP          0.145                               0.145    0.140
# .ATALCWOR          0.228                               0.228    0.240
# .ATALCTRY          0.242                               0.242    0.247
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           0.997    0.026   38.692    0.000    0.997    1.000
# ATALCURG          1.003    0.025   39.871    0.000    1.003    1.000
# ATALCPRB          1.002    0.029   34.786    0.000    1.002    1.000
# ATALCEXP          0.983    0.025   38.887    0.000    0.983    1.000
# ATALCWOR          1.025    0.036   28.672    0.000    1.025    1.000
# ATALCTRY          1.010    0.034   29.848    0.000    1.010    1.000
# 
# >


anova(AST.etoh.model.cfa.inv.GENDER.MEANS.fit, AST.etoh.model.cfa.inv.GENDER.STRICT.fit)
# > anova(AST.etoh.model.cfa.inv.GENDER.MEANS.fit, AST.etoh.model.cfa.inv.GENDER.STRICT.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.etoh.model.cfa.inv.GENDER.STRICT.fit 32         63.745                              
# AST.etoh.model.cfa.inv.GENDER.MEANS.fit  34         70.026     1.7414       2     0.4187
# >

################### factor variances and covariances

AST.etoh.model.cfa.inv.GENDER.FVCV.fit <- lavaan::cfa(AST.etoh.model.02,
                                                      data.etoh.dems.07,
                                                      std.lv=TRUE,
                                                      estimator = "WLSMV",
                                                      group = "GENDER",
                                                      ordered = c("ATALC3M", "ATALCURG", 
                                                                  "ATALCPRB", "ATALCEXP",
                                                                  "ATALCWOR", "ATALCTRY"),
                                                      group.equal=c("loadings",
                                                                    "intercepts",
                                                                    "residuals",
                                                                    "lv.variances",
                                                                    "lv.covariances"))                                                                                                              

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AST.etoh.model.cfa.inv.GENDER.FVCV.fit, fit.measures= TRUE, standardized=TRUE)

# > summary(AST.etoh.model.cfa.inv.GENDER.FVCV.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-6 ended normally after 27 iterations
# 
# Estimator                                       DWLS
# Optimization method                           NLMINB
# Number of free parameters                         66
# Number of equality constraints                    29
# 
# Number of observations per group:                   
#   2                                             1029
# 1                                              792
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                65.645      94.996
# Degrees of freedom                                33          33
# P-value (Chi-square)                           0.001       0.000
# Scaling correction factor                                  0.719
# Shift parameter for each group:                                 
#   2                                                      2.067
# 1                                                      1.591
# simple second-order correction                             
# Test statistic for each group:
#   2                                           49.779      71.329
# 1                                           15.866      23.667
# 
# Model Test Baseline Model:
#   
#   Test statistic                             12200.814    8087.060
# Degrees of freedom                                30          30
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.511
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.992
# Tucker-Lewis Index (TLI)                       0.998       0.993
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.033       0.045
# 90 Percent confidence interval - lower         0.021       0.035
# 90 Percent confidence interval - upper         0.045       0.056
# P-value RMSEA <= 0.05                          0.993       0.744
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
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.825    0.019   43.693    0.000    0.825    0.825
# ATALCUR (.p2.)    0.973    0.021   45.876    0.000    0.973    0.973
# ATALCPR (.p3.)    0.549    0.032   17.345    0.000    0.549    0.549
# ATALCEX (.p4.)    0.552    0.034   16.142    0.000    0.552    0.552
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.504    0.032   15.935    0.000    0.504    0.504
# ATALCEX (.p6.)    0.576    0.033   17.534    0.000    0.576    0.576
# ATALCWO (.p7.)    0.855    0.027   31.724    0.000    0.855    0.855
# ATALCTR (.p8.)    0.865    0.025   34.466    0.000    0.865    0.865
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2   (.37.)    0.448    0.026   16.918    0.000    0.448    0.448
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1             0.000                               0.000    0.000
# ETOH2             0.000                               0.000    0.000
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.294    0.037   -7.892    0.000   -0.294   -0.294
# ATALC3M (.12.)    0.255    0.037    6.853    0.000    0.255    0.255
# ATALC3M (.13.)    0.766    0.040   19.001    0.000    0.766    0.766
# ATALC3M (.14.)    1.493    0.053   28.382    0.000    1.493    1.493
# ATALCUR (.15.)    0.535    0.040   13.481    0.000    0.535    0.535
# ATALCUR (.16.)    0.843    0.042   20.219    0.000    0.843    0.843
# ATALCUR (.17.)    1.089    0.044   24.502    0.000    1.089    1.089
# ATALCUR (.18.)    1.530    0.053   28.819    0.000    1.530    1.530
# ATALCPR (.19.)    1.359    0.051   26.581    0.000    1.359    1.359
# ATALCPR (.20.)    1.527    0.055   27.850    0.000    1.527    1.527
# ATALCPR (.21.)    1.789    0.063   28.512    0.000    1.789    1.789
# ATALCPR (.22.)    2.128    0.080   26.586    0.000    2.128    2.128
# ATALCEX (.23.)    1.487    0.055   26.824    0.000    1.487    1.487
# ATALCEX (.24.)    1.776    0.064   27.697    0.000    1.776    1.776
# ATALCEX (.25.)    1.948    0.071   27.396    0.000    1.948    1.948
# ATALCEX (.26.)    2.252    0.089   25.291    0.000    2.252    2.252
# ATALCWO (.27.)    0.726    0.043   16.910    0.000    0.726    0.726
# ATALCWO (.28.)    1.414    0.051   27.806    0.000    1.414    1.414
# ATALCTR (.29.)    0.876    0.044   19.739    0.000    0.876    0.876
# ATALCTR (.30.)    1.491    0.054   27.705    0.000    1.491    1.491
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.320                               0.320    0.320
# .ATALCURG          0.054                               0.054    0.054
# .ATALCPRB          0.197                               0.197    0.197
# .ATALCEXP          0.080                               0.080    0.080
# .ATALCWOR          0.269                               0.269    0.269
# .ATALCTRY          0.252                               0.252    0.252
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           1.000                               1.000    1.000
# ATALCURG          1.000                               1.000    1.000
# ATALCPRB          1.000                               1.000    1.000
# ATALCEXP          1.000                               1.000    1.000
# ATALCWOR          1.000                               1.000    1.000
# ATALCTRY          1.000                               1.000    1.000
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 =~                                                              
#   ATALC3M (.p1.)    0.825    0.019   43.693    0.000    0.825    0.818
# ATALCUR (.p2.)    0.973    0.021   45.876    0.000    0.973    0.970
# ATALCPR (.p3.)    0.549    0.032   17.345    0.000    0.549    0.542
# ATALCEX (.p4.)    0.552    0.034   16.142    0.000    0.552    0.534
# ETOH2 =~                                                              
#   ATALCPR (.p5.)    0.504    0.032   15.935    0.000    0.504    0.498
# ATALCEX (.p6.)    0.576    0.033   17.534    0.000    0.576    0.557
# ATALCWO (.p7.)    0.855    0.027   31.724    0.000    0.855    0.865
# ATALCTR (.p8.)    0.865    0.025   34.466    0.000    0.865    0.863
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1 ~~                                                              
#   ETOH2   (.37.)    0.448    0.026   16.918    0.000    0.448    0.448
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .ATALC3M           0.000                               0.000    0.000
# .ATALCURG          0.000                               0.000    0.000
# .ATALCPRB          0.000                               0.000    0.000
# .ATALCEXP          0.000                               0.000    0.000
# .ATALCWOR          0.000                               0.000    0.000
# .ATALCTRY          0.000                               0.000    0.000
# ETOH1            -0.081    0.061   -1.324    0.186   -0.081   -0.081
# ETOH2            -0.024    0.089   -0.272    0.786   -0.024   -0.024
# 
# Thresholds:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M (.11.)   -0.294    0.037   -7.892    0.000   -0.294   -0.291
# ATALC3M (.12.)    0.255    0.037    6.853    0.000    0.255    0.253
# ATALC3M (.13.)    0.766    0.040   19.001    0.000    0.766    0.760
# ATALC3M (.14.)    1.493    0.053   28.382    0.000    1.493    1.480
# ATALCUR (.15.)    0.535    0.040   13.481    0.000    0.535    0.533
# ATALCUR (.16.)    0.843    0.042   20.219    0.000    0.843    0.841
# ATALCUR (.17.)    1.089    0.044   24.502    0.000    1.089    1.085
# ATALCUR (.18.)    1.530    0.053   28.819    0.000    1.530    1.525
# ATALCPR (.19.)    1.359    0.051   26.581    0.000    1.359    1.341
# ATALCPR (.20.)    1.527    0.055   27.850    0.000    1.527    1.507
# ATALCPR (.21.)    1.789    0.063   28.512    0.000    1.789    1.766
# ATALCPR (.22.)    2.128    0.080   26.586    0.000    2.128    2.100
# ATALCEX (.23.)    1.487    0.055   26.824    0.000    1.487    1.438
# ATALCEX (.24.)    1.776    0.064   27.697    0.000    1.776    1.718
# ATALCEX (.25.)    1.948    0.071   27.396    0.000    1.948    1.885
# ATALCEX (.26.)    2.252    0.089   25.291    0.000    2.252    2.178
# ATALCWO (.27.)    0.726    0.043   16.910    0.000    0.726    0.734
# ATALCWO (.28.)    1.414    0.051   27.806    0.000    1.414    1.431
# ATALCTR (.29.)    0.876    0.044   19.739    0.000    0.876    0.874
# ATALCTR (.30.)    1.491    0.054   27.705    0.000    1.491    1.488
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ETOH1             1.000                               1.000    1.000
# ETOH2             1.000                               1.000    1.000
# .ATALC3M           0.337                               0.337    0.331
# .ATALCURG          0.060                               0.060    0.060
# .ATALCPRB          0.223                               0.223    0.218
# .ATALCEXP          0.149                               0.149    0.139
# .ATALCWOR          0.247                               0.247    0.252
# .ATALCTRY          0.256                               0.256    0.255
# 
# Scales y*:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# ATALC3M           0.992    0.027   36.842    0.000    0.992    1.000
# ATALCURG          0.997    0.027   37.201    0.000    0.997    1.000
# ATALCPRB          0.987    0.027   36.783    0.000    0.987    1.000
# ATALCEXP          0.967    0.022   44.192    0.000    0.967    1.000
# ATALCWOR          1.011    0.040   25.370    0.000    1.011    1.000
# ATALCTRY          0.998    0.037   26.699    0.000    0.998    1.000
# >

anova(AST.etoh.model.cfa.inv.GENDER.STRICT.fit, AST.etoh.model.cfa.inv.GENDER.FVCV.fit)
# > anova(AST.etoh.model.cfa.inv.GENDER.STRICT.fit, AST.etoh.model.cfa.inv.GENDER.FVCV.fit)
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AST.etoh.model.cfa.inv.GENDER.STRICT.fit 32         63.745                              
# AST.etoh.model.cfa.inv.GENDER.FVCV.fit   33         65.644    0.83174       1     0.3618
# >
