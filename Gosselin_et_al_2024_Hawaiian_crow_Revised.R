# 1. General Information -----------------------------------------------------

# Data Analysis Supplemental Material for: 
# Individual Behavior and Housing Setup Interact to Influence Markers of Welfare in the Critically Endangered Hawaiian Crow

# R Script Authors:
# Rachel P. Gosselin
# Alison M. Flanagan
# Alison L. Greggor


# Data vs Paper Notes:

# Data: We described the enclosures as banked and non-banked
# Paper: We changed "non-banked" to "breeding" and "banked" to "holding"

# Data: We described behaviors as stress and non-stress
# Paper: We changed "stress" to "negative" and "non-stress" to "positive" welfare-indicating behaviors

# Data: We used the more general term "chick" for nestlings
# Paper: We used the more specific term of "nestling"

# 2. Setup -------------------------------------------------------------------

library(tidyverse)
library(irr)
library(lme4)
library(MuMIn)
library(formattable)
library(arm)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(viridis)

# 3. Clustering Behaviors ----------------------------------------------------

# Import the data file and remove the ...1 number column

behaviordata <- read_csv("Manuscript Behavior Data.csv")
behaviordata <- subset(behaviordata, select = -c(...1))


# Create a new data frame for clustered behaviors

clustercolumns <- behaviordata
clustercolumns$X = NULL
clustercolumns$BreedingBeh <- 0
clustercolumns$StressBeh <- 0
clustercolumns$NonStressBeh <- 0
clustercolumns$NonActive <- 0
clustercolumns$Sociabiliy <- 0
clustercolumns$Aggress.Sub <- 0


## 3.1 Breeding Behaviors ------------------------------------------------------

# Create histograms to visually analyze the data

allbreeding <- 
  behaviordata[, c(6, 12, 13, 14, 16, 18, 29)]

longallbreed <- 
  gather(
    allbreeding, 
    Behavior, 
    Count, 
    Allofeeding:Sitting, 
    factor_key = TRUE)

ggplot(
  longallbreed, 
  aes(
    x=Count, 
    fill=Behavior)) +
  geom_histogram(binwidth=10)+
  facet_grid(Behavior~.) + 
  stat_bin(bins = 30)


### 3.1.1 Notes - Breeding Behaviors ---------------------------------------------

# Coo and Copulation are rare
# Allofeeding should be kept as a sociable example, not a breeding behavior

# Cup and nest build are mutually exclusive and directly demonstrate breeding behaviors

# The lack of other behaviors due to sitting will be accounted for with the Breeding Stage fixed variable

# We are not looking at, or asking questions about breeding behaviors. 
# Rather, the breeding stage of the pair/individual is simply a confounding variable. 
# The Breeding Stage (added with the other confouding variables) is enough information to determine how 
# breeding affects our results concerning distress and positive welfare

# REMOVED BREEDING BEHAVIORS


## 3.2 Stress Behaviors --------------------------------------------------------

# Histograms
allstress <- behaviordata[, c(9, 10, 22, 27, 30, 31)]

longallstress <- gather(
  allstress, 
  Behavior, 
  Count, 
  Alarm:Wire, 
  factor_key = TRUE)

ggplot(
  longallstress, 
  aes(
    x=Count, 
    fill=Behavior)) +
  geom_histogram(binwidth=10) +
  facet_grid(Behavior~.) + 
  stat_bin(bins =30)

# Limited stress behaviors so make this a binomial

# Wire is both stress and general movement, no way to differentiate the two
# REMOVED WIRE

clustercolumns$StressBeh <- 
  (clustercolumns$Alarm + 
     clustercolumns$Bat + 
     clustercolumns$Pace.Fly + 
     clustercolumns$Roll + 
     clustercolumns$Stereotypy)

clustercolumns$StressBinomial = 0

clustercolumns$StressBinomial[clustercolumns$StressBeh > 0] <- 1


## 3.3 Non-Stress Behaviors ----------------------------------------------------

#Histograms
allnonstress <- behaviordata[, c(15, 17, 19, 24)]

longallnonstress <- 
  gather(
    allnonstress, 
    Behavior, 
    Count, 
    Full.Flight:Preen, 
    factor_key = TRUE)

ggplot(
  longallnonstress, 
  aes(
    x=Count, 
    fill=Behavior)) +
  geom_histogram(binwidth=10)+
  facet_grid(Behavior~.) + 
  stat_bin(bins =30)

# Can't define nonvocs as stressed/not/etc
# REMOVED NONVOCS

clustercolumns$NonStressBeh <- clustercolumns$Full.Flight + clustercolumns$Forage + clustercolumns$Preen


## 3.4 Activity ----------------------------------------------------------------

# Wire can't be separated from stress, Perch too messy
# REMOVED WIRE & PERCH

# Only look at rest duration

behaviordata$Rest <- as.numeric(behaviordata$Rest)
hist(behaviordata$Rest) # looks normal

# Rest = NonActive
clustercolumns$NonActive <- clustercolumns$Rest


## 3.5 Sociability -------------------------------------------------------------

# Histograms

allsocial <- behaviordata[, c(6, 7, 25, 28)]

longallsocial <- 
  gather(
    allsocial, 
    Behavior, 
    Count, 
    Allofeeding:Sharing, 
    factor_key = TRUE)

ggplot(
  longallsocial, 
  aes(
    x=Count, 
    fill=Behavior)) +
  geom_histogram(binwidth=10)+
  facet_grid(Behavior~.) + 
  stat_bin(bins =30)

# Allofeeding/allopreening associated with pair bonding
# Both occur while proximal
# REMOVE ALLOFEEDING & ALLOPREENING

# Only look at proximal

clustercolumns$Sociabiliy <- clustercolumns$Proximity


## 3.6 Aggressive/Submissive Behaviors ----------------------------------------

# Histograms
allaggression <- behaviordata[, c(8, 11)]

longallagg <- gather(
  allaggression, 
  Behavior, 
  Count, 
  Aggression:Begging, 
  factor_key = TRUE)

ggplot(
  longallagg, 
  aes(
    x=Count, 
    fill=Behavior)) +
  geom_histogram(binwidth=10)+
  facet_grid(Behavior~.) + 
  stat_bin(bins =30)

# Opposites on a continuum, begging negative, aggression positive

clustercolumns$Aggress.Sub <- clustercolumns$Aggression - clustercolumns$Begging



## 3.7 Clustered Behavior Data Frame -------------------------------------------

# New df of behavior clusters
names(clustercolumns)
clusters <- clustercolumns[,c(1:5, 21, 33:38)]
names(clusters)


# 4. Rate Data ---------------------------------------------------------------

# Transform time to minutes

clusters$X = NULL
clusters$Time.Vis.Min <- 0
clusters$Time.Rest.Min <- 0

clusters$Time.Vis.Min <- (clusters$Time.Visible / 60)
clusters$Time.Rest.Min <- (clusters$NonActive / 60)


# Make all behaviors clusters rates

# New Columns
clusters$Stress.Rate <- 0
clusters$Sociable.Rate <- 0
clusters$Aggress.Sub.Rate <- 0
clusters$NonStress.Rate <- 0
clusters$Rest.Rate <- 0

# Original column / time vis min

clusters$Stress.Rate <- (clusters$StressBeh / clusters$Time.Vis.Min)
clusters$Sociable.Rate <- (clusters$Sociabiliy / clusters$Time.Vis.Min)
clusters$Aggress.Sub.Rate <- (clusters$Aggress.Sub / clusters$Time.Vis.Min)
clusters$NonStress.Rate <- (clusters$NonStressBeh / clusters$Time.Vis.Min)
clusters$Rest.Rate <- (clusters$Time.Rest.Min / clusters$Time.Vis.Min)

# New df of rates

names(clusters)
rate.data <- clusters[, c(1:5, 12, 15:19)]
names(rate.data)


# 5. General ICC Tests -------------------------------------------------------

# New column for random selection and merging
rate.data$X = NULL

rate.data$PairDate <- 
  paste(
    rate.data$Pair, 
    rate.data$Date, 
    sep = "_" )

# Take out AL184_AL124, two different conditions (aviary types)
rate.data$Pair <- as.character(rate.data$Pair)
same.housing.rate.data <- rate.data[rate.data$Pair != "AL184_AL124",]

# Female subset for random selection
females_for_random_obs <- same.housing.rate.data[same.housing.rate.data$Sex == "Female",]

# Two pairs have 5 observations, so randomly select 5 observations for the remaining 24 pairs. 
# This will lead to varying results, run tests 10,000 times each and average the results

# Dfs for p values within each randomization and overall
iccpvalues <- 
  data.frame(
    "Stress" = NA, 
    "Sociability" = NA, 
    "AggSub" = NA,
    "NonActive" = NA,
    "NonStress" = NA)

alliccpvalues <- 
  data.frame(
    "Stress" = NA, 
    "Sociability" = NA, 
    "AggSub" = NA,
    "NonActive" = NA,
    "NonStress" = NA)

# Dfs for F values within each randomization and overall
iccfvalues <- 
  data.frame(
    "Stress" = NA, 
    "Sociability" = NA, 
    "AggSub" = NA,
    "NonActive" = NA,
    "NonStress" = NA)

alliccfvalues <- 
  data.frame(
    "Stress" = NA, 
    "Sociability" = NA, 
    "AggSub" = NA,
    "NonActive" = NA,
    "NonStress" = NA)


for(j in 1:10000) {

  # Select five random observations based on PairDate identifier
  random <- females_for_random_obs %>% group_by(Pair) %>% sample_n(5)
  
  ## Subset random df
  pairdate <- random[,c("PairDate")]
  
  ## Merge pairdate and original df
  icc.rate.data <- merge(pairdate, same.housing.rate.data)

## 5.1 Stress ICC --------------------------------------------------------------

  stress_icc <- 
    data.frame(
      "Alala"= (unique(icc.rate.data$SCSB)), 
      "Day.1"= NA,
      "Day.2"= NA, 
      "Day.3"= NA, 
      "Day.4"=NA, 
      "Day.5"=NA)  
  
  for(i in 1:length(unique(icc.rate.data$SCSB))){
    bird <- unique(icc.rate.data$SCSB)[i]
    all_bird <- icc.rate.data[which(icc.rate.data$SCSB==bird),]
    all_stress <- all_bird$Stress.Rate # Using stress rates, not binomial
    stress_icc[i, 2:6] <- all_stress
  }
  
  iccpvalues$Stress <- 
    icc(
      stress_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$p.value
  
  iccfvalues$Stress <- 
    icc(
      stress_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$Fvalue
  
## 5.2 Sociability ICC ---------------------------------------------------------
  
  social_icc <- 
    data.frame(
      "Alala"= (unique(icc.rate.data$SCSB)), 
      "Day.1"= NA,
      "Day.2"= NA, 
      "Day.3"= NA, 
      "Day.4"=NA, 
      "Day.5"=NA)  
  
  for(i in 1:length(unique(icc.rate.data$SCSB))){
    bird <- unique(icc.rate.data$SCSB)[i]
    all_bird <- icc.rate.data[which(icc.rate.data$SCSB==bird),]
    all_social <- all_bird$Sociable.Rate
    social_icc[i, 2:6] <- all_social
  }
  
  iccpvalues$Sociability <- 
    icc(
      social_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$p.value
  
  iccfvalues$Sociability <- 
    icc(
      social_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$Fvalue
  
## 5.3 Aggressive/Submissive ICC -----------------------------------------------

  agg.sub_icc <- 
    data.frame(
      "Alala"= (unique(icc.rate.data$SCSB)), 
      "Day.1"= NA,
      "Day.2"= NA, 
      "Day.3"= NA, 
      "Day.4"=NA, 
      "Day.5"=NA)  
  
  for(i in 1:length(unique(icc.rate.data$SCSB))){
    bird <- unique(icc.rate.data$SCSB)[i]
    all_bird <- icc.rate.data[which(icc.rate.data$SCSB==bird),]
    all_aggsub <- all_bird$Aggress.Sub.Rate
    agg.sub_icc[i, 2:6] <- all_aggsub
  }
  
  iccpvalues$AggSub <- 
    icc(
      agg.sub_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$p.value
  
  iccfvalues$AggSub <- 
    icc(
      agg.sub_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$Fvalue

## 5.4 Non-activity ICC --------------------------------------------------------

  rest_icc <- 
    data.frame(
      "Alala"= (unique(icc.rate.data$SCSB)), 
      "Day.1"= NA,
      "Day.2"= NA, 
      "Day.3"= NA, 
      "Day.4"=NA, 
      "Day.5"=NA)  
  
  for(i in 1:length(unique(icc.rate.data$SCSB))){
    bird <- unique(icc.rate.data$SCSB)[i]
    all_bird <- icc.rate.data[which(icc.rate.data$SCSB==bird),]
    all_rest <- all_bird$Rest.Rate
    rest_icc[i, 2:6] <- all_rest
  }
  
  iccpvalues$NonActive <- 
    icc(
      rest_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$p.value
  
  iccfvalues$NonActive <- 
    icc(
      rest_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$Fvalue

## 5.5 Non-stress ICC ----------------------------------------------------------

  nonstress_icc <- 
    data.frame(
      "Alala"= (unique(icc.rate.data$SCSB)), 
      "Day.1"= NA,
      "Day.2"= NA, 
      "Day.3"= NA, 
      "Day.4"=NA, 
      "Day.5"=NA)  
  
  for(i in 1:length(unique(icc.rate.data$SCSB))){
    bird <- unique(icc.rate.data$SCSB)[i]
    all_bird <- icc.rate.data[which(icc.rate.data$SCSB==bird),]
    all_nonstress <- all_bird$NonStress.Rate
    nonstress_icc[i, 2:6] <- all_nonstress
  }
  
  iccpvalues$NonStress <- 
    icc(
      nonstress_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$p.value
  
  iccfvalues$NonStress <- 
    icc(
      nonstress_icc[,c(2:6)], 
      model=c("oneway"), 
      type= c("consistency")
      )$Fvalue
  
  ### Add these new p/F values to their overall df
  
  alliccpvalues <- rbind(iccpvalues, alliccpvalues)
  alliccfvalues <- rbind(iccfvalues, alliccfvalues)
  
}


# Remove initial NA row

alliccpvalues <- na.omit(alliccpvalues)
alliccfvalues <- na.omit(alliccfvalues)


## 5.7 ICC Results -------------------------------------------------------------

### Stress

mean(alliccpvalues$Stress) # 0.05923185, Stress isn't significantly consistent
mean(alliccfvalues$Stress) # 1.969951

### Sociability

mean(alliccpvalues$Sociability) # 3.982995e-06
mean(alliccfvalues$Sociability) # 2.906632

### Agg/Sub

mean(alliccpvalues$AggSub) # 4.601107e-06
mean(alliccfvalues$AggSub) # 3.198406

### Non-Activity

mean(alliccpvalues$NonActive) # 2.281913e-06
mean(alliccfvalues$NonActive) # 2.835643

### Non-Stress

mean(alliccpvalues$NonStress) # 2.976672e-05
mean(alliccfvalues$NonStress) # 2.449627


# 6. Feather Assessment Data -------------------------------------------------

# Additional stress/nonstress measure to check if observational data is robust

# Feathers were noted as Full, Partial, and None on 7 body parts. 
# These were numerically altered to 2, 1, and 0, respectively. 
# The 7 body part scores were then added to make a 0 - 14 scale per assessment. 
# The two assessment scores were then combined to make a 0 - 28 scale of cumulative 
# feather plucking across the two assessment periods. 

AllFeathers <- read_csv("code_ready_feather_assessments.csv")

SimpleFeathers <- AllFeathers[,c(1,10)]

UniqueFeathers <- na.omit(SimpleFeathers)

### Check assessment score spread - rank or binomial
hist(
  UniqueFeathers$'Total Score:', 
  ylab="# of birds with that score", 
  xlab="Feather Assessment Sum Scores",
  main="Histogram of Alala Feather Assessment Sum Scores", 
  pch=19, 
  breaks = 10)

# Enough variation between 16 and 28 for a rank

UniqueFeathers$FeatherRank <- dense_rank(UniqueFeathers$'Total Score:')

# Edit studbook column name for combining dfs later
colnames(UniqueFeathers) <- c("SCSB", "FeatherScores", "FeatherRank")


## Make a df with stress and non stress rates for ranks

names(rate.data)

feather_stress_ranks <- unique(rate.data[,c(1, 4:5)]) 

## Averages of stress and non stress
# Stress
str(rate.data) #check stress.rate is numeric
StressAverage  <- 
  rate.data %>% 
  group_by(SCSB) %>% 
  summarize(Avg.Stress.Rate = mean(Stress.Rate)) %>% 
  as.data.frame()

# Non-stress
str(rate.data) # Check nonstress.rate is numeric
NonStressAverage <- 
  rate.data %>% 
  group_by(SCSB) %>% 
  summarize(Avg.Nonstress.Rate = mean(NonStress.Rate)) %>% 
  as.data.frame()


### Combine dfs for analysis
feather_stress_ranks <- 
  merge(
    StressAverage, 
    feather_stress_ranks, 
    by = "SCSB")

feather_stress_ranks <- 
  merge(
    NonStressAverage, 
    feather_stress_ranks,
    by = "SCSB")

feather_stress_ranks <- 
  merge(
    UniqueFeathers, 
    feather_stress_ranks, 
    by = "SCSB")

## Rank stress and non stress
feather_stress_ranks$StressRank <- 
  dense_rank(feather_stress_ranks$Avg.Stress.Rate)

feather_stress_ranks$NonStressRank <- 
  dense_rank(feather_stress_ranks$Avg.Nonstress.Rate)


# Spearman's rank correlation

# Two separate rank correlation tests, so need a family wise error correction
# Use Bonferroni test - most conservative method

# Stress - p = 0.05 > significant

cor.test(
  feather_stress_ranks$StressRank, 
  feather_stress_ranks$FeatherRank,
  method = "spearman")

#                 Spearman's rank correlation rho

# data:  feather_stress_ranks$StressRank and feather_stress_ranks$FeatherRank
# S = 28475, p-value = 0.1249
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
#   -0.2155327 

# NOT CORRELATED



# Nonstress - p = 0.025 > significant

cor.test(
  feather_stress_ranks$NonStressRank, 
  feather_stress_ranks$FeatherRank,
  method = "spearman")

# Spearman's rank correlation rho

# data:  feather_stress_ranks$NonStressRank and feather_stress_ranks$FeatherRank
# S = 19993, p-value = 0.2999
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
#   0.1465427 

# NOT CORRELATED


# 7. Confounding Variables ---------------------------------------------------

# Import file with the weather, breeding, and food variables

confounding.data <- read_csv("Confounding Variables R 26pair Pilo AvB.csv")

# Merge with rate data frame

confounding.w.rate <- 
  merge(
    rate.data, 
    confounding.data, 
    by = "PairDate", 
    all.x = TRUE, 
    all.y = TRUE)

# Import file with housing type data

housing.data <- read_csv("Banked-Non List (w AvB, Pilo).csv")

# Merge with confounding & rate data

housing.rate.confounding <- 
  merge(
    confounding.w.rate, 
    housing.data, 
    by.x = "SCSB", 
    by.y = "Individual ID", 
    all.x = TRUE, 
    all.y = TRUE)

# Change the last three observations of AL124 and AL184 from Banked to Not

housing.rate.confounding <- 
  within(
    housing.rate.confounding, 
    `Banked/Not`[
      `Banked/Not` == "Banked" &
        (PairDate == "AL184_AL124_20190628" |
           PairDate == "AL184_AL124_20190702" |
           PairDate == "AL184_AL124_20190712")
      ] <- "Not")


# Comparing poorer weather to Sunny is more logical 
# Sunny "aS" instead of "S" such that it's alphabetically first

housing.rate.confounding$Weather <- as.character(housing.rate.confounding$Weather)

housing.rate.confounding$Weather <- 
  ifelse(housing.rate.confounding$Weather == "S", 
         "aS", 
         housing.rate.confounding$Weather)


# Better to compare non breeding to breeding (sit and chick)
# No "aNo" instead of "No"

housing.rate.confounding$Breeding <- as.character(housing.rate.confounding$Breeding)

housing.rate.confounding$Breeding <- 
  ifelse(
    housing.rate.confounding$Breeding == "No", 
    "aNo", 
    housing.rate.confounding$Breeding)

# There are two Halfway counts ("Halfway" and "Halfway ") from import/human error, delete that space and check the factors again
housing.rate.confounding$Food <- as.character(housing.rate.confounding$Food)

housing.rate.confounding$Food <- 
  ifelse(
    housing.rate.confounding$Food == "Halfway ", 
    "Halfway", 
    housing.rate.confounding$Food)

housing.rate.confounding$Food <- as.factor(housing.rate.confounding$Food)

levels(housing.rate.confounding$Food)

head(housing.rate.confounding)



# 8. Keeper Presence Data ----------------------------------------------------

keeper <- read_csv("Keeper Presence Data (w AvB, Pilo).csv")

# Need to organize data and assign behaviors to individual birds

# Split observations into M and F df

M.keeper.data <- keeper[, c(1:8, 10, 12, 14, 16, 21, 23, 25, 27, 29, 31, 36)]
M.keeper.data$X = NULL
M.keeper.data$Sex <- "Male"
M.keeper.data$SCSB <- substr(M.keeper.data$Pair, 7, 11)

F.keeper.data <- keeper[, c(1:5, 9, 11, 13, 15, 17, 21, 24, 26, 28, 30, 32, 36)]
F.keeper.data$X = NULL
F.keeper.data$Sex <- "Female"
F.keeper.data$SCSB <- substr(F.keeper.data$Pair, 1, 5)

## Non directed calls could be anything, remove

M.keeper.data <- M.keeper.data[, c(1:9, 11:15, 17:21)]
F.keeper.data <- F.keeper.data[, c(1:7, 9:13, 15:19)]

## 8.1 Keeper Boldness ---------------------------------------------------------

# Assume M locked out of the HB would have been inside the HB if allowed 

M.keeper.data <- 
  within(
    M.keeper.data, 
    `Wait Location Male`[
      `Wait Location Male` == 0 | 
        `Wait Location Male` == 1 & 
        `Hack Box Male (Y/N)` == "Y"
      ] <- 2)

M.keeper.data <- 
  within(
    M.keeper.data, 
    `Afternoon Wait Location Male`[
      `Afternoon Wait Location Male` == 0 | 
        `Afternoon Wait Location Male` == 1 & 
        `Hack Box Male (Y/N)` == "Y"
      ] <- 2)

# Wait location + food timing = Boldness

M.keeper.data$Boldness <- 
  M.keeper.data$`Food Timing Male` + 
  M.keeper.data$`Wait Location Male` +
  M.keeper.data$`Afternoon Food Timing Male` + 
  M.keeper.data$`Afternoon Wait Location Male`

F.keeper.data$Boldness <- 
  F.keeper.data$`Food Timing Female` + 
  F.keeper.data$`Wait Location Female` +
  F.keeper.data$`Afternoon Food Timing Female` + 
  F.keeper.data$`Afternoon Wait Location Female`

## 8.2 Keeper Aggression ---------------------------------------------------

# Alarm + Direct Vocs

M.keeper.data$M.Aggressive.Vocs <- 
  M.keeper.data$`Alarm Calls Male (#)` + 
  M.keeper.data$`Directed Non-Alarm Calls Male (#)` + 
  M.keeper.data$`Afternoon Alarm Calls Male (#)` +
  M.keeper.data$`Afternoon Directed Non-Alarm Calls Male (#)`

F.keeper.data$F.Aggressive.Vocs <- 
  F.keeper.data$`Alarm Calls Female (#)` + 
  F.keeper.data$`Directed Non-Alarm Calls Female (#)` + 
  F.keeper.data$`Afternoon Alarm Calls Female (#)` +
  F.keeper.data$`Afternoon Directed Non-Alarm Calls Female (#)`

# Need to quantify vocs over time

M.keeper.data$Total.Time.Sec <- 
  M.keeper.data$`Length (in seconds)` + 
  M.keeper.data$`Afternoon Length (in seconds)`

M.keeper.data$Total.Time.Min <- M.keeper.data$Total.Time.Sec / 60

F.keeper.data$Total.Time.Sec <- 
  F.keeper.data$`Length (in seconds)` + 
  F.keeper.data$`Afternoon Length (in seconds)`

F.keeper.data$Total.Time.Min <- F.keeper.data$Total.Time.Sec / 60

# Per minute 

M.keeper.data$Agg.Voc.Per.Min <- 
  M.keeper.data$M.Aggressive.Vocs / M.keeper.data$Total.Time.Min

F.keeper.data$Agg.Voc.Per.Min <- 
  F.keeper.data$F.Aggressive.Vocs / F.keeper.data$Total.Time.Min

## 8.3 Combine Keeper Data -------------------------------------------------

# Add F Hack.Box column (all N)

F.keeper.data$Hack.Box <- "N"

# Edit M Hack.Box column name for binding

M.keeper.data$Hack.Box <- M.keeper.data$`Hack Box Male (Y/N)`

# Bind

M.keeper.clean <- M.keeper.data[,c(1, 18, 19, 2:5, 25, 20, 24)]
F.keeper.clean <- F.keeper.data[,c(1, 16, 17, 2:5, 23, 18, 22)]

All.keeper.clean <- rbind(M.keeper.clean, F.keeper.clean)
head(All.keeper.clean)


## 8.4 Keeper ICC --------------------------------------------------------------

# Remove AL184_AL124, different conditions (aviary types)

All.keeper.clean$Pair <- as.factor(All.keeper.clean$Pair)
twentyfive.keeper.data <- All.keeper.clean[All.keeper.clean$Pair != "AL184_AL124",]

# New column for random selection and merging

twentyfive.keeper.data$X = NULL

twentyfive.keeper.data$PairDate <- 
  paste(
    twentyfive.keeper.data$Pair, 
    twentyfive.keeper.data$Date, 
    sep = "_" )

# Random selection of 5 observations
# Run test 10,000 times

# Subset data to include only females
females_random_keeper <- twentyfive.keeper.data[twentyfive.keeper.data$Sex == "Female",]

# Dfs for p values within each randomization and overall
keeperpvalues <- data.frame("Boldness" = NA, "AggVocs" = NA)
allkeeperpvalues <- data.frame("Boldness" = NA, "AggVocs" = NA)

# Dfs for F values within each randomization and overall
keeperfvalues <- data.frame("Boldness" = NA, "AggVocs" = NA)
allkeeperfvalues <- data.frame("Boldness" = NA, "AggVocs" = NA)


for (j in 1:10000) {

  random <- females_random_keeper %>% group_by(Pair) %>% sample_n(5)
  
  ## Subset random df to include only PairDate, merge with full data to get both M & F 
  ## observations of the five selected dates
  pairdate <- random[,c("PairDate")]
  
  ## Merge
  icc.keeper.data <- merge(pairdate, twentyfive.keeper.data) ## 250 obs.- 10 per 25 pairs
  
  
  ## Boldness
  
  boldness_icc <- 
    data.frame(
      "Alala" = (unique(icc.keeper.data$SCSB)), 
      "Day.1" = NA,
      "Day.2" = NA, 
      "Day.3" = NA, 
      "Day.4" = NA, 
      "Day.5" = NA)  
  
  for(i in 1:length(unique(icc.keeper.data$SCSB))){
    bird <- unique(icc.keeper.data$SCSB)[i]
    all_bird <- icc.keeper.data[which(icc.keeper.data$SCSB==bird),]
    all_bold <- all_bird$Boldness
    boldness_icc[i, 2:6] <- all_bold
  }
  
  keeperpvalues$Boldness <- 
    icc(
      boldness_icc[,c(2:6)], 
      model = c("oneway"), 
      type = c("consistency")
      )$p.value
  
  keeperfvalues$Boldness <- 
    icc(
      boldness_icc[,c(2:6)], 
      model = c("oneway"), 
      type = c("consistency")
      )$Fvalue
  
  
  ## Aggressive Vocs
  
  agg_vocs_icc <- data.frame(
    "Alala" = (unique(icc.keeper.data$SCSB)), 
    "Day.1" = NA,
    "Day.2" = NA, 
    "Day.3" = NA, 
    "Day.4" = NA, 
    "Day.5" = NA)  
  
  for(i in 1:length(unique(icc.keeper.data$SCSB))){
    bird <- unique(icc.keeper.data$SCSB)[i]
    all_bird <- icc.keeper.data[which(icc.keeper.data$SCSB == bird),]
    all_agg_vocs <- all_bird$Agg.Voc.Per.Min
    agg_vocs_icc[i, 2:6] <- all_agg_vocs
  }
  
  keeperpvalues$AggVocs <-  
    icc(
      agg_vocs_icc[,c(2:6)], 
      model = c("oneway"), 
      type = c("consistency")
      )$p.value
  
  keeperfvalues$AggVocs <-  
    icc(
      agg_vocs_icc[,c(2:6)], 
      model = c("oneway"), 
      type = c("consistency")
      )$Fvalue
  
  
  ## Add these new p values to the overall df
  
  allkeeperpvalues <- rbind(keeperpvalues, allkeeperpvalues)
  allkeeperfvalues <- rbind(keeperfvalues, allkeeperfvalues)
  
}


  # Remove initial NA row

allkeeperpvalues <- na.omit(allkeeperpvalues)
allkeeperfvalues <- na.omit(allkeeperfvalues)


#### Results

## Boldness
mean(allkeeperpvalues$Boldness) # 3.383481e-17
mean(allkeeperfvalues$Boldness) # 5.626207

## Aggressive Vocs
mean(allkeeperpvalues$AggVocs) # 2.828803e-05
mean(allkeeperfvalues$AggVocs) # 2.650256



# 9. Aggression Cross Context ICC --------------------------------------------

# Aggression recorded both toward birds and humans = two contexts -> better definition of aggression as personality trait
# Check the consistency between contexts 

# Rank aggression levels
# Average each birds' aggression per context

# Averages df
Aggression_Averages <- unique(rate.data[,c(1, 4:5)])

# General Observation Aggression Averages 
str(rate.data) #check agg.sub.rate is numeric
Aggression_General_Averages <- 
  rate.data %>% 
  group_by(SCSB) %>% 
  summarize(General.Avg.Agg = mean(Aggress.Sub.Rate)) %>% 
  as.data.frame()

# Keeper Presence Aggression Averages 
# Some known aggressive birds don't vocalize (ex. AL035) and should be ranked higher
# Make "Y" = 10 to place aggressive birds' above others in rank

str(All.keeper.clean) # Need chr not factor, check vocs are num
All.keeper.clean$Hack.Box <- as.character(All.keeper.clean$Hack.Box)

All.keeper.clean$Voc.Altered <- 
  ifelse(
    All.keeper.clean$Hack.Box == "Y",
    (All.keeper.clean$Agg.Voc.Per.Min + 10), 
    (All.keeper.clean$Agg.Voc.Per.Min))

# Average 
str(All.keeper.clean) # Check voc.altered is numeric

Aggression_Keeper_Averages <- 
  All.keeper.clean %>% 
  group_by(SCSB) %>% 
  summarize(Keeper.Avg.Agg = mean(Voc.Altered)) %>% 
  as.data.frame()

# Combine dfs for analysis
Aggression_Averages <- 
  merge(
    Aggression_Averages, 
    Aggression_General_Averages, 
    by = "SCSB")

Aggression_Averages <- 
  merge(
    Aggression_Averages, 
    Aggression_Keeper_Averages, 
    by = "SCSB")

# Remove our moving pair AL184_AL124
Aggression_Averages$Pair <- 
  as.factor(Aggression_Averages$Pair)

Aggression_Averages_25 <- 
  Aggression_Averages[
    Aggression_Averages$Pair != "AL184_AL124",]

# Add rank columns
Aggression_Averages_25$General.Agg.Rank <- 
  dense_rank(
    Aggression_Averages_25$General.Avg.Agg)

Aggression_Averages_25$Keeper.Agg.Rank <- 
  dense_rank(
    Aggression_Averages_25$Keeper.Avg.Agg)


# Spearman's rank correlation

cor.test(
  Aggression_Averages_25$General.Agg.Rank, 
  Aggression_Averages_25$Keeper.Agg.Rank,
  method = "spearman")

# Results

# data:  Aggression_Averages_25$General.Agg.Rank and Aggression_Averages_25$Keeper.Agg.Rank
# S = 26725, p-value = 0.04618
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.2833217 


# Ranks are correlated
# AGGRESSION IS CORRELATED CROSS CONTEXT

# However, correlation is negative
# Perhaps due to human vs 'alala comfort levels?
# Humans aren't necessarily a negative or positive stimuli, 
# so the interaction depends on the bird's perception
# Other birds present at competition for territory/mates, 
# so perhaps those more concerned with other birds don't care about the humans and vice-versa, 
# just depends on their respective comfort levels/priorities?


# 10. Non-stress & Enclosure GLMM ---------------------------------------------

# Question: Is there a difference in the expression of non-stress behaviors of captive breeding 'Alala in banked and non-banked aviaries?
# Hypothesis: Non banked aviaries have more non stress behaviors exhibited

# Df for analysis
names(housing.rate.confounding)
NonStress_Enclosure_GLMM <- housing.rate.confounding[,c(1, 3:6, 11, 13:16)]

# Check all factor/num
str(NonStress_Enclosure_GLMM)
NonStress_Enclosure_GLMM$SCSB <- as.factor(NonStress_Enclosure_GLMM$SCSB)
NonStress_Enclosure_GLMM$Pair <- as.factor(NonStress_Enclosure_GLMM$Pair)
NonStress_Enclosure_GLMM$Date <- as.factor(NonStress_Enclosure_GLMM$Date)
NonStress_Enclosure_GLMM$Time.Slots <- as.factor(NonStress_Enclosure_GLMM$Time.Slots)
NonStress_Enclosure_GLMM$Sex <- as.factor(NonStress_Enclosure_GLMM$Sex)
NonStress_Enclosure_GLMM$Weather <- as.factor(NonStress_Enclosure_GLMM$Weather)
NonStress_Enclosure_GLMM$Breeding <- as.factor(NonStress_Enclosure_GLMM$Breeding)
NonStress_Enclosure_GLMM$'Banked/Not' <- as.factor(NonStress_Enclosure_GLMM$'Banked/Not')
str(NonStress_Enclosure_GLMM)

# We also want to consider how the progression of sessions could affect our results
# Create a column that labels the sessions 1-6 for each individual 

NonStress_Enclosure_GLMM <- 
  as.data.frame(
    transform(
      NonStress_Enclosure_GLMM,
      Trial_num = ave(
        seq_along(SCSB), 
        SCSB, 
        FUN = seq_along)))

NonStress_Enclosure_GLMM$Trial_num <- 
  ordered(NonStress_Enclosure_GLMM$Trial_num, levels = 1:6)

str(NonStress_Enclosure_GLMM)


## RUN THE GLMM 

# Check if non-stress is normal
hist(NonStress_Enclosure_GLMM$NonStress.Rate) # Not normal but not overly skewed
# Square more normal?
hist(sqrt(NonStress_Enclosure_GLMM$NonStress.Rate + 1)) # Yes, use sqrt non stress


## This model is Gaussian & tests for the probability of exhibiting non-stress behavior 
## based on housing conditions

## Response variable: Non Stress Behaviors (square root transformed)

## Fixed effects: Enclosure type (banked, non-banked) + time slot + weather + food timing + 
## breeding stage + sex + trial (1-6)

## Random effects: SCSB (Individual ID)

# Because of 0s, add 1 to entire column first, then sqrt
NonStress_Enclosure_GLMM$plusoneNonStress <- NonStress_Enclosure_GLMM$NonStress.Rate + 1
NonStress_Enclosure_GLMM$sqrtNonStress <- sqrt(NonStress_Enclosure_GLMM$plusoneNonStress)

# Check chr/factor/num
str(NonStress_Enclosure_GLMM)

# Make the global model, check residuals, and use VIF test for structural multicollinearity

GlobalModel <- 
  lmer(sqrtNonStress ~ 
         Banked.Not + 
         Time.Slots + 
         Weather + 
         Food + 
         Sex + 
         Breeding + 
         Trial_num + 
         (1|SCSB), 
       data = NonStress_Enclosure_GLMM, 
       REML = FALSE, 
       control = lmerControl(optimizer = "bobyqa"),
       na.action = "na.fail")

plot(GlobalModel) # Nicer residuals

# Run VIF 
# If VIF < 5, keep the predictors.  
# If not, remove them, rerun the model, and repeat VIF analysis

require(car)

vif(GlobalModel)

#                 GVIF Df GVIF^(1/(2*Df))
# Banked.Not 1.090516  1        1.044278
# Time.Slots 2.222180  2        1.220941
# Weather    2.358951  3        1.153772
# Food       2.199568  2        1.217824
# Sex        1.000000  1        1.000000
# Breeding   1.302854  2        1.068376
# Trial_num  2.287924  5        1.086286

## Square the scaled general VIF

# Banked.Not 1.090517
# Time.Slots 1.490697
# Weather    1.331190
# Food       1.483095
# Sex        1.000000
# Breeding   1.141427
# Trial_num  1.180017

## Keep all predictors, all VIF < 5,Belsley et al. (1980) 


## Model averaging

options(na.action = "na.fail") 

## Generate submodels 
m1.submodels <- dredge(GlobalModel)

## Select top submodels based on a delta AICc score of <= 2 of the best model
m1.topmodels <- get.models(m1.submodels, subset = delta <= 2) 

## Export top submodels
m1.selection <- model.sel(m1.topmodels)
m1.selection$model_id <- row.names(m1.selection)
m1.selection$formula <- NA
m1.sel_subset_TABLE <- NA

m1.sel_subset_TABLE <- 
  data.frame(m1.selection) %>% 
  dplyr::select(
    model_id, 
    df, 
    logLik,
    AICc, 
    delta, 
    weight,
    formula)


## Pull out the formulas from each top model
v <- character() 
list_len <- length(m1.topmodels)
for(i in 1:list_len)
  v <- c(v, m1.topmodels[[`i`]]@call[2]) 
print(v)

## Add the formulas to the m1.sel_subset_TABLE dataframe
m1.sel_subset_TABLE$formula <- v

## Check
m1.sel_subset_TABLE$formula


## Format
m1.sel_subset_TABLE <- m1.sel_subset_TABLE %>%
  mutate(formula = as.character(formula)) %>%
  as.data.frame()

m1.submodel_results <- m1.sel_subset_TABLE
m1.submodel_results # Supplementary materials export

write_csv(m1.submodel_results, "Positive Welfare Top Models Table.csv")


## Average parameter estimates over top model set 
glmm.modelavg <- model.avg(m1.topmodels)
summary(glmm.modelavg) 
print(glmm.modelavg$sw) 


## Calculate 95% confidence intervals around each parameter estimate
(ci <- round(confint(glmm.modelavg),2))
ci <- as.data.frame(ci)
ci$Variable <- row.names(ci)


## Extract parameter estimates & their standard errors
coeff <- coefTable(glmm.modelavg)
coeff <- as.data.frame(coeff)
coeff$Variable <- row.names(coeff)


## Extract relative importance scores
ri <- glmm.modelavg$sw
ri <- as.data.frame(ri)
ri$Variable <- row.names(ri)
ri

## Add intercept 
interceptri <- c("---", "Intercept")
ri <- rbind(ri, interceptri)

colnames(ri) <- c("RI", "Parameter")
ri

## Make table
merge1 <- 
  merge(
    coeff, 
    ci, 
    by = "Variable", 
    all = TRUE)

merge1

merge1$df = NULL

merge1$Level <- 
  c("Intercept", 
    "Not Banked", 
    "Chick Rearing", 
    "Incubating",
    "Male", 
    "Obs. 1", 
    "Obs. 2", 
    "Obs. 3",
    "Obs. 4",
    "Obs. 5",
    "Mist",
    "Overcast",
    "Rain")

merge1$Variable = NULL

merge1$"Parameter" <- NA

merge1$"Parameter" <- 
  c("Intercept", 
    "Housing", 
    "Breeding Stage", 
    "Breeding Stage", 
    "Sex", 
    "Observation", 
    "Observation", 
    "Observation",
    "Observation",
    "Observation",
    "Weather",
    "Weather",
    "Weather")

merge1


# CI column
merge1$CIa = NA
merge1$CIb = NA
merge1$CI = NA
merge1$CIa <- paste("(", merge1$`2.5 %`, ",", sep = "")
merge1$`97.5 %` <- as.character(merge1$`97.5 %`)
merge1$CIb <- paste(merge1$CIa, merge1$`97.5 %`, sep = " ")
merge1$CI <- paste(merge1$CIb, ")", sep =  "")

merge1$`2.5 %` = NULL
merge1$`97.5 %` = NULL
merge1$CIa = NULL
merge1$CIb = NULL

merge1


## Add RI to table
ri$"Parameter" <- 
  c("Breeding Stage", 
    "Observation", 
    "Housing",
    "Sex", 
    "Weather", 
    "Intercept")

ri

merge2 <- merge(
  merge1, 
  ri, 
  by = "Parameter")

merge2

Table1 <- merge2[
  c(4, 2, 1, 5:9, 3, 10, 12, 11, 13), 
  c(1, 4, 2, 3, 5, 6)]


formattable(Table1)

write_csv(Table1, "Positive Welfare Model Avg Results Table.csv")


## 10.1 Post Hoc analysis of Trial_num ------------------------------------

# Unclear why Trial_num's RI = 1
# Due to a confounding variable?
# Or due to methodological error?
# Or perhaps the crows acclimating to the observer over time?

# Is the change in positive welfare linear (pointing to methodology)?

# First we'll plot the predicted positive welfare rates

# Create a new data frame with unique values of Trial_num and specific levels of other factors
prediction_data <- data.frame(
  Trial_num = factor(levels(NonStress_Enclosure_GLMM$Trial_num)),       # Unique levels of Trial_num
  Banked.Not = factor(levels(NonStress_Enclosure_GLMM$Banked.Not)[1]),  # Reference level for factors
  Weather = factor(levels(NonStress_Enclosure_GLMM$Weather)[1]),
  Food = factor(levels(NonStress_Enclosure_GLMM$Food)[1]),
  Sex = factor(levels(NonStress_Enclosure_GLMM$Sex)[1]),
  Breeding = factor(levels(NonStress_Enclosure_GLMM$Breeding)[1]),
  Time.Slots = factor(levels(NonStress_Enclosure_GLMM$Time.Slots)[1]),
  SCSB = factor(levels(NonStress_Enclosure_GLMM$SCSB)[1])  # Random effect set to a reference level
)

# Generate predicted values for each Trial_num level
prediction_data$predicted <- predict(GlobalModel, newdata = prediction_data, re.form = NA)

# Plot the predicted values for Trial_num
ggplot(prediction_data, aes(x = Trial_num, y = predicted)) +
  geom_line(aes(group = 1)) +  # Connect points with a line
  geom_point() +
  labs(x = "Trial Number", y = "Predicted sqrtNonStress") +
  theme_minimal()

# It's not linear, and likely due to a confounding variable

# Let's follow up this visual inspections with a pairwise analysis
emmeans_results <- 
  emmeans(GlobalModel, pairwise ~ Trial_num)

pairwise_summary <- 
  as.data.frame(
    summary(emmeans_results))

pairwise_summary_clean <- 
  pairwise_summary[c(7:21), c(2,4,5,8:10)]

formattable(pairwise_summary_clean)
write_csv(pairwise_summary_clean, "Observation Order Post Hoc Pairwise Analysis Results Table.csv")

# Significant pairwise contrasts for observation 1 vs 4,5,6
# But non-significant contrasts between many adjacent trials (1 vs 2, 4 vs 5, etc.)

# Increase in avg positive welfare rate is more noticeable in a broader sense 
# (e.g., comparing 1 to 4,5,6) rather than between consecutive trials.

# When looking at our confounding variables, we can see that rain follows a similar patter
# Observations 1-3 had rain and 4-6 did not
# Observation 1 had the most rain 
# We've included a plot for the SI below (10.2.4)

# Rain is the most likely culprit as to why Trial_num's RI = 1
# This show how important capturing weather as a confounding variable is for our study

# The variation of weather, particularly rain, has been accounted for in our model,
# such that our other results (Breeding RI = 1) is even more reliable.


## 10.2 GLMM Plots ----------------------------------------------------


### 10.2.1 Observation Order Box Plot --------------------------------------------------

ggplot(
  NonStress_Enclosure_GLMM, 
  aes(
    Trial_num, 
    NonStress.Rate)) +
  geom_boxplot() + 
  theme_light() +
  labs(
    y = "Rate of Positive Welfare-Indicating Behavior (per bird, per min)", 
    x = "Observation") +
  theme(
    axis.title = element_text(size = 12), 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))


### 10.2.2 Non-Stress & Breeding Box Plot -------------------------------------------

# Change Category Names

# aNo to Not Breeding
NonStress_Enclosure_GLMM$Breeding <- 
  as.character(NonStress_Enclosure_GLMM$Breeding)

NonStress_Enclosure_GLMM$Breeding <- 
  ifelse(
    NonStress_Enclosure_GLMM$Breeding == "aNo",
    "Not Breeding",
    NonStress_Enclosure_GLMM$Breeding)

NonStress_Enclosure_GLMM$Breeding <- 
  as.factor(NonStress_Enclosure_GLMM$Breeding)


# Sit to Incubating
NonStress_Enclosure_GLMM$Breeding <- 
  as.character(NonStress_Enclosure_GLMM$Breeding)

NonStress_Enclosure_GLMM$Breeding <- 
  ifelse(
    NonStress_Enclosure_GLMM$Breeding == "Sit",
    "Incubating",
    NonStress_Enclosure_GLMM$Breeding)

NonStress_Enclosure_GLMM$Breeding <- 
  as.factor(NonStress_Enclosure_GLMM$Breeding)


# Chick to Nestling Rearing
NonStress_Enclosure_GLMM$Breeding <- 
  as.character(NonStress_Enclosure_GLMM$Breeding)

NonStress_Enclosure_GLMM$Breeding <- 
  ifelse(NonStress_Enclosure_GLMM$Breeding == "Chick",
         "Nestling Rearing",
         NonStress_Enclosure_GLMM$Breeding)

NonStress_Enclosure_GLMM$Breeding <- 
  as.factor(NonStress_Enclosure_GLMM$Breeding)


# Change levels for boxplot ordering 
NonStress_Enclosure_GLMM$Breeding <- 
  factor(
    NonStress_Enclosure_GLMM$Breeding, 
    levels = c(
      "Not Breeding", 
      "Incubating", 
      "Nestling Rearing"))


# Plot
ggplot(
  NonStress_Enclosure_GLMM, 
  aes(
    Breeding, 
    NonStress.Rate)) +
  geom_boxplot() + 
  theme_bw() +
  labs(
    y = "Rate of Positive Welfare-Indicating Behavior (per bird, per min, per observation)", 
    x = "Breeding Stage") +
  theme(
    axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 9), 
    axis.text.y = element_text(size = 10))



### 10.2.3 Non-Stress Rates vs Housing Plot ----------------------------------------------

# Housing is ~82%, worth consideration for longer studies
# Higher welfare in banked, but could be because not incubating/chick rearing
# Would want to have a similar study outside of the breeding season for a proper comparison


# Change Category Names

# Not to Breeding Aviary
NonStress_Enclosure_GLMM$Banked.Not <- 
  as.character(NonStress_Enclosure_GLMM$Banked.Not)

NonStress_Enclosure_GLMM$Banked.Not <- 
  ifelse(
    NonStress_Enclosure_GLMM$Banked.Not == "Not",
    "Breeding Aviary",
    NonStress_Enclosure_GLMM$Banked.Not)

NonStress_Enclosure_GLMM$Banked.Not <- 
  as.factor(NonStress_Enclosure_GLMM$Banked.Not)

# Banked to Holding Aviary
NonStress_Enclosure_GLMM$Banked.Not <- 
  as.character(NonStress_Enclosure_GLMM$Banked.Not)

NonStress_Enclosure_GLMM$Banked.Not <- 
  ifelse(
    NonStress_Enclosure_GLMM$Banked.Not == "Banked",
    "Holding Aviary",
    NonStress_Enclosure_GLMM$Banked.Not)

NonStress_Enclosure_GLMM$Banked.Not <- 
  as.factor(NonStress_Enclosure_GLMM$Banked.Not)


# Plot

ggplot(
  NonStress_Enclosure_GLMM, 
  aes(
    Banked.Not, 
    NonStress.Rate)) +
  geom_boxplot() + 
  theme_bw() +
  labs(y = "Positive Welfare Behavior Rate (per bird, per min, per observation)") + 
  theme(axis.title.x = element_blank()) +
  theme(
    axis.title = element_text(size = 12), 
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 10)
    )


#### 10.2.3.1 Combined Plots ----------------------------------------------------------

# Create combined plot Figure 1a and 1b due to same Y axis

# Breeding Stage Plot
plot1 <- ggplot(
  NonStress_Enclosure_GLMM, 
  aes(
    Breeding, 
    NonStress.Rate)) +
  geom_boxplot() + 
  ggtitle("a. Breeding Stage") + 
  theme_bw() +
  ylab("Rate of Positive Welfare-Indicating Behavior (per bird, per min, per observation)") + 
  theme(
    axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10),
  )

# Housing Plot
plot2 <- ggplot(
  NonStress_Enclosure_GLMM, 
  aes(
    Banked.Not, 
    NonStress.Rate)) +
  geom_boxplot() + 
  theme_bw() +
  ggtitle("b. Housing Setup") + 
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10)
    )


# Arrange the plots side by side with grid.arrange
grid.arrange(plot1, plot2, ncol = 2)


### 10.2.4 Weather vs Observation Plot ------------------------------------------------------------

# Visual aid for our poct hoc Trial-num result interpretation

## Proportion bar plot

# Reorder the levels of Weather for stacking order
NonStress_Enclosure_GLMM$Weather <- 
  factor(
    NonStress_Enclosure_GLMM$Weather, 
    levels = c("aS", "O", "M", "R"))

ggplot(
  NonStress_Enclosure_GLMM,
  aes(
    x = Trial_num,
    fill = Weather)) +
  geom_bar(position = "fill") + # Normalizes bars to show proportions
  labs(
    x = "Observation", 
    y = "Proportion", 
    fill = "Weather") +
  scale_fill_viridis(
    discrete = TRUE,
    option = "D", 
    direction = -1,
    labels = c("Sunny","Overcast","Mist","Rain")) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  theme_minimal()



# 11. Personality, Enclosure, Non-Stress GLM ----------------------------------

# Question: Do personality trait and enclosure interactions affect non-stress behavior frequency? 

# Hypothesis: Within banked aviaries, bolder, more active, less social, and more aggressive birds exhibit lower non-stress behavior frequencies. 

# Different contexts for personalities but method design helps control confounding variables

# Take mean score for each personality trait for each bird
Personality_GLM_allbirds <- 
  housing.rate.confounding [,c(1, 6, 9:12, 16)]

## Remove moving pair - housing type can't average
Personality_GLM_allbirds$SCSB <- as.factor(Personality_GLM_allbirds$SCSB)

Personality_GLM <- 
  Personality_GLM_allbirds[
    -c(
      which(
        Personality_GLM_allbirds$SCSB == "AL184" |
          Personality_GLM_allbirds$SCSB == "AL124")),]


# New data frame for averages 
MeanPers_GLM <- 
  data.frame(
    "SCSB" = unique(Personality_GLM$SCSB), 
    "Banked.Not" = NA, 
    "Sex" = NA, 
    "MeanAgg" = NA, 
    "MeanSoc" = NA,
    "MeanNonActivity" = NA, 
    "MeanBold" = NA, 
    "MeanNonStress" = NA)

MeanPers_GLM$Banked.Not <- 
  Personality_GLM[
    which(
      !duplicated(
        Personality_GLM$SCSB)), "Banked/Not"]

MeanPers_GLM$Sex <- 
  Personality_GLM[
    which(
      !duplicated(
        Personality_GLM$SCSB)), 
    "Sex"]


# Aggression means
All_agg_mean_birds <- 
  tapply(
    Personality_GLM$Aggress.Sub.Rate, 
    Personality_GLM$SCSB, 
    mean)

noNA_agg_mean <- 
  All_agg_mean_birds[!is.na(All_agg_mean_birds)]

MeanPers_GLM$MeanAgg <- noNA_agg_mean


# Social Connectedness means
All_soc_mean_birds <- 
  tapply(
    Personality_GLM$Sociable.Rate, 
    Personality_GLM$SCSB, 
    mean)

noNA_soc_mean <- 
  All_soc_mean_birds[!is.na(All_soc_mean_birds)]

MeanPers_GLM$MeanSoc <- noNA_soc_mean


# Activity means
All_activity_mean_birds <- 
  tapply(
    Personality_GLM$Rest.Rate, 
    Personality_GLM$SCSB, 
    mean)

noNA_act_mean <- 
  All_activity_mean_birds[!is.na(All_activity_mean_birds)]

MeanPers_GLM$MeanNonActivity <- noNA_act_mean


# Boldness mean
All.keeper.clean.25 <- 
  All.keeper.clean[-c(
    which(
      All.keeper.clean$SCSB == "AL184" | 
        All.keeper.clean$SCSB == "AL124")),]

All_bold_mean_birds <- 
  tapply(
    All.keeper.clean.25$Boldness, 
    All.keeper.clean.25$SCSB, 
    mean)

noNA_bold_mean <- 
  All_bold_mean_birds[!is.na(All_bold_mean_birds)]

MeanPers_GLM$MeanBold <- noNA_bold_mean


# Non-stress mean
All_nonstress_birds <- 
  tapply(
    Personality_GLM$NonStress.Rate, 
    Personality_GLM$SCSB, 
    mean)

noNA_nonstress <- 
  All_nonstress_birds[!is.na(All_nonstress_birds)]

MeanPers_GLM$MeanNonStress <- noNA_nonstress


### Ensure personalities aren't skewed to one enclosure type

## Create histograms per personality per aviary type
bank_birds <- MeanPers_GLM[MeanPers_GLM$Banked.Not == "Banked",]
notb_birds <- MeanPers_GLM[MeanPers_GLM$Banked.Not == "Not",]


## Sociability
hist(bank_birds$MeanSoc)
hist(notb_birds$MeanSoc)
# No skew


## Boldness
hist(bank_birds$MeanBold) #don't even have a 0 option, all banked birds are showing boldness
hist(notb_birds$MeanBold)


# Lets look a little closer with a boxplot of just boldness vs housing
boldness_housing <- MeanPers_GLM[,c("Banked.Not","MeanBold")]
str(boldness_housing)
boldness_housing$Banked.Not <- as.factor(boldness_housing$Banked.Not)
plot(boldness_housing) 
# Housing and Boldness are correlated, 
# Always bold behavior in banked, and bigger range in non-banked

# REMOVE BOLDNESS


## (Non) Activity
hist(bank_birds$MeanNonActivity)
hist(notb_birds$MeanNonActivity)
# Small skew


# Aggression
hist(bank_birds$MeanAgg)
hist(notb_birds$MeanAgg)
# Skew due to single begging female, ignore and continue use


# Make the global model and use VIF test for structural multicollinearity
# If VIF < 5, keep the predictors. 
# If not, remove them, rerun the model, and repeat VIF analysis

## Is mean non-stress norm?
hist(MeanPers_GLM$MeanNonStress) # Yes

## Standardize predictors before running model (See Grueber et al. 2011)

MeanPers_GLM$MeanSoc.z <- rescale(MeanPers_GLM$MeanSoc)
MeanPers_GLM$MeanNonActivity.z <- rescale(MeanPers_GLM$MeanNonActivity)
MeanPers_GLM$MeanAgg.z <- rescale(MeanPers_GLM$MeanAgg)
MeanPers_GLM$Banked.Not.z <- rescale(MeanPers_GLM$Banked.Not)
MeanPers_GLM$Sex.z <- rescale(MeanPers_GLM$Sex)


## Run global model
Personality_model1 <- 
  glm(MeanNonStress ~ 
        MeanSoc.z + 
        MeanNonActivity.z + 
        MeanAgg.z + 
        Banked.Not.z + 
        Sex.z + 
        MeanSoc.z:Banked.Not.z + 
        MeanNonActivity.z:Banked.Not.z + 
        MeanAgg.z:Banked.Not.z, 
      family = gaussian, 
      data = MeanPers_GLM)

## Run VIF 
require(car)

vif(Personality_model1)

#      MeanSoc.z              MeanNonActivity.z                      MeanAgg.z                   Banked.Not.z 
#      1.982143                       1.808854                       1.769896                       2.016777 
#      Sex.z                 MeanSoc.z:Banked.Not.z     MeanNonActivity.z:Banked.Not.z         MeanAgg.z:Banked.Not.z 
#      1.628342                       1.748961                       1.219642                       1.531592 

## Keep all predictors, all VIF < 5,Belsley et al. (1980) 


## Generate submodels from your global model
options(na.action = "na.fail") 
m2.submodels <- dredge(Personality_model1)

# Select top submodels based on a delta AICc score of < 2 
m2.topmodels <- 
  get.models(m2.submodels, 
             subset = delta <= 2) 

# 2 models within 2 AICc of the best (average them)

# Export top submodels for supplementary material

m2.selection <- 
  model.sel(m2.topmodels)

m2.selection$model_id <- 
  row.names(m2.selection)

m2.selection$formula <- NA

m2.sel_subset_TABLE <- 
  data.frame(m2.selection) %>% 
    dplyr::select(model_id, 
                  df, 
                  logLik, 
                  AICc, 
                  delta, 
                  weight, 
                  formula)

# Use a loop to pull out the formulas from each top model
v <- character() 
list_len <- length(m2.topmodels)
for(i in 1:list_len)
  v <- c(v, m2.topmodels[[`i`]]$formula) 
print(v) ## each formula has +1, remove in excel


# Append the model formulas in v to the m2.sel_subset_TABLE dataframe
m2.sel_subset_TABLE$formula <- v


## Format
m2.sel_subset_TABLE <- 
  m2.sel_subset_TABLE %>%
  mutate(formula = as.character(formula)) %>%
  as.data.frame()

m2.submodel_results <- m2.sel_subset_TABLE
m2.submodel_results #Supplemenary materials export

write_csv(m2.submodel_results, "Personality Top Models Table.csv")

# Average parameter estimates over top model set 
glm.modelavg <- model.avg(m2.topmodels)
summary(glm.modelavg) 
print(glm.modelavg$sw)

# Calculate 95% confidence intervals for each parameter estimate
(ci <- round(confint(glm.modelavg),2))
ci <- as.data.frame(ci)
ci$Variable <- row.names(ci)

# Extract parameter estimates & their standard errors
coeff  <- coefTable(glm.modelavg)
coeff <-as.data.frame(coeff)
coeff$Variable <- row.names(coeff)

# Extract relative importance scores
ri <- glm.modelavg$sw
ri <- as.data.frame(ri)
ri$Variable <- row.names(ri) 

merge1 <- merge(coeff, 
                ci, 
                by = "Variable", 
                all = TRUE)

merge2 <- merge(merge1, 
                ri, 
                by = "Variable", 
                all.x = TRUE)

vif <- as.data.frame(vif(Personality_model1))
vif$Variable <- rownames(vif)

merge3 <- 
  merge(
    merge2, 
    vif, 
    by = "Variable", 
    all.x = TRUE)

# Clean up table
merge3

merge3$df = NULL

merge3$Variable <- 
  c("Intercept", 
    "Housing", 
    "Housing:Mean Restfulness", 
    "Mean Restfulness",
    "Mean Social Connectedness",
    "Sex")

## Check
merge3  ## Supplementary materials export

write_csv(merge3, "Personality Model Avg Results Table.csv")

plot(Personality_model1) # residuals normal


## 11.1 GLM Plots  ------------------------------------------------------------------

# Need to use averaged model data
# Non-standardized data used for ease of interpretation

## Extract predicted values from averaged model

pred <- 
  predict(
    glm.modelavg, 
    MeanPers_GLM, 
    se.fit = TRUE, 
    type = "response")

preddf <- 
  data.frame(
    "fit" = c(pred$fit), 
    "se.fit" = c(pred$se.fit))

MeanPers_GLM$id <- 1:50

preddf$id <- 1:50

MeanPers_GLM <- 
  merge(
    MeanPers_GLM, 
    preddf, 
    by = "id")

# Change Not to Breeding
MeanPers_GLM$Banked.Not <- 
  as.character(
    MeanPers_GLM$Banked.Not)

MeanPers_GLM$Banked.Not <- 
  ifelse(
    MeanPers_GLM$Banked.Not == "Not", 
    "Breeding Aviary", 
    MeanPers_GLM$Banked.Not)

MeanPers_GLM$Banked.Not <- 
  as.factor(
    MeanPers_GLM$Banked.Not)

# Change Banked to Holding Aviary
MeanPers_GLM$Banked.Not <- 
  as.character(
    MeanPers_GLM$Banked.Not)

MeanPers_GLM$Banked.Not <- 
  ifelse(
    MeanPers_GLM$Banked.Not == "Banked", 
    "Holding Aviary", 
    MeanPers_GLM$Banked.Not)

MeanPers_GLM$Banked.Not <- 
  as.factor(
    MeanPers_GLM$Banked.Not)


### 11.1.1 Housing vs Mean Non-Stress Plot ----------------------------------------------
# RI = 1.0

ggplot(
  MeanPers_GLM, 
  aes(
    Banked.Not, 
    MeanNonStress)) +
  geom_boxplot() + 
  theme_bw() + 
  labs(
    y = "Mean Rate of Positive Welfare-Indicating Behavior (per bird, per minute)", 
    x = "Housing Setup") + 
  theme(
    axis.title = element_text(size = 10), 
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 9))

# More non-stress in banked housing


### 11.1.2 Sociability vs Non-Stress Plot ------------------------------------------
# RI = 1.0

ggplot(
  MeanPers_GLM, 
  aes(
    x = MeanSoc, 
    y = fit)) +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    colour = "black", 
    fill = "#D6DBDF",
    size = .3) +
  labs(
    x = 'Mean Rate of Social Connectedness Behavior (per bird, per min)', 
    y = 'Mean Rate of Positive Welfare-Indicating Behavior (per bird, per min)') +
  geom_point(
    data = MeanPers_GLM,
    aes(
      x = MeanSoc, 
      y = MeanNonStress),
    colour = "#566574", 
    inherit.aes = FALSE) + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank()) 


### 11.1.3 NonActive:BankedNot Plot ------------------------------------------------
# RI = 0.85

ggplot(
  MeanPers_GLM, 
  aes(
    x = MeanNonActivity, 
    y = fit, 
    group = Banked.Not)) +
  geom_smooth(
    aes(linetype = Banked.Not), 
    se = TRUE, 
    colour = "black", 
    fill = "#D6DBDF",
    size = .3) +
  labs(
    x = 'Mean Rate of Restful Behavior (per bird, per min)', 
    y = 'Mean Rate of Positive Welfare-Indicating Behavior (per bird, per min)') +
  geom_point(
    data = MeanPers_GLM,
    aes(
      x = MeanNonActivity, 
      y = MeanNonStress, 
      shape = Banked.Not),
    size = 2, 
    colour = "#566573", 
    inherit.aes = FALSE) + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank()) 

