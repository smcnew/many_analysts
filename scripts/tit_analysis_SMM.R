# Many Analysts Project
# Evolutionary Ecology Study
# Main q: "To what extent is the growth of
# blue tits influenced by competiton?"
#
# Experiment: Cross-fostering to manipulate brood
# sizes.
# Treatment: "net_rearing_manipulation" = brood size
# after manipulation, "rear_nest_trt"
# Response variables:
# number_chicks_fledged_from_rear_nest,
# day_14_tarsus_length
# day_14_weight
# chick_survival_to_first_breed_season
# Covariates:
# Year, lay date, parent, day14_measurer,
# chick_sex_molec, hatch area, hatch box, home_or_away
#
library(reshape2)
library(lme4)
library(dplyr)
library(lmerTest)
library(cAIC4)
library(MuMIn)
# Data
# Missing data: I found and replaced '.' with "NA" in excel
# before importing.
#-rear_nest_breed_id 203455 had one nestling from year 2002, which was corrected to 2003
#-rear_nest_breed_id 203182 was listed as 4 rear_Cs_at_start_of_rearing; corrected to 5
#-rear_nest_breed_id 201105, rear_Cs listed as NA;
#corrected to 8.

tit <- read.csv("data/blue_tit_data_updated_2020-04-18.csv")

# Turn these integer columns into factors
tit <- tit %>%  mutate_at(vars(hatch_year,
                        Extra.pair_paternity,
                        rear_nest_trt,
                        home_or_away,
                        chick_sex_molec,
                        chick_survival_to_first_breed_season

), as.factor)
tit$rear_nest_trt <- relevel(tit$rear_nest_trt, ref = "7")


# Data Exploration --------------------------------------------------------

dim(tit)
unique(tit$chick_ring_number) %>% length() #each row is a unique nestling
table(tit$hatch_year) #1000 - 1400 chicks over three years
unique(tit$hatch_nest_breed_ID) %>% length() # ~ 500 nests
summary(tit$hatch_nest_CS) #original clutch sizes: 2-16, mean = 10.5
summary(tit$net_rearing_manipulation) # up to four babies moved among nests
table(tit$rear_nest_trt) #1700 chicks more chicks (5), 1000 chicks decreased (6) control (no manipulation)
summary(tit$hatch_Area) # 9 hatch areas, between 90 and 700 nestlings each

# Zuur-style checks for weird data (based on Zuur et al. 2010 Methods Ecol Evol)
# 1. Look for outliers in xs and ys using boxplots and cleveland dotplots
# Identify key variables
response <- c(
  "number_chicks_fledged_from_rear_nest",
  "day_14_tarsus_length",
  "day_14_weight",
  "chick_survival_to_first_breed_season"
)
par(mfrow = c(4,2))
for(i in 1:length(response)){
  tit2 <- tit
  tit2$chick_survival_to_first_breed_season <- as.numeric(as.character(tit$chick_survival_to_first_breed_season))
  dplyr::select(tit2, response[i]) %>% boxplot(., main = response[i]) #boxplot
  dplyr::select(tit2, response[i]) %>%
    pull(.) %>% plot (x = ., y = 1:length(.), ylab="order of data", xlab= response[i]) #Dotplot index ~ value
}

# Response variables look pretty good, decent spread, few outliers. One baby has a very small tarsus ( < 13)

# Now check predictors, including treatment variables and covariates
predictors <- c(
  "net_rearing_manipulation",
  "rear_nest_trt",
  "rear_Cs_at_start_of_rearing",
  "d14_rear_nest_brood_size",
  "hatch_nest_LD"
)
par(mfrow = c(5,2), mar=c(4,2,2,0))
for(i in 1:length(predictors)){
  tit2 <- tit
  tit2$rear_nest_trt <- as.numeric(as.character(tit2$rear_nest_trt))
  dplyr::select(tit2, predictors[i]) %>% boxplot(., main = predictors[i]) #boxplot
  dplyr::select(tit2, predictors[i]) %>%
    pull(.) %>% plot (x = ., y = 1:length(.), ylab="order of data", xlab= predictors[i]) #Dotplot index ~ value
}

dplyr::select(tit, rear_nest_trt) %>% pull %>% as.character %>% as.numeric %>% boxplot()
# weird patterns in LD because sheet was sorted in chronological order

# 2. Look for homogeneity of variance: sure, looks like trt 5 is smaller than others
dev.off()
boxplot(day_14_tarsus_length ~ rear_nest_trt, tit) #boxes are about the same size, more outliers in 5
boxplot(day_14_weight ~ rear_nest_trt, tit) #boxes are about the same size, more outliers in 5

# 3. Check for normality, why not
qqnorm(tit$day_14_tarsus_length[tit$rear_nest_trt==5]) # 5 and 7 not super normal but let's move on
qqline(tit$day_14_tarsus_length[tit$rear_nest_trt==5])


# Check for covariance among predictors
head(tit)
tit %>% select(predictors, response) %>% pairs() # clutch, brood, and fledglings correlated


# Ok I'm satisfied the data are not super wonky.


# Analysis ----------------------------------------------------------------

response <- c(
  "number_chicks_fledged_from_rear_nest",
  "day_14_tarsus_length",
  "day_14_weight",
  "chick_survival_to_first_breed_season"
)
predictors <- c(
  "net_rearing_manipulation",
  "rear_nest_trt",
  "rear_Cs_at_start_of_rearing",
  "d14_rear_nest_brood_size",
  "hatch_nest_LD"
)

names(tit)

# Make some linear models to analyze different measures of success
# 1. Weight and tarsus of d. 14 chicks
# 2. Chick survival to next season
#
#
# Fixed effects: rear_nest_trt,  hatch_nest_LD,
# home_or_away, hatch_nest_CS, sex (maybe interacted?), hatch_nest_CS
# (Judgement call: include rear_Cs_at_start_of_rearing? NO for now, because it's a post-treatment variable)
# ALL THE RANDOM EFFECTS: ids of all parents, hatch_year, rear_Box within rear_area
# rearing brood


head(tit)
# Effects on mass:
# Males are bigger, transplanted chicks are bigger (asynch hatch?)
# Early nesters do better, negative effect of clutch size, Significant (ish)
# effect of treatment (for increased but not decreased brood size)
summary(
  lmer(
    day_14_weight ~
      rear_nest_trt +
      hatch_nest_LD +
      home_or_away +
      hatch_nest_CS +
      chick_sex_molec +
      (1 | hatch_year) + (1 | rear_nest_breed_ID) +
      (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
      (1 | hatch_mom_Ring) + (1 | genetic_dad_ring_.WP_or_EP.) +
      (1 | rear_area / rear_Box),
    data = tit
  )
)

summary (lmer(
    day_14_tarsus_length ~
      rear_nest_trt +
      #hatch_nest_LD + #not sig
      home_or_away +
      #hatch_nest_CS + #not sig
      chick_sex_molec +
      (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
      (1 | hatch_mom_Ring) + (1 | genetic_dad_ring_.WP_or_EP.) +
      (1 |hatch_year) +(1|rear_nest_breed_ID) +
      (1 | hatch_Area / hatch_Box), data = tit
  )
)


summary(
  glmer(
    chick_survival_to_first_breed_season ~
      rear_nest_trt +
      hatch_nest_LD +
      #home_or_away +
      #hatch_nest_CS +
      #chick_sex_molec +
      (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
      (1 | hatch_mom_Ring) + (1 | genetic_dad_ring_.WP_or_EP.) +
      (1 | hatch_year) + (1 | rear_nest_breed_ID) +
      (1 | hatch_Area / hatch_Box),
    data = tit,
    family = "binomial"
  )
)


# Analysis by brood size -------------------------------------------------
# The treatment randomizes and maximizes variation in clutch size. So let's just use
# that as a covariate instead of treatment

### MAIN MODEL FOR USE IN PREDICTION ###
main_mod <-  lmer(
    day_14_weight ~
      rear_Cs_at_start_of_rearing +
      hatch_nest_LD +
      home_or_away +
      #hatch_nest_CS + #not significant
      chick_sex_molec +
      (1 | hatch_year) + (1 | rear_nest_breed_ID) +
      (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
      (1 | hatch_mom_Ring) + (1 | genetic_dad_ring_.WP_or_EP.) +
      (1 | rear_area / rear_Box),
    data = tit
  ) #%>% summary




summary (lmer(
  day_14_tarsus_length ~
    #rear_Cs_at_start_of_rearing +
    #hatch_nest_LD + #not sig
    home_or_away +
    #hatch_nest_CS + #not sig
    chick_sex_molec +
    (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
    (1 | hatch_mom_Ring) + (1 | genetic_dad_ring_.WP_or_EP.) +
    (1 |hatch_year) +(1|rear_nest_breed_ID) +
    (1 | hatch_Area / hatch_Box), data = tit
)
)



# By nest analysis  -------------------------------------------------------


# Lets reshape the data so we have one row per nest, to evaluate total fledging success
head(tit)
nest <- dplyr::select(tit, hatch_year, rear_nest_breed_ID, rear_area, rear_Box, rear_mom_Ring, rear_dad_Ring,
               rear_nest_trt, rear_nest_LD, rear_nest_CS,rear_d0_rear_nest_brood_size,
               rear_Cs_out, rear_Cs_in, net_rearing_manipulation, rear_Cs_at_start_of_rearing,
               d14_rear_nest_brood_size, number_chicks_fledged_from_rear_nest) %>% distinct()
nest$died <- nest$rear_Cs_at_start_of_rearing - nest$number_chicks_fledged_from_rear_nest #change in brood size from start to d 14; i.e. dead nestlings

nest[duplicated(nest$rear_nest_breed_ID),] # no duplicates = no issues in data entry
head(nest)

# Variance in mass

head(tit)
variances <- aggregate(day_14_weight ~ rear_nest_breed_ID, tit, var)
variances_tar <- aggregate(day_14_tarsus_length ~ rear_nest_breed_ID, tit, var)
variances <- merge(variances, variances_tar)
names(variances)[2:3] <- c("weight_var", "tarsus_var")
nest <- merge(nest,variances)
head(nest)


# Can we predict that competition will mean more variance in size?
# But what if variance in size naturally increases with clutch size?
pvals <- NULL
for (i in 1:1000) {
  ns <- seq(from = 4, to = 16) %>% rep(.,10)
  ys <- lapply(ns, function(x) rnorm(x)) %>% lapply(., function(x) var(x)) %>% unlist()
  pvals[i] <- summary(lm(ys ~ ns))$coefficients[2,4]
}
hist1 <- hist(pvals) #looks pretty uniform (i.e. no consistent relationship between n and var, so let's forge ahead)


summary(lmer(weight_var~ rear_nest_trt +
               rear_nest_LD +
               (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
               (1 | hatch_year) +
               (1 | rear_area / rear_Box),
             data = nest,))

summary(lmer(tarsus_var~ rear_nest_trt +
               #rear_nest_LD +
               (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
               (1 | hatch_year) +
               (1 | rear_area / rear_Box),
             data = nest,))


# Test fledging success
summary(glmer(
  cbind(number_chicks_fledged_from_rear_nest, died) ~ rear_nest_trt +
    #rear_nest_LD +
    (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
    (1 | hatch_year) +
    (1 | rear_area / rear_Box),
  data = nest,
  family = "binomial"
))
head(nest)
aggregate(died ~ rear_nest_trt, data = nest, mean) #more nestlings died in the increased treatment
# Useful Plots ------------------------------------------------------------

# Mass vs. Brood size
dev.off()

# Broods that started off bigger had smaller nestlings by day 14
plot(day_14_weight ~ rear_Cs_at_start_of_rearing, tit, col = rear_nest_trt)
lm(day_14_weight ~ rear_Cs_at_start_of_rearing, tit) %>% abline() #summary()

plot(day_14_tarsus_length ~ rear_Cs_at_start_of_rearing, tit)
lm(day_14_tarsus_length ~ rear_Cs_at_start_of_rearing, tit) %>% abline() #summary


lm(day_14_weight ~ rear_Cs_at_start_of_rearing, tit[tit$rear_nest_trt==7,]) %>% summary()
# Make a boxplot to compare Day 0 and Day 14 brood sizes among treatments
# Both increased and controls (5 and 7) look like they decreased noticably,
# Smaller brood sizes didn't really look like they changed much.
select(nest, rear_nest_breed_ID, rear_Cs_at_start_of_rearing,  d14_rear_nest_brood_size, rear_nest_trt ) %>%
  melt(., id =c("rear_nest_breed_ID", "rear_nest_trt"))  %>% boxplot(value ~ variable + rear_nest_trt, data =. )

par(mfrow=c(1,2))
boxplot(tarsus_var ~ rear_nest_trt, data = nest)
boxplot(weight_var ~ rear_nest_trt, data = nest)
