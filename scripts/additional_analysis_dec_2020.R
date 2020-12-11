## December 2020: Additional analysis request
## Following https://osf.io/kr2g9/
## Goal: extract point estimates and predicted values from my model

# Packages

library(lme4)
library(lmerTest)
library(dplyr)

# Data
#
# Missing data: I found and replaced '.' with "NA" in excel before importing.
# -rear_nest_breed_id 203455 had one nestling from year 2002, which was corrected to 2003
# -rear_nest_breed_id 203182 was listed as 4 rear_Cs_at_start_of_rearing; corrected to 5
# -rear_nest_breed_id 201105, rear_Cs listed as NA; corrected to 8.

tit <- read.csv("data/blue_tit_data_updated_2020-04-18.csv", stringsAsFactors = FALSE)

# Turn these integer columns into factors
tit <- tit %>%  mutate_at(
  vars(
    hatch_year,
    Extra.pair_paternity,
    rear_nest_trt,
    home_or_away,
    chick_sex_molec,
    chick_survival_to_first_breed_season,
    rear_mom_Ring,
    rear_dad_Ring,
    hatch_mom_Ring,
    genetic_dad_ring_.WP_or_EP.,
    rear_area,
    rear_Box
  ),
  as.factor
)
tit$rear_nest_trt <- relevel(tit$rear_nest_trt, ref = "7")

# Predictor data
# Small but annoying issue with predictor data: warning "incomplete final line found by readTableHeader"
# Solution: save as tsv (.txt), open in text editor, place cursor on last character
# and press "enter."
prediction_data <-read.table("data/blue_tit_percentiles_for_supplement_wide.txt",
                             header =T,
                             stringsAsFactors = FALSE)
prediction_data <- prediction_data  %>%  mutate_at(
                                              vars(
                                                hatch_year,
                                                Extra.pair_paternity,
                                                rear_nest_trt,
                                                home_or_away,
                                                chick_sex_molec,
                                                chick_survival_to_first_breed_season,
                                                rear_mom_Ring,
                                                rear_dad_Ring,
                                                hatch_mom_Ring,
                                                genetic_dad_ring_.WP_or_EP.,
                                                rear_area,
                                                rear_Box
                                                ),
                                              as.factor)
prediction_data$rear_nest_trt <- relevel(prediction_data$rear_nest_trt, ref = "7")
predictin_data <- prediction_data[1:3,]
prediction_data_fixed <- prediction_data[2:4,]
prediction_data_fixed <- droplevels(prediction_data_fixed)

names(prediction_data) == names(tit) # make sure columns are the same

# main model

main_mod <-  lmer(
  day_14_weight ~
    rear_Cs_at_start_of_rearing +
    hatch_nest_LD +
    home_or_away +
    chick_sex_molec +
    (1 | hatch_year) + (1 | rear_nest_breed_ID) +
    (1 | rear_mom_Ring) + (1 | rear_dad_Ring) +
    (1 | hatch_mom_Ring) + (1 | genetic_dad_ring_.WP_or_EP.) +
    (1 | rear_area / rear_Box),
  data = tit #fit data using data, excluding the test set (at end)
) #%>% summary




# Check names: TRUE
all(names(tit %>% dplyr::select(-chick_ring_number))  %in%
      names(prediction_data %>%  dplyr::select(-scenario)))
all(names(prediction_data %>% dplyr::select(-scenario)) %in%
      names(tit)) # TRUE

# Predict data

predict(main_mod,
        type = "link",
          newdata = prediction_data) # generates error

predict(main_mod,
        type = "link",
        newdata = prediction_data_fixed)

predict(main_mod,
        type = "link",
        newdata = prediction_data, allow.new.levels =T ) # fix error



# Check predictors in train and test dfs to make sure levels match
modeldf <- main_mod@frame
list_predictors <- names(modeldf)
sapply(list_predictors, function(x) class(prediction_data[,x]) == class(modeldf[,x])) # classes of all variables is the same
sapply(list_predictors, function(x) levels(prediction_data[,x]) %in% levels(modeldf[,x])) # levels match; what's the problem??

# Check all columns just in case
allnames <- names(tit %>% dplyr::select(-chick_ring_number))
sapply(allnames, function(x) levels(prediction_data[,x]) %in% levels(tit[,x])) # levels match; what's the problem??
sapply(allnames, function(x) class(prediction_data[,x]) == class(tit[,x])) # classes of all variables is the same

