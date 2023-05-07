#-------------------------------------------------------------------------------
# SEXUAL VIOLENCE AS A TACTIC FOR TERRITORIAL CONTROL
# REPLICATION DATA: MODELS
# ALEX AVERY
# LAST UPDATED: 05/07/23
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# load packages
library(plm)
library(sandwich)
library(lmtest)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# VARIABLES
#-------------------------------------------------------------------------------
# independent variables: total_terr_event, total_gov_control, total_nsa_control
# dependent variables: total_crsv_event, total_gov_perp, total_nsa_perp
# control variables: total_acled_fat
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MODELS
#-------------------------------------------------------------------------------
citymodel1 <- plm(total_crsv_event ~ total_terr_event + total_acled_fat,
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))



city_model <- plm(total_nsa_perp ~ total_gov_control + total_acled_fat, 
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(city_model, vcov. = vcovHC(city_model, typw = 'HC2'))

city_model <- plm(total_gov_perp ~ total_nsa_control + total_acled_fat, 
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(city_model, vcov. = vcovHC(city_model, typw = 'HC2'))


#-------------------------------------------------------------------------------

