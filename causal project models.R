#-------------------------------------------------------------------------------
# SEXUAL VIOLENCE AS A TACTIC FOR TERRITORIAL CONTROL
# REPLICATION DATA: MODELS
# ALEX AVERY
# LAST UPDATED: 05/08/23
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
# dependent variables: total_gov_perp, total_nsa_perp
# control variables: total_acled_fat
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODELS 
#-------------------------------------------------------------------------------
citymodel1 <- plm(total_gov_perp ~ total_terr_event + total_acled_fat,
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(citymodel7, vcov. = vcovHC(citymodel7, type = 'HC2'))

# SIGNIFANCE!!!!!!!!
citymodel2 <- plm(total_gov_perp ~ total_gov_control + total_acled_fat,
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(citymodel8, vcov. = vcovHC(citymodel8, type = 'HC2'))

citymodel3 <- plm(total_gov_perp ~ total_nsa_control + total_acled_fat,
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(citymodel9, vcov. = vcovHC(citymodel9, type = 'HC2'))

citymodel4 <- plm(total_nsa_perp ~ total_terr_event + total_acled_fat,
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(citymodel10, vcov. = vcovHC(citymodel10, type = 'HC2'))

citymodel5 <- plm(total_nsa_perp ~ total_gov_control + total_acled_fat,
                  data = drc_cities_df,
                  model = 'within',
                  index = c('city_id', 'month_date'))
coeftest(citymodel11, vcov. = vcovHC(citymodel11, type = 'HC2'))

citymodel6 <- plm(total_nsa_perp ~ total_nsa_control + total_acled_fat,
                   data = drc_cities_df,
                   model = 'within',
                   index = c('city_id', 'month_date'))
coeftest(citymodel12, vcov. = vcovHC(citymodel12, type = 'HC2'))


