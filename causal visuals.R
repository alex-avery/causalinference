#-------------------------------------------------------------------------------
# SEXUAL VIOLENCE AS A TACTIC FOR TERRITORIAL CONTROL
# REPLICATION DATA: VISUALS
# ALEX AVERY
# LAST UPDATED: 05/07/23
#-------------------------------------------------------------------------------

# load libraries
library(ggplot2)
library(stargazer)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MAPS
#-------------------------------------------------------------------------------

# ACLED DRC Events

ggplot() +
  geom_polygon(data = drc_prov_fort, mapping = aes(x = long, y = lat, group = group),
               color = 'black', alpha = 0) +
  geom_point(data = acled_drc, mapping = aes(x = acled_lon, y = acled_lat),
             color = 'blue', alpha = 0.5) + 
  coord_map() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Spatial Distribution of Violent Territorial Exchanges 2020-2023")

# CRSV DRC EVENTS

ggplot() +
  geom_polygon(data = drc_prov_fort, mapping = aes(x = long, y = lat, group = group),
               color = 'black', alpha = 0) +
  geom_point(data = crsv_drc, mapping = aes(x = crsv_lon, y = crsv_lat),
             color = 'green', alpha = 0.5) + 
  coord_map() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Spatial Distribution of Reported CRSV Events 2020-2023")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# GRAPHS 
#-------------------------------------------------------------------------------

# Graph 1: Histogram of Reported Perpetrators

ggplot(data = crsv_drc, aes(x = crsv_rep_perp)) + 
  geom_bar(stat = "count", fill = "black") +
  labs(title = "Reported Perpetrators of CRSV in the DRC", 
       x = "Perpetrator", y = "Count")

# Graph 2: Fitted Values for  City Model 2

# ditted values for the model
drc_cities_df$predicted2 <- predict(citymodel2)
drc_cities_df$fitted2 <- fitted(citymodel2)

# Plot fitted values 
ggplot(data = drc_cities_df) +
  geom_point(aes(x = total_gov_control, y = fitted2), color = 'red', alpha = 0.5) +
  geom_smooth(aes(x = total_gov_control, y = fitted2), method = 'lm', se = FALSE,
              color = 'red') +
  ggtitle('Government Territorial Control and CRSV Events Fitted Values') +
  xlab('Government Gains in Territory per Month') +
  ylab('Government Perpetrated CRSV Events')

# Graph 3: Time Series of Government Territorial Control & CRSV

# Convert date variable to a date format
drc_cities_df$month_date <- as.Date(drc_cities_df$month_date)

# Create a time series plot
ggplot(drc_cities_df) +
  geom_line(aes(x = month_date, y = total_gov_control), color = 'blue', alpha = 0.5) +
  geom_line(aes(x = month_date, y = total_gov_perp), color = 'red', alpha = 0.7) +
  labs(title = "Time Series Plot: Government Control & Government Perpetrated CRSV Events ",
       x = "Month/Year",
       y = "Event Count") +
  theme_bw()
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# TABLE
#-------------------------------------------------------------------------------

# Regression Table

stargazer(citymodel2, citymodel3, citymodel5, citymodel6,
      type = 'latex',
      column.labels = c('Model 1: Government Perpetration', 'Model 2: Goverment Perpetration', 
                        'Model 3: Non-State Actor Perpetration', 'Model 4: Non-State Actor Perpetration'), 
      dep.var.caption = '', 
      covariate.labels = c('Goverment Territory Control', 'Non-State Actor Territory Control',
                           'Event Fatalities'),
      title = 'Two-way Fixed Effect Models of Territorial Control and Perpetration of CRSV')
  

