
#-------------------------------------------------------------------------------
# VISUALIZATIONS
#-------------------------------------------------------------------------------
drc_prov_fort <- fortify(drc_prov)

ggplot() +
  geom_polygon(data = drc_prov_fort, mapping = aes(x = long, y = lat, group = group), color = 'white') +
  geom_point(data = acled_drc, mapping = aes(x = acled_lon, y = acled_lat),
             color = 'red') +
  theme_minimal()
#-------------------------------------------------------------------------------



plot(drc_prov)
plot(x = acled_drc$acled_lon, y = acled_drc$acled_lat, color = 'red', add = TRUE)
