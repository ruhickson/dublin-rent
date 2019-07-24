library(dplyr)
library(ggplot2)
library(tweenr)
library(gganimate)
library(magick)
library(transformr)

## Specify the area of Dublin you want to analyze
area = 'Dublin 4'

## load and format rental file
## original data available from
## https://www.cso.ie/px/pxeirestat/statire/SelectVarVal/Define.asp?MainTable=RIA02&PLanguage=0&PXSId=0
rent<-read.csv("dublin_rent_rates.csv", sep=";")
rent <- rent %>%
  filter(location %in% c(paste0(area)))
rent <- rent %>%
  filter(property_type %in% c('One bed','Two bed', 'Three bed', 'Four plus bed'))
rent_years <- rent %>%
  select(year) %>%
  distinct()
rent_years <- as.vector(rent_years$year)
rent$property_type<-factor(rent$property_type, levels = c('One bed','Two bed', 'Three bed', 'Four plus bed'))


## load and format income file
## original data available from
## https://www.cso.ie/px/pxeirestat/statire/SelectVarVal/Define.asp?MainTable=EHA05&PLanguage=0&PXSId=0
income<-read.csv("avg_earnings.csv", sep=";")
income$average_monthly_earnings <- round(income$average_annual_earnings/12,0)

## combine both datasets
rent <- inner_join(rent,income,by="year")

## plot for Dublin 4-specific rental price changes
## if you change the dplyr logic when reading in the rental data,
## remember to change the title property accordingly
ggplot(rent, aes(x=as.factor(year),y=price, group = property_type, color= property_type)) +
  geom_point(size=2) +
  labs(title=paste0("Average ",area," Rent/Month by Property Type - 2008-2018"),
       y="Price (€)",
       x="Year",
       caption="data from CSO StatBank") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, size=12),
        axis.text.y = element_text(angle=0, size=12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=18)) +
  geom_line(aes(colour = property_type)) +
  scale_x_discrete(breaks=rent_years,
                   labels=rent_years) +
  facet_grid(location ~ property_type) +
  guides(color=guide_legend(title="Property Type"))

## same plot as above, but animated and includes
## 33% of average Dublin tech sector worker data salary
rent_plot <- ggplot(rent, aes(x=as.factor(year),y=price, group = property_type, color= property_type)) +
  geom_line(aes(x=as.factor(year),y=average_monthly_earnings*.33, color="1/3 average monthly wage\nin Dublin tech sector")) +
  geom_point(size=2,aes(x=as.factor(year),y=average_monthly_earnings*.33, color="1/3 average monthly wage\nin Dublin tech sector")) +
  geom_point(size=2) +
  labs(title=paste0("Average ",area," Rent/Month by Property Type - 2008-2018"),
       y="Price (€)",
       x="Year",
       caption="data from CSO StatBank") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, size=12),
        axis.text.y = element_text(angle=0, size=12),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=18)) +
  geom_line(aes(colour = property_type)) +
  scale_x_discrete(breaks=rent_years,
                   labels=rent_years) +
  facet_grid(location ~ property_type) +
  guides(color=guide_legend(title="")) +
  transition_states(year, wrap=F) + 
  transition_reveal(year) +
  shadow_wake(wake_length = 0.1, alpha = FALSE)


## animate plot
animate(rent_plot, height = 600, width =900)

## save plot locally
anim_save("rent_plot.gif")
