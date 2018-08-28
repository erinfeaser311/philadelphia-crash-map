library(tidyverse)

philly <- read_rds("data/philly-crash-data.rds")


# NSWE: 40.1, 39.8, -75.3, -75
  #Makes the big square of phildelphia (latitude)

## https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles 

##The approximate conversions are:
  
##  Latitude: 1 deg = 110.574 km
  

lat_width = (40.1 - 39.8) / 33.36
long_width = (-75 + 75.3) / 25.63

philly$latitude_kmsquare = cut(philly$latitude, 
                               seq(from = 39.8, 
                                   to = 40.12, ## have to include higher interval 
                                   by = lat_width), include.lowest = TRUE, right = FALSE)

philly$longitude_kmsquare = cut(philly$longitude, 
                                seq(from = -75.3, 
                                    to = -74.98, ## have to include higher interval
                                    by = long_width), include.lowest = TRUE, right = FALSE)


philly$hr_block = cut(as.numeric(philly$hour_of_day),
                      seq(from = 0,
                          to = 24,
                          by = 3), include.lowest = TRUE, right = FALSE)
                  

crash_grouping <- philly %>% 
  filter(!is.na(philly$hr_block)) %>% 
  group_by(crash_month, day_of_week, hr_block, 
           latitude_kmsquare, longitude_kmsquare) %>% 
  summarize(total_unit_count = sum(total_unit_count), 
            injury_count = sum(injury_count), 
            fatality_count = sum(fatality_count),
            accident_count = n())

## visualize crash_grouping

#ggplot(crash_grouping, aes(fatality_count)) + geom_histogram()



#ggplot(filter(crash_grouping, fatality_count > 1), aes(fatality_count)) + geom_histogram()
## split

crash_grouping2 <- crash_grouping %>% 
  separate(latitude_kmsquare, c("lame1", "lat1", "lat2", "lame2"), sep = "[\\[,\\)]") %>% 
  separate(longitude_kmsquare, c("lame3", "long1", "long2", "lame4"), sep = "[\\[,\\)]") %>% 
  select(-starts_with("lame")) %>% 
  mutate(risk_score = (2*total_unit_count) + (3*injury_count) + (4*fatality_count))

crash_grouping2$risk_level <- cut(crash_grouping2$risk_score, breaks=c(0, 10, 50, Inf), 
                                  right=FALSE, labels=c('Low Risk', 'Medium Risk', 'High Risk'))

crash_grouping2$lat1 <- as.numeric(crash_grouping2$lat1)
crash_grouping2$lat2 <- as.numeric(crash_grouping2$lat2)
crash_grouping2$long1 <- as.numeric(crash_grouping2$long1)
crash_grouping2$long2 <- as.numeric(crash_grouping2$long2)

crash_grouping2$crash_month <- as.integer(crash_grouping2$crash_month)
philly$crash_month <- as.integer(philly$crash_month)

dtf_color <- tibble(
  risk_level = c('Low Risk', 'Medium Risk', 'High Risk'),
  color = c("green", "yellow", "red")
)

crash_grouping2 <- crash_grouping2 %>% 
  left_join(dtf_color, by = "risk_level") %>% 
  mutate(layerId = paste0(lat1, ",", lat2, ",", long1, ",", long2)) %>% 
  ungroup() %>% 
  mutate(crash_month = as.integer(crash_month))

philly <-
  philly %>% 
  mutate(fatality = ifelse(fatality_count >0, "Yes", "No"))

write_rds(crash_grouping2, "shiny/app-data/crash_grouping2.rds")
write_rds(philly, "shiny/app-data/philly.rds")




## use a loop and if then else to assign color to risk_color?

## PHASE ONE 
  # 1. Figure out 1 km by 1 km squares
    #Need to know the range of latitude and longitude (increments)
     #Can use cut function 
     #seq(long, by______)
     #Collapse down and look at number of crashes within that square - high risk? low risk?
       #Collapse down by day of the week and time of day
       #Logical split of high, medium, and low risk
  # 2. Squares by month, day of week, and 3 hour chunks 
  # 3. Create a variable that would label it high risk, medium risk, and low risk 
    ## columns - risk, risk_color




## PHASE TWO
  # 4. Create interactive map (leaflet)
  # 5. Add the sqaures layers to the map 

## PHASE THREE
  # 6. Click on square to get more information 
  # 7. Adding specific crash points to each square

#Determining risk: using injuries, deaths, and possibly vehicle count 
  #Maybe using fct_lump on variables to split them up into 3 groups; have to do it after we 
  #group by other variables (month, day of week, etc.)

