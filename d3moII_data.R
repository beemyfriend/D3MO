setwd('//media/beemyfriend/UUI/R_Projects/D3MO/')
packages <- c('dplyr', 'stringr', 'htmltab', 'tidyr', 'rvest', 'readr', 'tidytext', 'ggplot2', 'ggmap')
sapply(packages, library, character.only = T)

main_url <- 'http://www.nws.noaa.gov/hic/flood_stats/Fatalities/'
main_append <- '.htm'

#manually extracted first two years from site's html
info_location <- c('data/us_flood_data/2010_flood_fatalities.html', 
                   'data/us_flood_data/2011_flood_fatalities.html', 
                   str_c(main_url, 2012, main_append),
                   str_c(main_url, 2013, main_append),
                   str_c(main_url, 2014, main_append))


death_info_10 <- read_html(info_location[1]) %>% html_nodes('table') %>% html_table(header = T) %>% .[[1]]
death_info_10 <- death_info_10[death_info_10 %>% names() %>% sapply(function(x){x != ''}) ]

death_info_11 <- read_html(info_location[2]) %>% html_nodes('table') %>% html_table(header = T) %>% .[[1]]
death_info_11 <- death_info_11[death_info_11 %>% names() %>% sapply(function(x){x != ''})]

death_info_12 <- htmltab(info_location[3])
death_info_13 <- htmltab(info_location[4])
death_info_14 <- htmltab(info_location[5])

death_info_list <- list(death_info_10, 
                        death_info_11, 
                        death_info_12, 
                        death_info_13, 
                        death_info_14)

#Check for differences in years
sapply(death_info_list, names)

#change 'Comments' variable to 'Circumstance' variable in death_info_13. To stay consistent with other data.frame variables
names(death_info_list[[4]])[9] <- 'Circumstance'

#Determine what should be kept in final dataset
important_columns <- c('Date', 'State', 'City', 'County', 'Age', 'Sex', 'Vehicle_Related', 'Circumstance')
sapply(important_columns, function(x){
  sapply(death_info_list, function(y){
    x %in% names(y)
  })
})

#the first two data frames simply new named variables
names(death_info_list[[1]])[8] <- 'Vehicle_Related'
names(death_info_list[[2]])[8] <- 'Vehicle_Related'

#There is only one data frame that doesn't explicitly explain what the victim was doing.
sapply(death_info_list, function(x){
  T %in% str_detect(names(x), 'Vehicle|Activity')
})

#The first two data frames provide a binary for whether the victim was in a vehicle or not
#the last two data frames provide the activity the the victim was doing before death
sapply(death_info_list, function(x){
  if(T %in% str_detect(names(x), 'Vehicle')){
    column_name <- x %>% names() %>% .[str_detect(x %>% names(), 'Vehicle')]
    x[column_name] %>% head
  } else if (T %in% str_detect(names(x), 'Activity')){
    column_name <- x %>% names() %>% .[str_detect(x %>% names(), 'Activity')]
    x[column_name] %>% unique
  }
})



#Let's create the 'Vehicle_Related' variable for the middle data frame
#we will do this by searching for terms associated with vehicle.
death_info_list[[3]] <- death_info_list[[3]] %>%
  mutate(Vehicle_Related = Circumstance %>% 
           str_detect(regex('drive|car|vehicle', ignore_case = T)))

#We'll also create a 'Vehicle_Related' variable for the final two.
death_info_list[[4]] <- death_info_list[[4]] %>%
  mutate(Vehicle_Related = Activity %>% str_detect(regex('Driving|Horseback|ATV')))

death_info_list[[5]] <- death_info_list[[5]] %>%
  mutate(Vehicle_Related = Activity %>% str_detect(regex('Driving|Horseback|ATV')))

#And let's make the 'Vehicle_Related' column in the first two data frames consistent with the rest
#and provide a true/flase binary
death_info_list[[1]]$Vehicle_Related <- death_info_list[[1]]$Vehicle_Related %>%
  str_detect('Yes')

death_info_list[[2]]$Vehicle_Related <- death_info_list[[2]]$Vehicle_Related %>%
  str_detect('Yes')

#Let's also create an explicit Year variable, just in case
death_info_list %>% sapply(function(x){
  x$Date %>% head
})

for(i in 1:5){
  death_info_list[[i]]$Year = (2009 + i)
}

#Now, let's put them together
every_flood_death <- data.frame()
for(i in seq_along(death_info_list)){
  temp <- death_info_list[[i]] %>%
    select(Year, Date, State, City, County, Age, Sex, Vehicle_Related, Circumstance)
  every_flood_death <- rbind(every_flood_death, temp)
}

#let's get rid of missing values. It seems like Date == '' is a good indicator of missing values
every_flood_death <- every_flood_death %>% 
  filter(Date != '') 

readr::write_tsv(every_flood_death, 'data/us_flood_data/every_flood_death_20102014.tsv')


#let's analyze this data
#starting with text analysis

every_flood_death <- readr::read_tsv('data/us_flood_data/every_flood_death_20102014.tsv')

circumstance <- every_flood_death$Circumstance %>%
  str_replace_all('\\s\\s+', ' ')

find_circ <- function(vect, i){ifelse(vect[i] == '"', find_circ(vect, i-1), vect[i])}
circumstance <- sapply(seq_along(circumstance), function(i){find_circ(circumstance, i)})
every_flood_death$Circumstance <- circumstance

write_tsv <- write_tsv(every_flood_death, 'data/us_flood_data/every_flood_death_20102014.tsv')

unique_circumstance <- circumstance %>% unique %>% .[!is.na(.)]
circumstance_df <- data.frame(id = seq_along(unique_circumstance), circumstance = unique_circumstance, stringsAsFactors = F)

circumstance_counts <- circumstance_df %>% 
  unnest_tokens(word, circumstance) %>%
  anti_join(stop_words) %>%
  count(word, sort = T)

circumstance_bigrams <- circumstance_df %>%
  unnest_tokens(bigram, circumstance, token = 'ngrams', n = 2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = T) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  mutate(bigram = bigram %>% factor(levels = rev(bigram)))
  
circumstance_trigrams <- circumstance_df %>%
  unnest_tokens(trigram, circumstance, token = 'ngrams', n = 3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram, sort = T) %>%
  mutate(trigram = trigram %>% factor(levels = rev(trigram)))

circumstance_bigrams %>% filter(n > 2) %>% .$bigram 
circumstance_trigrams %>% filter(n > 1) %>% .$trigram

png(filename = 'images/us_flood_images/circumstance_bigrams.png', width = 1000, height = 600, res = 100)
ggplot(circumstance_bigrams %>% filter(n > 2), aes(bigram, n)) +
  geom_col() +
  coord_flip() +
  ggtitle('National Weather Services: Top Bigrams for Flood Death Circumstances')
dev.off()

png(filename = 'images/us_flood_images/circumstance_trigrams.png', width = 1000, height = 600, res = 100)
ggplot(circumstance_trigrams %>% filter(n > 1), aes(trigram, n)) +
  geom_col() +
  coord_flip() +
  ggtitle('National Weather Services: Top Trigrams for Flood Death Circumstances')
dev.off()

#now let's analyze the locations

county <- every_flood_death$County %>% str_replace_all('\\s+', ' ')
every_flood_death$County <- county
every_flood_death %>%
  count(County, sort = T)

city <- every_flood_death$City %>% str_replace_all('\\s+', ' ')
every_flood_death$City <- city
every_flood_death %>%
  count(City, sort = T)

every_flood_death %>%
  count(State, City, County, sort = T)

every_flood_death %>%
  count(State, City, sort = T)

every_flood_death <- every_flood_death %>%
  mutate(location_for_google = str_c(City, ', ', State))

location_for_google <- every_flood_death %>% 
  filter(!is.na(location_for_google)) %>% .$location_for_google %>% unique

location_coordinates <- lapply(location_for_google, geocode)

geolocation_information <- data.frame(
  location_for_google = location_for_google,
  lon = sapply(seq_along(location_coordinates), function(i){location_coordinates[[i]]$lon}),
  lat = sapply(seq_along(location_coordinates), function(i){location_coordinates[[i]]$lat})
)

write_tsv(geolocation_information, 'data/us_flood_data/geolocation_information.tsv')

every_flood_death <- every_flood_death %>%
  left_join(geolocation_information)

deaths_by_location_total <- every_flood_death %>%
  filter(!is.na(location_for_google)) %>%
  count(location_for_google, lon, lat, sort = T)

deaths_by_location_year <- every_flood_death %>%
  filter(!is.na(location_for_google)) %>%
  count(location_for_google, lon, lat, Year, sort = T)

usmap <- get_map(location = 'united states', 
                 maptype  = 'watercolor', 
                 zoom = 4)

png(filename = 'images/us_flood_images/geolocated_deaths_total.png', width = 1000, height = 600, res = 100)
ggmap(usmap) +
  geom_point(
    data = deaths_by_location_total, 
    aes(lon, lat, size = n),
    alpha = .5
  ) +
  ggtitle('National Weather Services: Map of Flood Deaths in Mainland USA') +
  labs(size = 'Total Number of Deaths') 
dev.off()

png(filename = 'images/us_flood_images/geolocated_deaths_year.png', width = 1000, height = 600, res = 100)
ggmap(usmap) +
  geom_point(
    data = deaths_by_location_year, 
    aes(lon, lat, size = n),
    alpha = .5
  ) +
  ggtitle('National Weather Services: Map of Flood Deaths in Mainland USA by Year') +
  labs(size = 'Total Number of Deaths') +
  facet_wrap(~Year)
dev.off()

write_tsv(every_flood_death, 'data/us_flood_data/every_flood_death_20102014.tsv')

#analyze the days
deadly_days <- every_flood_death %>%
  count(Date, sort = T) %>%
  filter(n > 10)

deadly_days <- lapply(deadly_days$Date, function(x){
  every_flood_death %>% filter(Date == x) %>% .$Circumstance %>% unique}
  )

sapply(deadly_days, length)

deadliest_day <- every_flood_death %>%
  filter(Date == '8/28/2011')