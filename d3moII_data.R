setwd('//media/beemyfriend/UUI/R_Projects')
packages <- c('dplyr', 'stringr', 'htmltab', 'tidyr', 'rvest')
sapply(packages, library, character.only = T)

main_url <- 'http://www.nws.noaa.gov/hic/flood_stats/Fatalities/'
main_append <- '.htm'

#manually extracted first two years from site's html
info_location <- c('us_flood_data/2010_flood_fatalities.html', 
                   'us_flood_data/2011_flood_fatalities.html', 
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

death_info_list <- list(death_info_10, death_info_11, death_info_12, death_info_13, death_info_14)
names(death_info_list[[4]])[9] <- 'Circumstance'
sapply(death_info_list, names)

important_columns <- c('Date', 'State', 'City', 'County', 'Age', 'Sex', 'Vehicle_Related', 'Circumstance')
sapply(important_columns, function(x){
  sapply(death_info_list, function(y){
    x %in% names(y)
  })
})

names(death_info_list[[1]])[8] <- 'Vehicle_Related'
names(death_info_list[[2]])[8] <- 'Vehicle_Related'

sapply(death_info_list, function(x){
  str_detect(names(x), 'Vehicle|Activity')
})
trial %>% str()

sapply(death_info_list, function(x){
  if(T %in% str_detect(names(x), 'Vehicle')){
    column_name <- x %>% names() %>% .[str_detect(x %>% names(), 'Vehicle')]
    x[column_name] %>% head
  } else if (T %in% str_detect(names(x), 'Activity')){
    column_name <- x %>% names() %>% .[str_detect(x %>% names(), 'Activity')]
    x[column_name] %>% unique
  }
})

vehicle_column <- 'Vehicle_Related'

death_info_list[[3]] <- death_info_list[[3]] %>%
  mutate(Vehicle_Related = Circumstance %>% str_detect(regex('drive|car|vehicle', ignore_case = T)))

death_info_list[[4]] <- death_info_list[[4]] %>%
  mutate(Vehicle_Related = Activity %>% str_detect(regex('Driving|Horseback|ATV')))

death_info_list[[5]] <- death_info_list[[5]] %>%
  mutate(Vehicle_Related = Activity %>% str_detect(regex('Driving|Horseback|ATV')))

death_info_list %>% sapply(function(x){
  x$Date %>% head
})

for(i in 1:5){
  death_info_list[[i]]$Year = (2009 + i)
}

every_flood_death <- data.frame()
for(i in seq_along(death_info_list)){
  temp <- death_info_list[[i]] %>%
    select(Year, Date, State, City, County, Age, Sex, Vehicle_Related, Circumstance)
  every_flood_death <- rbind(every_flood_death, temp)
}

every_flood_death <- every_flood_death %>% 
  filter(Date != '') %>%
  mutate(Vehicle_Related = str_detect(Vehicle_Related, regex('TRUE|Yes')))

readr::write_tsv(every_flood_death, 'every_flood_death_20102014.tsv')


fhtml %>%
  str_extract('<tbody>')

html %>%
  html_node('frame') %>%
  html_node('#document')

trial %>% str
driver_deaths <- trial %>%
  filter(Activity == 'Driving')

driver_deaths_info <- driver_deaths[[death_info[index]]] %>%
  str_replace_all('\\s\\s+', ' ')

driver_deaths %>%
  str

