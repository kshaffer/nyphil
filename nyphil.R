library(jsonlite)
library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(tidyjson)
library(purrr)
library(lubridate)
library(broom)

nyp <- fromJSON('https://raw.githubusercontent.com/nyphilarchive/PerformanceHistory/master/Programs/json/complete.json')

work_to_data_frame <- function(work) {
  workID <- work['ID']
  composer <- work['composerName']
  title <- work['workTitle']
  movement <- work['movement']
  conductor <- work['conductorName']
  soloist <- work['soloists']
  return(c(workID = workID,
           composer = composer,
           title = title,
           movement = movement,
           conductor = conductor,
           soloist = soloist))
}

expand_works <- function(record) {
  if (is_empty(record)) {
    works_db <- as.data.frame(cbind(workID = NA, 
                                composer = NA, 
                                title = NA, 
                                movement = NA, 
                                conductor = NA, 
                                soloist = NA))
    } else {
      total <- length(record)
      works_db <- t(sapply(record[1:total], work_to_data_frame))
      colnames(works_db) <- c('workID',
                              'composer',
                              'title',
                              'movement',
                              'conductor',
                              'soloist')
    }
  return(works_db)
}

expand_program <- function(record_number) {
  record <- nyp$programs[[record_number]]
  total <- length(record)
  program <- as.data.frame(cbind(id = record$id,
                                 programID = record$programID,
                                 orchestra = record$orchestra,
                                 season = record$season,
                                 eventType = record$concerts[[1]]$eventType,
                                 location = record$concerts[[1]]$Location,
                                 venue = record$concerts[[1]]$Venue,
                                 date = record$concerts[[1]]$Date,
                                 time = record$concerts[[1]]$Time))
  works <- expand_works(record$works)
  return(cbind(program, works))
}

# this takes a LOOOOOONG time

db <- data.frame()
for (i in 1:13771) {
  db <- rbind(db, cbind(i, expand_program(i)))
}

write.csv(db, 'ny_phil_programs.csv')

tidy_nyp <- db %>%
  as_tibble() %>%
  mutate(workID = as.character(workID), 
         composer = as.character(composer), 
         title = as.character(title), 
         movement = as.character(movement), 
         conductor = as.character(conductor),
         soloist = as.character(soloist)) 

tidy_nyp %>%
  write.csv('ny_phil_programs.csv')

# most frequent composers
tidy_nyp %>%
  filter(!composer %in% c('NULL', 'Traditional,', 'Anthem,')) %>%
  filter(as.integer(substr(as.character(date),1,4)) < 1930) %>%
  count(composer, sort=TRUE) %>%
  filter(n > 200) %>%
  mutate(composer = reorder(composer, n)) %>%
  ggplot(aes(composer, n, fill = composer)) +
  geom_bar(stat = 'identity') +
  xlab('Composer') +
  ylab('Number of works performed') +
  theme(legend.position="none") +
  ggtitle('Most performed composers, NY Philharmonic, 1930-2016') +
  coord_flip()

# most frequent works
tidy_nyp %>%
  filter(!title %in% c('NULL')) %>%
  mutate(composer_work = paste(composer, '-', title)) %>%
  group_by(composer_work, programID) %>%
  summarize(times_on_program = n()) %>%
  count(composer_work, sort=TRUE) %>%
  filter(n > 220) %>%
  mutate(composer_work = reorder(composer_work, n)) %>%
  ggplot(aes(composer_work, n, fill = composer_work)) +
  geom_bar(stat = 'identity') +
  xlab('Composer and work') +
  ylab('Number of times performed') +
  theme(legend.position="none") +
  coord_flip()

# most frequent DSCH works
tidy_nyp %>%
  filter(!title %in% c('NULL')) %>%
  filter(composer == 'Shostakovich,  Dmitri') %>%
  mutate(composer_work = paste(composer, '-', title)) %>%
  count(composer_work, sort=TRUE) %>%
  filter(n > 20) %>%
  mutate(composer_work = reorder(composer_work, n)) %>%
  ggplot(aes(composer_work, n)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab('Number of times performed') +
  coord_flip()

# meistersinger - complete or overture?
meister <- tidy_nyp %>%
  filter(title == 'MEISTERSINGER, DIE, WWV 96') %>%
  select(programID)

tidy_nyp %>%
  filter(programID %in% meister$programID) %>%
  group_by(programID) %>%
  summarize(count_pieces = n()) %>%
  filter(count_pieces <= 2)

# changes over time
comp_counts <- tidy_nyp %>%
  filter(!composer %in% c('NULL', 'Traditional,', 'Anthem,')) %>%
  mutate(year = as.integer(substr(as.character(date),1,4))) %>%
  group_by(year) %>%
  mutate(year_total = n()) %>%
  group_by(composer, year) %>%
  mutate(comp_total_by_year = n()) %>%
  ungroup() %>%
  group_by(composer, year, comp_total_by_year, year_total) %>%
  summarize() %>%
  mutate(share = comp_total_by_year/year_total) %>%
  group_by(year) %>%
  mutate(average_share = mean(share)) 

# count composers per year
comp_by_years <- comp_counts %>%
  group_by(year) %>%
  summarize(comp_per_year = n())

comp_by_years %>%
  ggplot(aes(year, comp_per_year)) +
  geom_line() +
  xlab('Year') +
  ylab('Composers appearing on a program')

comp_counts %>%
  filter(composer %in% c('Beethoven,  Ludwig  van',
                         'Tchaikovsky,  Pyotr  Ilyich',
                         'Wagner,  Richard',
                         'Mozart,  Wolfgang  Amadeus')) %>%
  ggplot() +
  geom_line(aes(year, share, color = composer), alpha = 0.8, size = 0.7) +
  labs(x = 'Year',
       y = 'Percentage of works in repertoire',
       title = 'Composers\' changing share of NY Philharmonic repertoire over time')

# top nationality
comp_counts %>%
  filter(composer %in% c('Berlioz,  Hector',
                         'Tchaikovsky,  Pyotr  Ilyich',
                         'Wagner,  Richard',
                         'Dvorak,  Antonín',
                         'Gershwin,  George')) %>%
  ggplot(aes(year, share, color = composer)) +
  geom_line(alpha = 0.8, size = 0.7) +
  labs(x = 'Year',
       y = 'Percentage of works in repertoire',
       title = 'Composers\' changing share of NY Philharmonic repertoire over time')

# some composers
comp_counts %>%
  filter(composer %in% c('Berlioz,  Hector',
                         'Ravel,  Maurice',
                         'Wagner,  Richard',
                         'Debussy,  Claude',
                         'Mahler,  Gustav')) %>%
  ggplot(aes(year, share, color = composer)) +
  geom_line(alpha = 0.8, size = 0.7) +
  labs(x = 'Year',
       y = 'Percentage of works in repertoire',
       title = 'Composers\' changing share of NY Philharmonic repertoire over time')

# most frequent composers - normalized
comp_share %>%
  filter(composer %in% c('Beethoven,  Ludwig  van',
                         'Tchaikovsky,  Pyotr  Ilyich',
                         'Wagner,  Richard',
                         'Mozart,  Wolfgang  Amadeus')) %>%
  ggplot() +
  geom_line(aes(year, share_norm, color = composer), alpha = 0.8, size = 0.7) +
  labs(x = 'Year',
       y = 'Normalized share of works in repertoire (share / average share)',
       title = 'Changing repertoire share of the top four composers in the\nNY Philharmonic performance history')


