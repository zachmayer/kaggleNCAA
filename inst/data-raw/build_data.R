
#Clear all existing in-RAM data
rm(list=ls(all=TRUE))
library(data.table)
library(devtools)

##########################################
# Base Kaggle Data
##########################################
seasons <- fread('inst/kaggle_data/Seasons.csv')
teams <- fread('inst/kaggle_data/Teams.csv')

regular_season_compact_results <- fread('inst/kaggle_data/RegularSeasonCompactResults.csv')
regular_season_detailed_results <- fread('inst/kaggle_data/RegularSeasonDetailedResults.csv')

tourney_compact_results <- fread('inst/kaggle_data/TourneyCompactResults.csv')
tourney_detailed_results <- fread('inst/kaggle_data/TourneyDetailedResults.csv')

tourney_seeds <- fread('inst/kaggle_data/TourneySeeds.csv')
tourney_slots <- fread('inst/kaggle_data/TourneySlots.csv')

sample_submission <- fread('inst/kaggle_data/SampleSubmission.csv')

names(seasons) <- tolower(names(seasons))
names(teams) <- tolower(names(teams))
names(regular_season_compact_results) <- tolower(names(regular_season_compact_results))
names(regular_season_detailed_results) <- tolower(names(regular_season_detailed_results))
names(tourney_compact_results) <- tolower(names(tourney_compact_results))
names(tourney_detailed_results) <- tolower(names(tourney_detailed_results))
names(tourney_seeds) <- tolower(names(tourney_seeds))
names(tourney_slots) <- tolower(names(tourney_slots))
names(sample_submission) <- tolower(names(sample_submission))

##########################################
# Extra data
##########################################

geo <- fread('inst/kaggle_data/TourneyGeog.csv')[,list(season, slot, host, lat, lng)]
geo_team <- fread('inst/kaggle_data/TeamGeog.csv')
spreads <- fread('inst/kaggle_data/covers_ncaab_data_mt.csv')

teams <- merge(teams, geo_team, by=c('team_id'), all.x=TRUE)
tourney_slots <- merge(tourney_slots, geo, by=c('season', 'slot'), all.x=TRUE)

##########################################
# Save data
##########################################

use_data(
  sample_submission,
  teams,
  tourney_compact_results,
  tourney_seeds,
  tourney_slots,
  seasons,
  tourney_detailed_results,
  regular_season_compact_results,
  regular_season_detailed_results,
  overwrite=TRUE)

##########################################
# Seed and slot print positions
##########################################
seed_print_positions <- fread('inst/kaggle_data/seed_print_positions.csv')
slot_print_positions <- fread('inst/kaggle_data/slot_print_positions.csv')
devtools::use_data(seed_print_positions, slot_print_positions, overwrite=TRUE)

##########################################
# Assign find every possible matchup and what slot it would occur in
##########################################

#Build the base data-structure linking slots together
slot_map <- reshape2::melt(tourney_slots, id.vars=c('season', 'slot'), value.name='prior_slot')
slot_map <- slot_map[,list(season, slot=slot, prior_slot)]
tourny_tree <- slot_map[,list(season, seed=prior_slot, slot1=slot, slot2=prior_slot)]

#Self join the slot map to build a tourney tree
for(i in 2:7){
  slots_merge <- data.table::copy(slot_map)
  slot1 <- paste0('slot', i)
  slot2 <- paste0('slot', i+1)
  data.table::setnames(slots_merge, c('slot', 'prior_slot'), c(slot1, slot2))
  tourny_tree <- merge(tourny_tree, slots_merge, by=c('season', slot1), all.x=TRUE, allow.cartesian=TRUE)
  data.table::set(tourny_tree, i=which(!is.na(tourny_tree[[slot2]])), j='seed', value=tourny_tree[[slot2]][!is.na(tourny_tree[[slot2]])])
}

#Order and sort the tourney tree
keys <- c('season', 'seed', 'slot1', 'slot2', 'slot3', 'slot4', 'slot5', 'slot6', 'slot7', 'slot8')
data.table::setcolorder(tourny_tree, keys)
data.table::setkeyv(tourny_tree, keys)

#Convert to possible 2-team matchups
all_slots <- tourny_tree[,list(season, slot=slot1, seed)]
all_slots <- merge(all_slots, tourney_seeds, by=c('season', 'seed'), all.x=TRUE)
all_slots <- merge(all_slots, all_slots, by=c('season', 'slot'), suffixes=c('_1', '_2'), allow.cartesian=TRUE)

#Add round
all_slots[, round := as.integer(NA)]
all_slots[grepl('R', slot), round := as.integer(substr(slot, 2, 2))]
all_slots[!grepl('R', slot), round := 0]

#Remove impossible matchups
all_slots <- all_slots[team_1 != team_2,]
all_slots[, first_matchup := min(round) == round, keyby=c('season', 'team_1', 'team_2')]
all_slots <- all_slots[first_matchup==TRUE,]
all_slots[,first_matchup := NULL]

#Cleanup final dataset
all_slots <- all_slots[team_1 != team_2,]

##########################################
# Find next slot for each slot
##########################################

#Last time we found the prior slot
#Now we're looking for the next slot
slot_tree <- reshape2::melt(tourney_slots[,list(season, slot, strongseed, weakseed)], id.vars=c('season', 'slot'))
data.table::setnames(slot_tree, c('slot', 'value'), c('next_slot', 'slot'))
slot_tree <- slot_tree[,list(season, slot, next_slot)]

#Join the forward-looking slot tree
#Note that the next slot for the championship game will be NA
all_slots <- merge(all_slots, slot_tree, by = c('season', 'slot'), all.x=TRUE)

##########################################
# ID playin games
##########################################
team_playins <- all_slots[round == 0, list(season, team_1, team_1_playedin=1L)]
opp_playins  <- all_slots[round == 0, list(season, team_2, team_2_playedin=1L)]
all_slots <- merge(all_slots, team_playins, by=c('season', 'team_1'), all.x=TRUE)
all_slots <- merge(all_slots, opp_playins, by=c('season', 'team_2'), all.x=TRUE)

all_slots[is.na(team_1_playedin), team_1_playedin := 0L]
all_slots[is.na(team_2_playedin), team_2_playedin := 0L]

##########################################
# Specify a slot ordering
##########################################
slot_order <- unique(all_slots[,list(round, slot)])[order(round, slot, decreasing=TRUE),slot]
all_slots[,slot := factor(slot, levels=slot_order)]
all_slots[,next_slot := factor(next_slot, levels=slot_order)]
all_slots[,next_slot := addNA(next_slot)]

##########################################
# Add geo
##########################################

all_slots <- merge(all_slots, geo, by=c('season', 'slot'), all.x=TRUE)

##########################################
# Save final dataset
##########################################

data.table::setkeyv(all_slots, c('season', 'team_1', 'team_2'))
devtools::use_data(all_slots, overwrite=TRUE)
