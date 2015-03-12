
#Clear all existing in-RAM data
rm(list=ls(all=TRUE))

#Load Kaggle Data
sample_submission <- data.table::fread(system.file('inst/kaggle_data/teams.csv', package = "kaggleNCAA"))
teams <- data.table::fread(system.file('inst/kaggle_data/teams.csv', package = "kaggleNCAA"))
tourney_compact_results <- data.table::fread(system.file('inst/kaggle_data/tourney_compact_results.csv', package = "kaggleNCAA"))
tourney_seeds <- data.table::fread(system.file('inst/kaggle_data/tourney_seeds.csv', package = "kaggleNCAA"))
tourney_slots <- data.table::fread(system.file('inst/kaggle_data/tourney_slots.csv', package = "kaggleNCAA"))

devtools::use_data(sample_submission, teams, tourney_compact_results, tourney_seeds, tourney_slots, overwrite=TRUE)

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
# Save final dataset
##########################################

data.table::setkeyv(all_slots, c('season', 'slot'))
devtools::use_data(all_slots, overwrite=TRUE)
