
#Clear all existing in-RAM data
rm(list=ls(all=TRUE))
library(data.table)
library(devtools)
library(geosphere)

THIS_YEAR <- 2017

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

#sample_submission <- fread('inst/kaggle_data/SeedBenchmark.csv') # Not out yet
sample_submission <- fread('inst/kaggle_data/SampleSubmission.csv')
setnames(sample_submission, tolower(names(sample_submission)))

names(seasons) <- tolower(names(seasons))
names(teams) <- tolower(names(teams))
names(regular_season_compact_results) <- tolower(names(regular_season_compact_results))
names(regular_season_detailed_results) <- tolower(names(regular_season_detailed_results))
names(tourney_compact_results) <- tolower(names(tourney_compact_results))
names(tourney_detailed_results) <- tolower(names(tourney_detailed_results))
names(tourney_seeds) <- tolower(names(tourney_seeds))
names(tourney_slots) <- tolower(names(tourney_slots))

##########################################
# Extra data
##########################################

#Team geos
geo_team <- fread('inst/kaggle_data/TeamGeog.csv')
teams <- merge(teams, geo_team, by=c('team_id'), all.x=TRUE)

#Tourney geo
geo_tourney <- fread('inst/kaggle_data/TourneyGeog.csv')[,list(season, slot, host, lat, lng)]
setnames(geo_tourney, c('lat', 'lng'), c('host_lat', 'host_lng'))
tourney_slots <- merge(tourney_slots, geo_tourney, by=c('season', 'slot'), all.x=TRUE)

#Spreads (MISSING 2016 and 2017!)
spreads <- fread('inst/kaggle_data/covers_ncaab_data_mt.csv')

#ADD: Massey Ordinals

##########################################
# Calculate distance for regular season games
##########################################

wdist = teams[,list(wteam=team_id, wlat=lat, wlng=lng)]
ldist = teams[,list(lteam=team_id, llat=lat, llng=lng)]

regular_season_compact_results = merge(
  regular_season_compact_results,
  wdist, by='wteam', all.x=TRUE
)

regular_season_compact_results = merge(
  regular_season_compact_results,
  ldist, by='lteam', all.x=TRUE
)

regular_season_compact_results[, dist := distCosine(cbind(wlng, wlat), cbind(llng, llat)) / 1609.34]
regular_season_compact_results[wloc == 'N', dist := NA]
regular_season_compact_results[wloc == 'A', wdist := dist]
regular_season_compact_results[wloc == 'H', wdist := 0]
regular_season_compact_results[wloc == 'A', ldist := 0]
regular_season_compact_results[wloc == 'H', ldist := dist]

#Use median distance for neutral game
med_dist <- regular_season_compact_results[,median(dist, na.rm=TRUE)]
regular_season_compact_results[is.na(wdist), wdist := med_dist]
regular_season_compact_results[is.na(ldist), ldist := med_dist]
regular_season_compact_results[, dist := NULL]

##########################################
# Seed and slot print positions
##########################################
seed_print_positions <- fread('inst/kaggle_data/seed_print_positions.csv')
slot_print_positions <- fread('inst/kaggle_data/slot_print_positions.csv')

##########################################
# Assign find every possible matchup and what slot it would occur in
##########################################

#Build the base data-structure linking slots together
slot_map <- reshape2::melt(tourney_slots, id.vars=c('season', 'slot', 'host', 'host_lat', 'host_lng'), value.name='prior_slot')
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
# Add geo to all slots
##########################################

dist_1 = teams[,list(team_1=team_id, lat_1=lat, lng_1=lng)]
dist_2 = teams[,list(team_2=team_id, lat_2=lat, lng_2=lng)]
all_slots <- merge(all_slots, geo_tourney, by=c('season', 'slot'), all.x=TRUE)
all_slots = merge(all_slots, dist_2, by='team_2')
all_slots = merge(all_slots, dist_1, by='team_1')
setkeyv(all_slots, c('season', 'team_1', 'team_2'))
rm(dist_1, dist_2)

all_slots[, dist_1 := distCosine(cbind(lng_1, lat_1), cbind(host_lng, host_lat)) / 1609.34]
all_slots[, dist_2 := distCosine(cbind(lng_2, lat_2), cbind(host_lng, host_lat)) / 1609.34]

##########################################
# Add all-slots geo to tourney compact
##########################################

tourney_geo = all_slots[,list(
  season,
  wteam = team_1,
  lteam = team_2,
  wlat = lat_1,
  wlng = lng_1,
  llat = lat_2,
  llng = lng_2,
  wdist = dist_1,
  ldist = dist_2)]

tourney_compact_results <- merge(
  tourney_compact_results,
  tourney_geo,
  by=c('season', 'wteam', 'lteam'),
  all.x=TRUE
)

setcolorder(tourney_compact_results, names(regular_season_compact_results))

##########################################
# Replace sample sub with a simple seed diff benchmark
##########################################

#Wrangle the data, ugh
tmp_dat <- tourney_compact_results[,list(season, wteam, lteam, wscore, lscore)]
tmp_dat[,id1 := stri_paste(season, wteam, lteam, sep='_')]
tmp_dat[,id2 := stri_paste(season, lteam, wteam, sep='_')]
tmp_dat[wteam < lteam, id := id1]
tmp_dat[wteam > lteam, id := id2]
tmp_dat[,c('id1', 'id2') := NULL]
rename = c('team', 'score')
oldnames <- c(paste0('w', rename), paste0('l', rename))
newnames <- c(paste0(rename, '_1'), paste0(rename, '_2'))
setnames(tmp_dat, oldnames, newnames)

ids <- stri_split_fixed(sample_submission[['id']], '_')
sample_submission[,pred := NULL]
sample_submission[, season := as.integer(sapply(ids, '[[', 1))]
sample_submission[, team_1 := as.integer(sapply(ids, '[[', 2))]
sample_submission[, team_2 := as.integer(sapply(ids, '[[', 3))]

tmp_dat <- rbind(tmp_dat, sample_submission, fill=T)
sample_submission[,c('season', 'team_1', 'team_2') := NULL]

team1 <- tmp_dat[,list(
  id, season,
  team_1, team_2,
  score_1, score_2)]

team2 <- tmp_dat[,list(
  id, season,
  team_1=team_2, team_2=team_1,
  score_1=score_2, score_2=score_1)]

tmp_dat <- rbindlist(list(team1, team2))
rm(team1, team2)
gc(reset=TRUE)

#Add seeds
tmp_dat_2 <- all_slots[,list(season, team_1, team_2, seed_1, seed_2)]
replace <- '[W|X|Y|Z|a|b]'
tmp_dat_2[,seed_1 := as.integer(gsub(replace, '', seed_1))]
tmp_dat_2[,seed_2 := as.integer(gsub(replace, '', seed_2))]
tmp_dat_2[,seed_diff := seed_1 - seed_2]
tmp_dat_2[,c('seed_1', 'seed_2') := NULL]
tmp_dat <- merge(tmp_dat, tmp_dat_2, by=c('season', 'team_1', 'team_2'))

#Model and Predict
tmp_dat[,won := as.integer(score_1 > score_2)]
model <- tmp_dat[!is.na(won), glm(won ~ 0 + seed_diff, family='binomial')]
tmp_dat <- tmp_dat[,pred := predict(model, tmp_dat, type='response')]
setkeyv(tmp_dat, 'id')
# summary(tmp_dat[,sum(pred),by='id'])
# summary(tmp_dat[,sum(won),by='id'])
# summary(tmp_dat[,sum(seed_diff),by='id'])
tmp_dat

#Add to sample sub
tmp_dat <- tmp_dat[,id := paste(season, team_1, team_2, sep='_')]
tmp_dat <- tmp_dat[,list(id, pred)]
sample_submission <- merge(sample_submission, tmp_dat, by='id', all.x=T)

##########################################
# Save data
##########################################

#set slot order for all slot objects
slot_print_positions[,slot := factor(slot, levels=slot_order)]
tourney_slots[,slot := factor(slot, levels=slot_order)]
slot_print_positions[,slot := factor(slot, levels=slot_order)]
all_slots[,slot := factor(slot, levels=slot_order)]

use_data(
  seed_print_positions,
  slot_print_positions,
  sample_submission,
  teams,
  tourney_compact_results,
  tourney_seeds,
  tourney_slots,
  seasons,
  tourney_detailed_results,
  regular_season_compact_results,
  regular_season_detailed_results,
  all_slots,
  overwrite=TRUE)
