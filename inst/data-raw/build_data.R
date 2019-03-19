
#Clear all existing in-RAM data
rm(list=ls(all=TRUE))
gc(reset=T)
library(data.table)
library(devtools)
library(geosphere)
library(stringi)
library(splines)
library(pbapply)
library(lawstat)

##########################################
# Quick check on Women's data
##########################################
if(FALSE){
  library(data.table)
  compact <- fread('inst/kaggle_data/WRegularSeasonCompactResults.csv')
  detail <- fread('inst/kaggle_data/WRegularSeasonDetailedResults.csv')
  both <- merge(compact, detail, by=intersect(names(compact), names(detail)), all=T)
  both[,list(
    pct_missing=sum(is.na(WFGA))/.N
  ), by='Season']

  compact <- fread('inst/kaggle_data/WNCAATourneyCompactResults.csv')
  detail <- fread('inst/kaggle_data/WNCAATourneyDetailedResults.csv')
  both <- merge(compact, detail, by=intersect(names(compact), names(detail)), all=T)
  both[,list(
    pct_missing=sum(is.na(WFGA))/.N
  ), by='Season']
}

##########################################
# Base Kaggle Data
##########################################
THIS_YEAR <- 2019
load_both <- function(filename, base_dir='inst/kaggle_data/', check_year=THIS_YEAR){
  W <- fread(paste0(base_dir, 'W', filename))
  M <- fread(paste0(base_dir,  '', filename))
  W[,women := 1]
  M[,women := 0]
  if(length(check_year) > 0){
    if('season' %in% names(M)){
      stopifnot(all(check_year %in% M[['season']]))
    }
    if('season' %in% names(W)){
      stopifnot(all(check_year %in% W[['season']]))
    }
  }
  out <- rbind(W, M, fill=T)
  setnames(out, tolower(names(out)))
  return(out)
}

# Load Sample submission first
sample_submission <- load_both('SampleSubmission.csv', check_year=THIS_YEAR)
CHK <- sample_submission[,as.integer(sort(unique(sapply(strsplit(id, '_'), '[[', 1))))]
stopifnot(CHK == THIS_YEAR)
sample_submission_W <- sample_submission[women == 1,]
sample_submission_M <- sample_submission[women == 0,]

# Load the rest of the data
seasons <- load_both('Seasons.csv')
teams <- load_both('Teams.csv')
regular_season_compact_results <- load_both('RegularSeasonCompactResults.csv')
tourney_compact_results <- load_both('NCAATourneyCompactResults.csv')
tourney_seeds <- load_both('NCAATourneySeeds.csv')
tourney_seeds[season==THIS_YEAR & teamid %in% c(3107, 3124),]

tourney_slots <- load_both('NCAATourneySlots.csv')
cities <- load_both('Cities.csv')
game_cities <- load_both('GameCities.csv')
regular_season_detailed_results <- load_both('RegularSeasonDetailedResults.csv')
tourney_detailed_results <- load_both('NCAATourneyDetailedResults.csv')

# All slots for all years for women
all_w_seasons = data.table(
  women=1,
  season_new=tourney_slots[,sort(unique(season))]
)
stopifnot(THIS_YEAR %in% all_w_seasons$season_new)
tourney_slots = merge(tourney_slots, all_w_seasons, by='women', all=T, allow.cartesian=TRUE)
tourney_slots[is.na(season) & women == 1,season := season_new]
tourney_slots[,season_new := NULL]
stopifnot(tourney_slots[season==THIS_YEAR,any(women==1)])

##########################################
# Extra data
##########################################

# Men's secondary tourney data - CONSIDER ADDING NEXT YEAR
# tourney_compact_results2 <- fread('inst/kaggle_data/SecondaryTourneyCompactResults.csv')
# tourney_compact_results[,tourney := 'madness']
# tourney_compact_results2[,women := 0]
# setnames(tourney_compact_results2, tolower(names(tourney_compact_results2)))
# setnames(tourney_compact_results2, 'secondarytourney', 'tourney')
# tourney_compact_results <- rbind(tourney_compact_results, tourney_compact_results2, fill=T)

#ADD: Vegas Spreads (need a new source!)
#ADD: Massey Ordinals (lots of 'em— median?)

##########################################
# Map games to location
##########################################

##########################################
# Map teams to location
##########################################

##########################################
# Calculate distance for regular season games
##########################################

# wdist = teams[,list(wteamid=teamid, wlat=lat, wlng=lng)]
# ldist = teams[,list(lteamid=teamid, llat=lat, llng=lng)]

# regular_season_compact_results = merge(
#   regular_season_compact_results,
#   wdist, by='wteamid', all.x=TRUE
# )
#
# regular_season_compact_results = merge(
#   regular_season_compact_results,
#   ldist, by='lteamid', all.x=TRUE
# )

# regular_season_compact_results[, dist := distCosine(cbind(wlng, wlat), cbind(llng, llat)) / 1609.34]

# regular_season_compact_results[wloc == 'N', dist := NA]
# regular_season_compact_results[wloc == 'A', wdist := dist]
# regular_season_compact_results[wloc == 'H', wdist := 0]
# regular_season_compact_results[wloc == 'A', ldist := 0]
# regular_season_compact_results[wloc == 'H', ldist := dist]

#Use median distance for neutral game
# med_dist <- regular_season_compact_results[,median(dist, na.rm=TRUE)]
# regular_season_compact_results[is.na(wdist), wdist := med_dist]
# regular_season_compact_results[is.na(ldist), ldist := med_dist]
# regular_season_compact_results[, dist := NULL]

##########################################
# Seed and slot print positions
##########################################
seed_print_positions <- fread('inst/kaggle_data/seed_print_positions.csv')
slot_print_positions <- fread('inst/kaggle_data/slot_print_positions.csv')

##########################################
# Assign find every possible matchup and what slot it would occur in
##########################################

#Build the base data-structure linking slots together
slot_map <- reshape2::melt(tourney_slots, id.vars=c('women', 'season', 'slot'), value.name='prior_slot')
slot_map <- slot_map[,list(women, season, slot=slot, prior_slot)]
tourny_tree <- slot_map[,list(women, season, seed=prior_slot, slot1=slot, slot2=prior_slot)]

sink <- tourny_tree[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- tourny_tree[women==1, stopifnot(all(THIS_YEAR %in% season))]

#Self join the slot map to build a tourney tree
for(i in 2:7){
  slots_merge <- data.table::copy(slot_map)
  slot1 <- paste0('slot', i)
  slot2 <- paste0('slot', i+1)
  data.table::setnames(slots_merge, c('slot', 'prior_slot'), c(slot1, slot2))
  tourny_tree <- merge(tourny_tree, slots_merge, by=c('women', 'season', slot1), all.x=TRUE, allow.cartesian=TRUE)
  data.table::set(tourny_tree, i=which(!is.na(tourny_tree[[slot2]])), j='seed', value=tourny_tree[[slot2]][!is.na(tourny_tree[[slot2]])])
}

#Order and sort the tourney tree
keys <- c('women', 'season', 'seed', 'slot1', 'slot2', 'slot3', 'slot4', 'slot5', 'slot6', 'slot7', 'slot8')
setdiff(names(tourny_tree), keys)
data.table::setcolorder(tourny_tree, keys)
data.table::setkeyv(tourny_tree, keys)
sink <- tourny_tree[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- tourny_tree[women==1, stopifnot(all(THIS_YEAR %in% season))]

#Convert to possible 2-team matchups
all_slots <- tourny_tree[,list(women, season, slot=slot1, seed)]
all_slots <- merge(all_slots, tourney_seeds, by=c('women', 'season', 'seed'), all.x=TRUE)
all_slots <- merge(all_slots, all_slots, by=c('women', 'season', 'slot'), suffixes=c('_1', '_2'), allow.cartesian=TRUE)
sink <- all_slots[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- all_slots[women==1, stopifnot(all(THIS_YEAR %in% season))]
all_slots[teamid_1 == 3107 & teamid_2 == 3124 & season == THIS_YEAR,]

#Add round
all_slots[, round := as.integer(NA)]
all_slots[grepl('R', slot), round := as.integer(substr(slot, 2, 2))]
all_slots[!grepl('R', slot), round := 0]
sink <- all_slots[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- all_slots[women==1, stopifnot(all(THIS_YEAR %in% season))]

#Remove impossible matchups
all_slots <- all_slots[teamid_1 != teamid_2,]
all_slots[, first_matchup := min(round) == round, keyby=c('women', 'season', 'teamid_1', 'teamid_2')]
all_slots <- all_slots[first_matchup==TRUE,]
all_slots[,first_matchup := NULL]
sink <- all_slots[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- all_slots[women==1, stopifnot(all(THIS_YEAR %in% season))]

#Cleanup final dataset
all_slots <- all_slots[teamid_1 != teamid_2,]
sink <- all_slots[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- all_slots[women==1, stopifnot(all(THIS_YEAR %in% season))]

##########################################
# Find next slot for each slot
##########################################

#Last time we found the prior slot
#Now we're looking for the next slot
slot_tree <- reshape2::melt(tourney_slots[,list(women, season, slot, strongseed, weakseed)], id.vars=c('women', 'season', 'slot'))
data.table::setnames(slot_tree, c('slot', 'value'), c('next_slot', 'slot'))
slot_tree <- slot_tree[,list(women, season, slot, next_slot)]

#Join the forward-looking slot tree
#Note that the next slot for the championship game will be NA
all_slots <- merge(all_slots, slot_tree, by = c('women', 'season', 'slot'), all.x=TRUE)

##########################################
# ID playin games
##########################################
team_playins <- all_slots[round == 0, list(women, season, teamid_1, teamid_1_playedin=1L)]
opp_playins  <- all_slots[round == 0, list(women, season, teamid_2, teamid_2_playedin=1L)]
all_slots <- merge(all_slots, team_playins, by=c('women', 'season', 'teamid_1'), all.x=TRUE)
all_slots <- merge(all_slots, opp_playins, by=c('women', 'season', 'teamid_2'), all.x=TRUE)

all_slots[is.na(teamid_1_playedin), teamid_1_playedin := 0L]
all_slots[is.na(teamid_2_playedin), teamid_2_playedin := 0L]

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

# dist_1 = teams[,list(teamid_1=teamid, lat_1=lat, lng_1=lng)]
# dist_2 = teams[,list(teamid_2=teamid, lat_2=lat, lng_2=lng)]
# all_slots <- merge(all_slots, geo_tourney, by=c('season', 'slot'), all.x=TRUE)
# all_slots = merge(all_slots, dist_2, by='teamid_2')
# all_slots = merge(all_slots, dist_1, by='teamid_1')
setkeyv(all_slots, c('season', 'teamid_1', 'teamid_2'))
# rm(dist_1, dist_2)

# all_slots[, dist_1 := distCosine(cbind(lng_1, lat_1), cbind(host_lng, host_lat)) / 1609.34]
# all_slots[, dist_2 := distCosine(cbind(lng_2, lat_2), cbind(host_lng, host_lat)) / 1609.34]

##########################################
# Add all-slots geo to tourney compact
##########################################

tourney_geo = all_slots[,list(
  women,
  season,
  wteamid = teamid_1,
  lteamid = teamid_2
  #wlat = lat_1,
  #wlng = lng_1,
  #llat = lat_2,
  #llng = lng_2,
  #wdist = dist_1,
  #ldist = dist_2
  )]

tourney_compact_results <- merge(
  tourney_compact_results,
  tourney_geo,
  by=c('women', 'season', 'wteamid', 'lteamid'),
  all.x=TRUE
)

setcolorder(tourney_compact_results, names(regular_season_compact_results))
tourney_compact_results

##########################################
# Replace sample sub with a simple seed diff benchmark
##########################################

# Wrangle a training set of tournament data
tmp_dat <- tourney_compact_results[
  !season %in% THIS_YEAR,list(women, season, wteamid, lteamid, wscore, lscore)]
tmp_dat[,id1 := stri_paste(season, wteamid, lteamid, sep='_')]
tmp_dat[,id2 := stri_paste(season, lteamid, wteamid, sep='_')]
tmp_dat[wteamid < lteamid, id := id1]
tmp_dat[wteamid > lteamid, id := id2]
tmp_dat[,c('id1', 'id2') := NULL]
rename = c('teamid', 'score')
oldnames <- c(paste0('w', rename), paste0('l', rename))
newnames <- c(paste0(rename, '_1'), paste0(rename, '_2'))
setnames(tmp_dat, oldnames, newnames)

# Add a test set of the prediction data
ids <- stri_split_fixed(sample_submission[['id']], '_')
sample_submission[,pred := NULL]
sample_submission[, season := as.integer(sapply(ids, '[[', 1))]
sample_submission[, teamid_1 := as.integer(sapply(ids, '[[', 2))]
sample_submission[, teamid_2 := as.integer(sapply(ids, '[[', 3))]
tmp_dat <- rbind(tmp_dat, sample_submission, fill=T)
sample_submission[,c('season', 'teamid_1', 'teamid_2') := NULL]

# Make the dataset symmetric
team1 <- tmp_dat[,list(
  id, women, season,
  teamid_1, teamid_2,
  score_1, score_2)]
team1[id == '2014_1107_1110',]
team1[id == '2014_1107_1196',]
team1[id == 'THIS_YEAR_3107_3124',]

team2 <- tmp_dat[,list(
  id, women, season,
  teamid_1=teamid_2, teamid_2=teamid_1,
  score_1=score_2, score_2=score_1)]
team2[id == 'THIS_YEAR_3107_3124',]

tmp_dat <- rbindlist(list(team1, team2))
rm(team1, team2)
gc(reset=TRUE)
tmp_dat[id == 'THIS_YEAR_3107_3124',]

#Add seeds
all_slots[teamid_1 == 3107 & teamid_2 == 3124 & season == THIS_YEAR,]
tmp_dat_2 <- all_slots[,list(women, season, teamid_1, teamid_2, seed_1, seed_2)]
tmp_dat_2[, seed_1 := as.integer(gsub('[[:alpha:]]', '', seed_1))]
tmp_dat_2[, seed_2 := as.integer(gsub('[[:alpha:]]', '', seed_2))]
tmp_dat_2[,seed_diff := seed_1 - seed_2]
tmp_dat_2[,c('seed_1', 'seed_2') := NULL]
tmp_dat_2[teamid_1 == 3107 & teamid_2 == 3124 & season == THIS_YEAR,]

tmp_dat <- merge(tmp_dat, tmp_dat_2, by=c('women', 'season', 'teamid_1', 'teamid_2'))
tmp_dat[id == 'THIS_YEAR_3107_3124',]

#Model and Predict
tmp_dat[,won := as.integer(score_1 > score_2)]
model <- tmp_dat[!is.na(won), glm(won ~ 0 + seed_diff + women:seed_diff, family='binomial')]
summary(model)
tmp_dat <- tmp_dat[,pred := predict(model, tmp_dat, type='response')]
setkeyv(tmp_dat, 'id')
# summary(tmp_dat[,sum(pred),by='id'])
# summary(tmp_dat[,sum(won),by='id'])
# summary(tmp_dat[,sum(seed_diff),by='id'])

#Add to sample sub
#tmp_dat <- tmp_dat[,id := paste(season, teamid_1, teamid_2, sep='_')]
tmp_dat <- tmp_dat[teamid_1 < teamid_2 & season %in% THIS_YEAR,]
tmp_dat <- tmp_dat[,list(id, pred)]
sample_submission <- merge(sample_submission, tmp_dat, by='id', all.x=T)

stopifnot(! anyNA(sample_submission$pred))
sample_submission[is.na(pred), pred := .50]

#Save so we can upload to kaggle
sample_submission_women = sample_submission[women==1,list(id, pred)]
sample_submission_men = sample_submission[women==0,list(id, pred)]
write.csv(sample_submission_women, 'inst/kaggle_data/seed_benchmark_women.csv', row.names=F)
write.csv(sample_submission_men, 'inst/kaggle_data/seed_benchmark_men.csv', row.names=F)

##########################################
# Checks
##########################################

#set slot order for all slot objects
slot_print_positions[,slot := factor(slot, levels=slot_order)]
tourney_slots[,slot := factor(slot, levels=slot_order)]
slot_print_positions[,slot := factor(slot, levels=slot_order)]
all_slots[,slot := factor(slot, levels=slot_order)]

sink <- all_slots[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- all_slots[women==1, stopifnot(all(THIS_YEAR %in% season))]

sink <- tourney_slots[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- tourney_slots[women==1, stopifnot(all(THIS_YEAR %in% season))]

sink <- regular_season_compact_results[women==0, stopifnot(all(THIS_YEAR %in% season))]
sink <- regular_season_compact_results[women==1, stopifnot(all(THIS_YEAR %in% season))]

sink <- regular_season_detailed_results[women==0, stopifnot(all(THIS_YEAR %in% season))]
# sink <- regular_season_detailed_results[women==1, stopifnot(all(THIS_YEAR %in% season))]
stopifnot(tourney_slots[season==THIS_YEAR,any(women==1)])

##########################################
# Save
##########################################

use_data(
  seed_print_positions,
  slot_print_positions,
  sample_submission_women,
  sample_submission_men,
  teams,
  tourney_seeds,
  tourney_slots,
  seasons,
  cities,
  game_cities,
  tourney_compact_results,
  tourney_detailed_results,
  regular_season_compact_results,
  regular_season_detailed_results,
  all_slots,
  overwrite=TRUE)
