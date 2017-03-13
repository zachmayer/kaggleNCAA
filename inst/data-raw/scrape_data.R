#http://kenpom.com/
library('data.table')
library('pbapply')
library('XML')
library('stringdist')
library('stringi')
rm(list=ls(all=TRUE))
gc(reset=TRUE)
set.seed(8865)

#Load Spellings
spell <- fread('inst/kaggle_data/TeamSpellings.csv')

#Manually add some spellings
new1 <- copy(spell[name_spelling == 'citadel',])
new1[,name_spelling := 'the citadel']

new2 <- copy(spell[name_spelling == 'fort wayne(ipfw)',])
new2[,name_spelling := 'fort wayne']

new3 <- copy(spell[name_spelling == 'fort wayne(ipfw)',])
new3[,name_spelling := 'ft wayne']

spell <- rbindlist(list(
  spell,
  new1,
  new2,
  new3
))

#Scrape ratings
dat_list <- pblapply(2002:2017, function(x){
  Sys.sleep(1)
  out <- readHTMLTable(paste0('http://kenpom.com/index.php?y=', x))[[1]]
  data.table(
    Season = x,
    out
  )
})

#Combine and cleanup dataframe
dat <- rbindlist(dat_list)
remove <- paste0('V', c(7, 9, 11, 13, 15, 17, 19, 21))
set(dat, j=remove, value=NULL)
setnames(dat, c(
  'Season',
  'Rank',
  'Team',
  'Conf',
  'W-L',
  'AdjEM',
  'AdjO',
  'AdjD',
  'AdjT',
  'Luck',
  'schedule_AdjEM',
  'schedule_OppO',
  'schedule_OppD',
  'conf_AdjEM'
))

dat <- dat[!(is.na(AdjEM) | is.na(Rank) | is.na(schedule_AdjEM)),]
for(var in names(dat)[6:ncol(dat)]){
  set(dat, j=var, value=as.numeric(dat[[var]]))
}

#Match to spellings
cleanup <- function(x){
  x <- gsub('[[:digit:]]+', ' ', x)
  x <- gsub('[[:space:]]+', ' ', x)
  x <- stringi::stri_trim_both(x)
  x <- stringi::stri_trans_tolower(x)
  return(x)
}
dat[,Team := cleanup(Team)]
spell[,name_spelling := cleanup(name_spelling)]

matches <- amatch(dat[['Team']], spell[['name_spelling']], method='cosine')

dat[,team_id := spell[matches, 'team_id']]
dat[,alternative_spelling := spell[matches, 'name_spelling']]
dat[is.na(team_id),]
dat[,match_rating := 1-stringdist(Team, alternative_spelling, method='cosine')]
dat[Team != alternative_spelling,][order(match_rating),unique(data.table(Team, alternative_spelling, match_rating))]

#Save
PomeryRatings <- dat
write.csv(PomeryRatings, 'inst/kaggle_data/PomeryRatings.csv', row.names=FALSE)
devtools::use_data(PomeryRatings, overwrite=TRUE)
