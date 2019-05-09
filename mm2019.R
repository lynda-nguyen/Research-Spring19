# libraries
library(stringr)
library(tidyr)
library(splitstackshape)
library(ggplot2)
library(tidyverse)
library(plyr)
library(gridExtra)
library(kableExtra)
library(rowr)

# load in datasets


#combine all datasets into one (going down into rows)
sweet16 <- rbind.fill(vt_data, uva.bb,
                      gonzaga.bb,michiganSt.bb,duke.bb,michigan.bb,
                      northCarolina.bb,kentucky.bb,texasTech.bb,purdue.bb,tennessee.bb,
                      houston.bb, auburn.bb, floridaSt.bb, lsu.bb, oregon.bb)

#combining by columns
schools_list <- cbind.fill(vt_data, uva.bb, gonzaga.bb,michiganSt.bb,duke.bb,michigan.bb, northCarolina.bb,kentucky.bb,texasTech.bb,purdue.bb,tennessee.bb, houston.bb, auburn.bb, floridaSt.bb, lsu.bb, oregon.bb)
numOfSchools <- (length(schools_list))/10
#include headers: Date	Rk	Opponent	Result	 	 	Location	Record	Conf	
colnames(sweet16) <- c("Date","Rank","Opponent Rank","Opponent","Result","N/A","OT","Location","Record","Conf")

#create a column that indicates which team by going through # rows
numRows <- c(vt_rows,uva_rows,gonzaga_rows,michiganSt_rows,duke_rows,michigan_rows,northCarolina_rows,kentucky_rows, texasTech_rows, purdue_rows,tennessee_rows,houston_rows,auburn_rows,floridaSt_rows,lsu_rows,oregon_rows)
names <- c("vt.bb", "uva.bb", "gonzaga.bb","michiganSt.bb","duke.bb","michigan.bb", "northCarolina.bb","kentucky.bb","texasTech.bb","purdue.bb","tennessee.bb", "houston.bb", "auburn.bb", "floridaSt.bb", "lsu.bb", "oregon.bb")
school_change <- 1
counter <- 1
School <- ""
for(i in numRows){
  for (j in 1:(i)){
    School[counter] <- names[school_change]
    counter <- counter + 1
  }
  school_change <- school_change + 1
}
sweet16 <- cbind(sweet16, School)

## DATA CLEANING
post_rows <- which(is.na(sweet16$Rank),arr.ind=TRUE)
post_rows <- post_rows[-3] # vt data is before the rest
sweet16.data <- sweet16[-37,] #^  

# replace the second column for the cumsum of the initial second column
numRowsCum <- cumsum(numRows)
post_rows <- post_rows[c(TRUE, FALSE)]

#test by removing VT post season
sweet16.data <- sweet16.data[-c(post_rows[1]: (numRowsCum[1]-1)),]
deletedVT <- length(post_rows[1]: (numRowsCum[1]-1)) + 1

for(i in 2:numOfSchools){
  for(j in post_rows[i]:(numRowsCum[i])){
    sweet16.data[j - deletedVT,] <- NA
  }
}

sweet16.df <- sweet16.data[!is.na(sweet16.data$Rank), ] # result
sweet16.df <- data.frame(sweet16.df)

Win_Lost <- str_split_fixed(sweet16.df$Result, ",", 2)[,1]
scores <- str_split_fixed(sweet16.df$Result, ",", 2)[,2]
Home_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,1])
Opp_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,2])

s16_data <- cbind(sweet16.df , Win_Lost,Home_Points,Opp_Points)
s16_data <- s16_data[,c(1,2,3,4,5,6,11,12,13,14,7,8,9,10)]
s16_data <- s16_data[, -5] #deletes original results and n/a columns
s16_data  <- s16_data[, -5] #deletes original results and n/a columns

# we need to swap points were Home loses, because while cleaning we got all the higher points
Home_points2 <- Home_Points
s16.df <- data.frame(cbind(s16_data, Home_points2))

for (i in (which(s16.df$Win_Lost == "L"))){
  s16.df$Home_Points[i] = s16.df$Opp_Points[i]
  s16.df$Opp_Points[i] = s16.df$Home_points2[i]
}
s16.df <- s16.df[, -ncol(s16.df)]

home_loc <- which(s16.df$Location == "Home")

# x(g) = margin by which the home team beats the road team
x.home <- 0
for (g in home_loc){
  x.home[g] <- s16.df$Home_Points[g] - s16.df$Opp_Points[g]
}

#place the home_loc according to row
s16.df <- cbind.fill(s16.df, x.home)

# check for errors (especially at the end)
err <- which(!is.na(s16.df$object) & s16.df$Location == 'Away')
s16.df$object[err] <- NA
colnames(s16.df)[colnames(s16.df)=="object"] <- "home.point.diff" 

#which(s16.df$OT == "OT" & s16.df$Location == "Home") ##considering overtime
home.game.count <- length(x.home) - length(which(is.na(x.home)))


#####################################################################################
#2019
abileneChr.bb <- read.csv("~/Downloads/AbileneChristian19_Stats - Sheet1.csv", header = FALSE)
abileneChr_rows <- nrow(abileneChr.bb)
belmont.bb <- read.csv("~/Downloads/Belmont19_Stats - Sheet1.csv", header = FALSE)
belmont_rows <- nrow(belmont.bb)
baylor.bb <- read.csv("~/Downloads/Baylor19_Stats - Sheet1.csv", header = FALSE)
baylor_rows <- nrow(baylor.bb)
arizonaSt.bb <- read.csv("~/Downloads/ArizonaSt19_Stats - Sheet1.csv", header = FALSE)
arizonaSt_rows <- nrow(arizonaSt.bb)
bradley.bb <- read.csv("~/Downloads/Bradley19_Stats - Sheet1.csv", header = FALSE)
bradley_rows <- nrow(bradley.bb)
buffalo.bb <- read.csv("~/Downloads/Buffalo19_Stats - Sheet1.csv", header = FALSE)
buffalo_rows <- nrow(buffalo.bb)
cincinnati.bb <- read.csv("~/Downloads/Cincinati19_Stats - Sheet1.csv", header = FALSE)
cincinnati_rows <- nrow(cincinnati.bb)
colgate.bb <- read.csv("~/Downloads/Colgate19_Stats - Sheet1.csv", header = FALSE)
colgate_rows <- nrow(colgate.bb)
fairDick.bb <- read.csv("~/Downloads/FairleighDickinson19_Stats - Sheet1.csv", header = FALSE)
fairDick_rows <- nrow(fairDick.bb)
florida.bb <- read.csv("~/Downloads/Florida19_Stats - Sheet1.csv", header = FALSE)
florida_rows <- nrow(florida.bb)
gardWebb.bb <- read.csv("~/Downloads/GardnerWebb19_Stats - Sheet1.csv", header = FALSE)
gardWebb_rows <- nrow(gardWebb.bb )
georgiaSt.bb <- read.csv("~/Downloads/GeorgiaSt19_Stats - Sheet1.csv", header = FALSE)
georgiaSt_rows <- nrow(georgiaSt.bb)
iona.bb <- read.csv("~/Downloads/Iona19_Stats - Sheet1.csv", header = FALSE)
iona_rows <- nrow(iona.bb)
iowa.bb <- read.csv("~/Downloads/Iowa19_Stats - Sheet1.csv", header = FALSE)
iowa_rows <- nrow(iowa.bb)
iowaSt.bb <- read.csv("~/Downloads/IowaSt19_Stats - Sheet1.csv", header = FALSE)
iowaSt_rows <- nrow(iowaSt.bb)
kansas.bb <- read.csv("~/Downloads/Kansas19_Stats - Sheet1.csv", header = FALSE)
kansas_rows <- nrow(kansas.bb)
kansasSt.bb <- read.csv("~/Downloads/KansasSt19_Stats - Sheet1.csv", header = FALSE)
kansasST_rows <- nrow(kansasSt.bb)
liberty.bb <- read.csv("~/Downloads/Liberty19_Stats - Sheet1.csv", header = FALSE)
liberty_rows <- nrow(liberty.bb)
louisville.bb <- read.csv("~/Downloads/Lousville19_Stats - Sheet1.csv", header = FALSE)
louisville_rows <- nrow(louisville.bb)
marquette.bb <- read.csv("~/Downloads/Marquette19_Stats - Sheet1.csv", header = FALSE)
marquette_rows <- nrow(marquette.bb)
maryland.bb <- read.csv("~/Downloads/Maryland19_Stats - Sheet1.csv", header = FALSE)
maryland_rows <- nrow(maryland.bb)
minnesota.bb <- read.csv("~/Downloads/Minnesota19_Stats - Sheet1.csv", header = FALSE)
minnesota_rows <- nrow(minnesota.bb)
mississippiSt.bb <- read.csv("~/Downloads/MississippiSt19_Stats - Sheet1.csv", header = FALSE)
mississippiSt_rows <- nrow(mississippiSt.bb)
montana.bb <- read.csv("~/Downloads/Montana19_Stats - Sheet1.csv", header = FALSE)
montana_rows <- nrow(montana.bb)
murraySt.bb <- read.csv("~/Downloads/MurraySt19_Stats - Sheet1.csv", header = FALSE)
murraySt_rows <- nrow(murraySt.bb)
nevada.bb <- read.csv("~/Downloads/Nevada19_Stats - Sheet1.csv", header = FALSE)
nevada_rows <- nrow(nevada.bb)
newmexicoSt.bb <- read.csv("~/Downloads/NewMexicoSt19_Stats - Sheet1.csv", header = FALSE)
newmexicoSt_rows <- nrow(newmexicoSt.bb)
ndSt.bb <- read.csv("~/Downloads/NorthDakotaSt19_Stats - Sheet1.csv", header = FALSE)
ndSt_rows <- nrow(ndSt.bb)
northEastern.bb <- read.csv("~/Downloads/Northeastern19_Stats - Sheet1.csv", header = FALSE)
northEastern_rows <- nrow(northEastern.bb)
northernKentucky.bb <- read.csv("~/Downloads/NorthernKentucky19_Stats - Sheet1.csv", header = FALSE)
northernKentucky_rows <- nrow(northernKentucky.bb)
ohioSt.bb <- read.csv("~/Downloads/OhioSt19_Stats - Sheet1.csv", header = FALSE)
ohioSt_rows <- nrow(ohioSt.bb)
oklahoma.bb <- read.csv("~/Downloads/Oklahoma19_Stats - Sheet1.csv", header = FALSE)
oklahoma_rows <- nrow(oklahoma.bb)
odu.bb <- read.csv("~/Downloads/OldDominion19_Stats - Sheet1.csv", header = FALSE)
odu_rows <- nrow(odu.bb)
oleM.bb <- read.csv("~/Downloads/OleMiss19_Stats - Sheet1.csv", header = FALSE)
oleM_rows <- nrow(oleM.bb)
stLouis.bb <- read.csv("~/Downloads/SaintLouis19_Stats - Sheet1.csv", header = FALSE)
stLouis_rows <- nrow(stLouis.bb)
stMarys.bb <- read.csv("~/Downloads/SaintMarys19_Stats - Sheet1.csv", header = FALSE)
stMarys_rows <- nrow(stMarys.bb)
seton.bb <- read.csv("~/Downloads/SetonHall19_Stats - Sheet1.csv", header = FALSE)
seton_rows <- nrow(seton.bb)
syracuse.bb <- read.csv("~/Downloads/Syracuse19_Stats - Sheet1.csv", header = FALSE)
syracuse_rows <- nrow(syracuse.bb)
ucf.bb <- read.csv("~/Downloads/UCF19_Stats - Sheet1.csv", header = FALSE)
ucf_rows <- nrow(ucf.bb)
ucIrvine.bb <- read.csv("~/Downloads/UCIrvine19_Stats - Sheet1.csv", header = FALSE)
ucIrvine_rows <- nrow(ucIrvine.bb)
utahSt.bb <- read.csv("~/Downloads/UtahSt19_Stats - Sheet1.csv", header = FALSE)
utahSt_rows <- nrow(utahSt.bb)
vcu.bb <- read.csv("~/Downloads/VCU19_Stats - Sheet1.csv", header = FALSE)
vcu_rows <- nrow(vcu.bb)
vermont.bb <- read.csv("~/Downloads/Vermont19_Stats - Sheet1.csv", header = FALSE)
vermont_rows <- nrow(vermont.bb)
villanova.bb <- read.csv("~/Downloads/Villanova19_Stats - Sheet1.csv", header = FALSE)
villanova_rows <- nrow(villanova.bb)
washington.bb <- read.csv("~/Downloads/Washington19_Stats - Sheet1.csv", header = FALSE)
washington_rows <- nrow(washington.bb)
wisconsin.bb <- read.csv("~/Downloads/Wisconsin19_Stats - Sheet1.csv", header = FALSE)
wisconsin_rows <- nrow(wisconsin.bb)
wofford.bb <- read.csv("~/Downloads/Wofford19_Stats - Sheet1.csv", header = FALSE)
wofford_rows <- nrow(wofford.bb)
yale.bb <- read.csv("~/Downloads/Yale19_Stats - Sheet1.csv", header = FALSE)
yale_rows <- nrow(yale.bb)

mm2019 <- rbind.fill(abileneChr.bb, arizonaSt.bb, baylor.bb, belmont.bb, bradley.bb,buffalo.bb,cincinnati.bb, colgate.bb, fairDick.bb, florida.bb, gardWebb.bb, georgiaSt.bb,iona.bb, iowa.bb, iowaSt.bb, kansas.bb, kansasSt.bb, liberty.bb, louisville.bb,marquette.bb, maryland.bb, minnesota.bb, mississippiSt.bb, montana.bb, murraySt.bb,nevada.bb, newmexicoSt.bb, ndSt.bb, northEastern.bb, northernKentucky.bb, ohioSt.bb,oklahoma.bb, odu.bb, oleM.bb, stLouis.bb, stMarys.bb, seton.bb, syracuse.bb, ucf.bb,ucIrvine.bb, utahSt.bb, vcu.bb, vermont.bb, villanova.bb, washington.bb, wisconsin.bb,
wofford.bb, yale.bb)

colnames(mm2019) <- c("Date","Rank","Opponent Rank","Opponent","Result","N/A","OT","Location","Record","Conf")

#combining by columns
schools_list2 <- cbind.fill(abileneChr.bb, arizonaSt.bb, baylor.bb, belmont.bb, bradley.bb,buffalo.bb,cincinnati.bb, colgate.bb, fairDick.bb, florida.bb, gardWebb.bb, georgiaSt.bb,iona.bb, iowa.bb, iowaSt.bb, kansas.bb, kansasSt.bb, liberty.bb, louisville.bb,marquette.bb, maryland.bb, minnesota.bb, mississippiSt.bb, montana.bb, murraySt.bb,nevada.bb, newmexicoSt.bb, ndSt.bb, northEastern.bb, northernKentucky.bb, ohioSt.bb,oklahoma.bb, odu.bb, oleM.bb, stLouis.bb, stMarys.bb, seton.bb, syracuse.bb, ucf.bb,ucIrvine.bb, utahSt.bb, vcu.bb, vermont.bb, villanova.bb, washington.bb, wisconsin.bb,
                           wofford.bb, yale.bb)
numOfSchools2 <- (length(schools_list2))/10

#create a column that indicates which team by going through # rows
numRows2 <- c(abileneChr_rows,arizonaSt_rows,baylor_rows,belmont_rows,bradley_rows,buffalo_rows,cincinnati_rows,colgate_rows, fairDick_rows, florida_rows,gardWebb_rows,georgiaSt_rows,iona_rows,iowa_rows,iowaSt_rows,kansas_rows, kansasST_rows,liberty_rows,louisville_rows,marquette_rows, maryland_rows, minnesota_rows, mississippiSt_rows, montana_rows, murraySt_rows, nevada_rows, newmexicoSt_rows, ndSt_rows, northEastern_rows, northernKentucky_rows, ohioSt_rows, oklahoma_rows, odu_rows, oleM_rows, stLouis_rows, stMarys_rows, seton_rows,syracuse_rows, ucf_rows, ucIrvine_rows, utahSt_rows, vcu_rows, vermont_rows, villanova_rows, washington_rows, wisconsin_rows, wofford_rows, yale_rows)
names2 <- c("abileneChr.bb", "arizonaSt.bb", "baylor.bb", "belmont.bb", "bradley.bb","buffalo.bb","cincinnati.bb", "colgate.bb", "fairDick.bb", "florida.bb", "gardWebb.bb", "georgiaSt.bb","iona.bb", "iowa.bb", "iowaSt.bb", "kansas.bb", "kansasSt.bb", "liberty.bb", "louisville.bb","marquette.bb", "maryland.bb", "minnesota.bb", "mississippiSt.bb", "montana.bb", "murraySt.bb","nevada.bb", "newmexicoSt.bb", "ndSt.bb", "northEastern.bb", "northernKentucky.bb", "ohioSt.bb","oklahoma.bb", "odu.bb", "oleM.bb", "stLouis.bb", "stMarys.bb", "seton.bb", "syracuse.bb", "ucf.bb","ucIrvine.bb", "utahSt.bb", "vcu.bb", "vermont.bb", "villanova.bb", "washington.bb", "wisconsin.bb",
           "wofford.bb", "yale.bb")
school_change2 <- 1
counter2 <- 1
School2 <- ""
for(i in numRows2){
  for (j in 1:(i)){
    School2[counter2] <- names2[school_change2]
    counter2 <- counter2 + 1
  }
  school_change2 <- school_change2 + 1
}
mm2019 <- cbind(mm2019, School2)

## DATA CLEANING
post_rows2 <- which(is.na(mm2019$Rank),arr.ind=TRUE)

# replace the second column for the cumsum of the initial second column
numRowsCum2 <- cumsum(numRows2)
post_rows2 <- post_rows2[c(TRUE, FALSE)]

for(i in 1:numOfSchools2){
  for(j in post_rows2[i]:(numRowsCum2[i])){
    mm2019[j ,] <- NA
  }
}

mm2019.df <- mm2019[!is.na(mm2019$Rank), ] # result
mm2019.df <- data.frame(mm2019.df)


Win_Lost <- str_split_fixed(mm2019.df$Result, ",", 2)[,1]
scores <- str_split_fixed(mm2019.df$Result, ",", 2)[,2]
Home_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,1])
Opp_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,2])

mm2019_data <- cbind(mm2019.df , Win_Lost,Home_Points,Opp_Points)
mm2019_data <- mm2019_data[,c(1,2,3,4,5,6,11,12,13,14,7,8,9,10)]
mm2019_data <- mm2019_data[, -5] #deletes original results and n/a columns
mm2019_data  <- mm2019_data[, -5] #deletes original results and n/a columns

# we need to swap points were Home loses, because while cleaning we got all the higher points
Home_points2 <- Home_Points
mm2019_df <- data.frame(cbind(mm2019_data, Home_points2))

for (i in (which(mm2019_df$Win_Lost == "L"))){
  mm2019_df$Home_Points[i] = mm2019_df$Opp_Points[i]
  mm2019_df$Opp_Points[i] = mm2019_df$Home_points2[i]
}
mm2019_df <- mm2019_df[, -ncol(mm2019_df)]

home_loc2 <- which(mm2019_df$Location == "Home")

# x(g) = margin by which the home team beats the road team
x.home2 <- 0
for (g in home_loc2){
  x.home2[g] <- mm2019_df$Home_Points[g] - mm2019_df$Opp_Points[g]
}

#place the home_loc according to row
mm2019_df <- cbind.fill(mm2019_df, x.home2)

# check for errors (especially at the end)
err <- which(!is.na(mm2019_df$object) & mm2019_df$Location == 'Away')
mm2019_df$object[err[1]] <- NA
mm2019_df$object[err[2]] <- NA
colnames(mm2019_df)[colnames(mm2019_df)=="object"] <- "home.point.diff" 

#which(s16.df$OT == "OT" & s16.df$Location == "Home") ##considering overtime
home.game.count2 <- length(x.home2) - length(which(is.na(x.home2)))


#combine top sweet 16 with the rest of 48 teams for 2019
colnames(mm2019_df)[colnames(mm2019_df)=="School2"] <- "School" 
tourn2019 <- rbind(s16.df, mm2019_df)

####################################################################################
# get the piechart of 2019 statistics for loses
loses19.home <- length(which(tourn2019$Win_Lost == "L" & tourn2019$Location == "Home"))
loses19.away <- length(which(tourn2019$Win_Lost == "L" & tourn2019$Location == "Away"))
loses19.neutral <- length(which(tourn2019$Win_Lost == "L" & tourn2019$Location == "Neutral"))
lossSum <- length(which(tourn2019$Win_Lost == "L"))

loss.df <- data.frame(
  group = c( "Away","Home","Neutral"),
  value = c( loses19.away,loses19.home, loses19.neutral)
)

bp <- ggplot(loss.df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y") + labs(title = "Loss Percentages by Location", x = "", y = "")

pie <- pie + scale_fill_brewer(palette="Reds")+
  theme_minimal()+ geom_text(aes(label = paste0(
    scales::percent(value / sum(value))
  )), 
  position = position_stack(vjust = 0.5))
pie

#####################################################################################
# make new data frame
char_array = c(names, names2)
naming = data.frame("name.data"=char_array,"name.data2"=1:64)
naming$name.data = toupper(substr(naming$name.data,1,nchar(char_array)-3))

# get win-loss percentage
# get the accurate number for team split 
#gets the last range of the school
cum.team_split1 <-  cumsum(numRows - (numRowsCum - (post_rows-1)))
cum.team_split2 <-  cumsum(numRows2 - (numRowsCum2 - (post_rows2-1)))
sumTeamCumSplit <- c(cum.team_split1, cum.team_split1[16] + cum.team_split2)

win.per <- 0
lost.per <- 0
for (i in 1:length(sumTeamCumSplit)){ #increment
  win.per[i] <- length(which(tourn2019$School == char_array[i] & tourn2019$Win_Lost == "W"))
  lost.per[i] <- length(which(tourn2019$School == char_array[i] & tourn2019$Win_Lost == "L"))
}

stat2019 <- cbind("Team" = naming$name.data, "Wins" = win.per, "Loses" = lost.per, "(W-L)%" = win.per/(win.per +lost.per))
stat2019 <- data.frame(stat2019)
#####################################################################################
# strength of schedule
sos <- read.csv("~/Downloads/StrengthOfSchedule - Sheet1.csv", header = FALSE)

colnames(sos) <- c("RANK","SCHOOL",	"W-L"	,"SOS",	"OPPONENT WINNING PCT."	,"RANK"	,"W-L",	"SOS",	"RANK",	"RPI")
sos$SCHOOL <- toupper(sos$SCHOOL)

j <- 1
i <- 1
sos.score <- 0
while (j < nrow(sos)){
  while (i <= length(stat2019$Team)){
    if (sos$SCHOOL[j] == stat2019$Team[i]){
      sos.score[i] <- sos$SOS[j]
      i <- i + 1
      j <- 1
    }
    else{
      j <- j + 1
    }
  }
  if (!is.na(sos.score[length(stat2019$Team)])){
    break
  }
}

stat2019 <- cbind(stat2019, "SOS" = sos.score)

# march madness outcomes
stages <- c(4, 1, 3, 2, 3, 4, 4, 3, 1, 3, 4, 4, 2, 4, 4, 4, 6, 6, 5, 6, 6, 5, 6, 6, 6, 5, 6, 6, 6, 5, 6, 5, 6, 5, 6, 6, 5, 5, 6, 6, 5, 6, 6, 6, 6, 6, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 6, 6, 6, 5, 5, 5, 5, 6)

stat2019 <- cbind(stat2019, stages)

#####################################################################################
# massey

massey.data <- read.csv("~/Downloads/Massey - Sheet1.csv", header = FALSE)
head(massey.data)

def <- str_sub(massey.data$V6, -5, -1)
off <- str_sub(massey.data$V3, -6, -1)
check.off <- which(substring(off, 1, 1) == 1)
partial.off <- as.numeric(substring(off, 2))

mOff <- c(96.07, 96.04, 105.17, 101.25, 105.17, 94.38, 108.56,97.81,97.73,102.77,104.77,96.40,103.86,98.41,103.38,91.09,87.33,97.74,95.96,101.27,85.03,103.70,93.59,93.01,90.10,90.52,92.01,94.36,92.77,101.42,100.20,100.16,89.37,88.76,96.49,96.79,93.91,95.50,100.38,90.20,97.92,96.75,94.33,86.32,92.92,93.57,92.02,96.14,82.85,96.65,86.47,91.42,95.45,92.64,93.47,91.28,96.29,90.18,90.23,94.65,90.89,91.33,98.56,99.22)
mDef <- c(31.13, 38.15, 26.92, 31.88, 26.94, 36.57, 22.46,32.95,35.20,26.15,24.00,31.99,26.48,28.72,21.09,32.87,21.69,19.78,24.88,18.21,22.90,20.37,29.63,17.98,15.24,32.63,15.86,18.76,12.82,19.71,26.05,25.69,34.92,88.76,25.45,22.85,28.26,25.19,23.29,19.87,21.96,22.09,24.29,12.77,21.48,19.40,27.37,26.19,28.46,21.60,26.72,30.62,22.20,26.06,28.62,24.53,22.20,26.17,24.60,27.16,28.58,32.11,26.54,15.75)
#####################################################################################
# kenpom  

kenpom <- read.csv("~/Downloads/kenpom - Sheet1.csv", header = FALSE)
head(kenpom)

schools <- toupper(kenpom$V2)
schools <- substr(schools, 1, nchar(schools)-1) 
schools <- substr(schools, 1, nchar(schools)-1) 
adjO <- c(118.1, 123.4, 124.5, 121.0, 120, 114.5, 119.7, 117.6, 114.1, 122.5, 122.7, 115.3, 120.9, 112.8, 117.7, 109.5, 103.4, 109.7, 115.3, 115.6, 101.5, 115.2, 112.3, 112.4, 110.5, 106.5, 107.8, 107.7, 106.9,117.4,118.9, 113.9, 108.4, 111.1, 113.7,113.2, 110.2, 113.4, 111.4, 117.7, 107.4, 112.5, 114.0, 113.0, 107.4,111.0, 109.4,101.4,113.2, 104.2, 102.2,114.7,109.0,110.5, 112.1, 107.4, 112.6, 104.3 , 108.5, 116.5, 107.7, 110.4, 118.1, 111.6)
adjD <- c(94.0, 89.2, 91.6, 90.2, 89.3, 86.2, 92.0, 90.1,84.1, 95.6,96.5 , 91.2, 95.9, 90.4, 97.5,91.7, 102.0, 98.2, 98.8,101.5, 101.4,95.4, 94.8, 105.9, 110.7, 92.2,107.8, 102.8,109.7,101.3, 96.8, 92.3,88.4, 99.6, 94.4, 96.7, 94.1,97.0,97.6, 103.9,98.6, 95.9, 99.3,110.6, 103.4, 102.3,94.4,94.1,96.0,99.2,96.2,97.4,97.5,95.3,95.6,98.2,97.2, 89.5, 99.6,99.2,93.5,88.5,97.4, 102.8)

stat2019 <- cbind(stat2019, adjO, adjD)
stat2019 <- cbind(stat2019, mOff, mDef)

       
#####################################################################################

ggplot(stat2019, aes(SOS,adjD, color = clusters)) + geom_point()
ggplot(stat2019, aes(SOS,mDef, color = clusters)) + geom_point()

set.seed(2)
bbCluster <- kmeans(stat2019[, 4:5], 6, nstart = 20)
bbCluster

table(bbCluster$cluster, bbCluster$cluster)

stat2019.c <- stat2019[,-6]
stat2019.c <- stat2019.c[,-1]

# K-Means Cluster Analysis
fit <- kmeans(stat2019.c, 6) # 5 cluster solution
# get cluster means 
aggregate(stat2019.c,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(stat2019.c, fit$cluster)

# vary parameters for most readable graph
head(stat2019.c)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=4, lines=0, main = "CLUSPLOT of Stages in March Madness")

library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
#library(h2o)          # an extremely fast java-based platform
library(party)

# Create the forest.
output.forest <- randomForest(stages ~ mOff + mDef, 
                              data = stat2019)

#stat2019$X.W.L..
# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2)) 


# Set random seed to make results reproducible:
set.seed(17)

colnames(stat2019.cc) <- c("Teams", "Wins", "Loses", "W/L.Per", "Adj.O", "Adj.D", "Mas.O", "Mas.D")

#remove teams
stat2019.cc <- stat2019[,-1]
stat2019.cc$stages <- as.factor(stat2019.cc$stages)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(stat2019.cc)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(stat2019.cc), size = data_set_size)
# Assign the data to the correct sets
training <- stat2019.cc[indexes,]
training$stages <- as.factor(training$stages)
validation1 <- stat2019.cc[-indexes,]

new.dat <- droplevels(stat2019.cc)
rf1 <- ranger(stages~., data=new.dat)
rf2 <- ranger(stages~ adjO + adjD, data=new.dat)
fit3 <- randomForest(factor(stages)~., data=training, importance = TRUE)
VI_F=varImp(fit3)
importance(fit3)
varImpPlot(VI_F,type=2)

#rf_classifier = randomForest(stages ~ X.W.L.. + SOS + adjO + adjD + mOff +mDef, data=training, ntree=100,importance=TRUE)

