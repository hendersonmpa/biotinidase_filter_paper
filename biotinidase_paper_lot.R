library("tidyverse")
library("lubridate")
library("magrittr")
library("readxl")
library("mcr")
library("xts")
library("TTR")
library("RODBC")
library("xtable")
library("TSA")
					#  library("forecast")
options(warn=-1) ## options(warn=0) to turn back on
## Suppress summarise info
options(dplyr.summarise.inform = FALSE)
## options(tibble.width = Inf)
## options(tibble.print_max = Inf) 
today <- as.Date(now())
source("credentials.r")

form2lot <- function(letter, number){
  switch(letter,
	 A = {if (number > 715926 && number <= 791000) {
		return("B6861310")
	      } else if (number > 791000 && number <= 866075) {
		return("B6884210")
	      } else if (number > 866075 && number <= 9441150) {
		return("B6884210b")
	      } else if (number > 9441150) {
		return("B6900911")
	      } else {
		return("A")
	      }
	 },
	 B = {if (number <= 16226) {
		return("B6900911")
	      } else if (number > 16226 && number <= 91301) {
		return("B6911711")
	      } else if (number > 91301 && number <= 166376) {
		return("B6921412")
	      } else if (number > 166376 && number <= 241451) {
		return("B6931512")
	      } else if (number > 241451 && number <= 316526) {
		return("B6941613")
	      } else if (number > 316526 && number <= 354101) {
		return("B6958113")
	      } else if (number > 354101 && number <= 429176) {
		return("B6967813")
	      } else if (number > 429176 && number <= 541751 ) {
		return("B6978414")
	      } else if (number > 541751 && number <= 616826 ) {
		return("B6996714")
	      } else if (number > 616826 && number <= 692604 ) {
		return("B7012515")
	      } else if (number > 692604 && number <= 793323 ) {
		return("B7019815")
	      } else if (number > 793323 && number <= 868401 ) {
		return("W152")
	      } else if (number > 868401 && number <= 914685 ) {
		return("B7052616")
	      } else if (number > 914685 && number <= 991436 ) {
		return("W161")
	      } else if (number > 991436 && number <= 999925 ) {
		return("W161")
	      } else {
		return("B")
		##return(paste0(letter,number))
	      }
	 },
	 C = {if (number <=30100) { 
		return("W161")
	      }else if (number > 30100 && number <= 105700) {
		return("B7078017")
	      } else if (number > 105700 && number <= 180700) {
		return("B7087317")
	      } else if (number > 180700 && number <= 257871) {
		return("B7101418")
	      } else if (number > 257871){
		return("B7115218")
	      } else {
		return("C")
					#return(paste0(letter,number))
	      }

	 },
	 "Unknown")}


biotquery <- "select s.spcextcode1 as accession,
	   a.ansTimeMeasured as measured_time,
	   s.spcExtcode2 as form,
	   sd.sd2GestationAge as ga,
	   sd.sd2Weight as bw,
	   sd.sd2AgeAtCollection as aoc,
	   a.ansvalueplain as result,
	   va.ResultCode as result_code
	   from (select s.specimenid, a.testid, max(answerix) as answerindex
	   from Answer a inner join specimen s on s.SpecimenID = a.SpecimenID
	   where a.TestId = 179 
	   and a.ansStatus = 110
	   and s.spcextcode1 like '[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]'
	   and substring(s.spcextcode1,1,8) between '20140000' and '20200000'
	   and substring(s.spcextcode1,9,1) not in ('4', '7', '8')
	   group by s.specimenid, a.TestId) a1
	   inner join answer a on a1.SpecimenID = a.SpecimenID and a1.AnswerIndex = a.AnswerIX and a1.TestId = a.TestId
	   inner join specimen s on a1.specimenid = s.specimenid
	   inner join vw_Answers va on s.spcExtcode1 = va.AccessionNumber and a.TestId = va.TestID
	   inner join specimendetail2 sd on sd.SpecimenId = va.SpecimenID
	   order by s.spcextcode1"

## biotdata <- with_con(biotquery)
## write.csv(biotdata, file= paste0("./data/biot_data_", today, ".csv"))
biotdata <- read.csv("./data/biot_data_2021-12-07.csv", stringsAsFactors = FALSE)
biotdata$measured_time  <- ymd_hms(biotdata$measured_time)
biotdata <- na.omit(biotdata)


galtquery <- "select s.spcextcode1 as accession,
	   a.ansTimeMeasured as measured_time,
	   s.spcExtcode2 as form,
	   sd.sd2GestationAge as ga,
	   sd.sd2Weight as bw,
	   sd.sd2AgeAtCollection as aoc,
	   a.ansvalueplain as result,
	   va.ResultCode as result_code
	   from (select s.specimenid, a.testid, max(answerix) as answerindex
	   from Answer a inner join specimen s on s.SpecimenID = a.SpecimenID
	   where a.TestId = 13 
	   and a.ansStatus = 110
	   and s.spcextcode1 like '[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]'
	   and substring(s.spcextcode1,1,8) between '20140000' and '20200000'
	   and substring(s.spcextcode1,9,1) not in ('4', '7', '8')
	   group by s.specimenid, a.TestId) a1
	   inner join answer a on a1.SpecimenID = a.SpecimenID and a1.AnswerIndex = a.AnswerIX and a1.TestId = a.TestId
	   inner join specimen s on a1.specimenid = s.specimenid
	   inner join vw_Answers va on s.spcExtcode1 = va.AccessionNumber and a.TestId = va.TestID
	   inner join specimendetail2 sd on sd.SpecimenId = va.SpecimenID
	   order by s.spcextcode1"

## galtdata <- with_con(galtquery)
## write.csv(galtdata, file= paste0("./data/galt_data_", today, ".csv"))
galtdata <- read.csv("./data/galt_data_2021-12-08.csv", stringsAsFactors = FALSE)
galtdata$measured_time  <- ymd_hms(galtdata$measured_time)
galtdata <- na.omit(galtdata)

biotdata %>%
    na.omit() %>%
    group_by(year = year(measured_time)) %>%
    summarise(n = n(),
	      median = median(result, na.rm = TRUE),
	      pos = length(result_code[result_code == "BIOT-C-01-012"]),
	      rate = pos/n) %>%
    xtable(caption = "Yearly Biotinidase Screen Positive Rate",
	   label = "tab:biot_year", display = c("d", "d", "d", "f", "d", "g")) %>%
    print(include.rownames = FALSE)

galtdata %>%
    na.omit() %>%
    group_by(year = year(measured_time)) %>%
    summarise(n = n(),
              median = median(result, na.rm = TRUE),
              pos = length(result_code[result_code == "GALT-C-01-012"]),
              rate = pos/n) %>%
    xtable(caption = "Yearly Galactosemia Screen Positive Rate",
           label = "tab:galt_year", display = c("d", "d", "d", "f", "d", "g")) %>%
    print(include.rownames = FALSE)

weather <- read.csv("./data/weather/temp.csv", header = FALSE)
dates <- as.Date(c("2017-01-01","2017-08-31"))
labels <- c("","")
events <- xts(labels, dates)

  #weathertemp <- weather[-c(1,2),] # remove first two days to align with biotts
biotweek <- biotdata %>%
	group_by(week = date(floor_date(measured_time, unit = "week"))) %>%
	summarise(n = n(),
		  median = median(result, na.rm = TRUE),
		  mean = median(result, na.rm = TRUE),
		  pos = length(result_code[result_code == "BIOT-C-01-012"])) %>%
	filter(week >= "2014-01-12") ## start of Spotcheck Pro results

tempweek <- weather %>%
  select(date= 5, dailymean = 14) %>%
  group_by(week = floor_date(date(date), unit = "week")) %>%
  filter(week >= "2014-01-12" & week <= biotweek$week[length(biotweek$week)]) %>%
  summarise(temp = mean(dailymean, na.rm = TRUE)) %>%
  na.omit()

biotcombined <- cbind(biotweek$pos, biotweek$median, tempweek$temp)
biotcombinedts <- xts(biotcombined, biotweek$week)
plot(biotcombinedts, col = c("firebrick","steelblue4", "gold2"),
       multi.panel = TRUE, 
       yaxis.same = FALSE, 
       yaxis.right = FALSE,
       main = "")
addEventLines(events, srt = 90, pos = 2, on = 1, col = "blue")
addLegend(legend.loc = "topright",
	  legend.names = c("screen positive", "median activity", "temperature (C)"),
	  col = c("firebrick","steelblue4", "gold2"),
	  lty = c("solid","solid", "solid"),
	  on = 1)

galtweek <- galtdata %>%
  group_by(week = date(floor_date(measured_time, unit = "week"))) %>%
    summarise(n = n(),
	      median = median(result, na.rm = TRUE),
	      mean = median(result, na.rm = TRUE),
	      pos = length(result_code[result_code == "GALT-C-01-012"])) %>%
    filter(week >= "2014-01-12") ## start of Spotcheck Pro results

  galtcombined <- cbind(galtweek$pos, galtweek$median, tempweek$temp)
  galtcombinedts <- xts(galtcombined, galtweek$week)
  plot(galtcombinedts, col = c("firebrick","steelblue4", "gold2"),
       multi.panel = TRUE, 
       yaxis.same = FALSE, 
       yaxis.right = FALSE,
       main = "")
addEventLines(events, srt = 90, pos = 2, on = 1, col = "blue")
addLegend(legend.loc = "topright",
	  legend.names = c("screen positive", "median activity", "temperature (C)"),
	  col = c("firebrick","steelblue4", "gold2"),
	  lty = c("solid","solid", "solid"),
	  on = 1)

## Decompose
ts_biotmedian <- ts(biotweek$median, frequency = 52)
d <- decompose(ts_biotmedian, "additive")
#und <- unclass(d)
biotdecomp <- cbind(biotweek$median, unclass(d$trend), unclass(d$seasonal), unclass(d$random))

biotdts <- xts(biotdecomp, biotweek$week)
plot(biotdts, col = c("steelblue4", "firebrick", "gold2", "grey50"),
     yaxis.right = FALSE,
     main = "")
addLegend(legend.loc = "right",
	  legend.names = c("median", "trend", "seasonal", "random"),
	  col = c("steelblue4", "firebrick", "gold2", "grey50"),
	  lty = c("solid","solid", "solid", "solid"),
	  on = 1)

cor(d$seasonal,d$x, use = "pairwise.complete.obs")
cor(d$seasonal,d$trend, use = "pairwise.complete.obs")

biotdata$form_letter <- gsub("([[:upper:]]{1})([[:digit:]]{6})", "\\1", biotdata$form, perl = TRUE)
biotdata$form_number <- as.numeric(gsub("([[:upper:]]{1})([[:digit:]]{6})", "\\2", biotdata$form, perl = TRUE))
biotdata$lot <- unlist(mapply(form2lot, biotdata$form_letter, biotdata$form_number, SIMPLIFY = TRUE, USE.NAMES = FALSE))

formweekm <- biotdata %>%
    group_by(week = date(floor_date(measured_time, unit = "week")), lot) %>%
    summarise(n = n(),
	      mean = mean(result, na.rm = TRUE),
	      median = median(result, na.rm = TRUE)) %>%
    filter(n > 50 & week >= "2014-01-12")  %>%  ## start of Spotcheck Pro results
    spread(key = lot, value = mean) %>%
    group_by(week) %>%
    summarise_all(funs(na.omit(.)[1])) 
lots <- c("B6978414","B7012515", "W152", "W161","B7078017","B7087317", "B7101418")
formslide <- xts(formweekm[, lots], formweekm$week)

plot(formslide, col = c("black","red", "steelblue4", "gold2", "darkorchid" , "darkorange", "cyan4"),
     multi.panel = FALSE,
     yaxis.right = FALSE,
     main = "",
     ylab = "Biotinidase")

					#July 2014 B6978414 W121
					#July 2015 B7012515 W141
					#July 2016 W152
					#Jan 2017 161 
					#July 2017 W162
					#Junw 2018 7115218 W171

addLegend(legend.loc = "bottomright", 
	  legend.names = c("w121", "w141", "w152", "w161", "w162", "w162b", "w171"),
	  col = c("black","red", "steelblue4", "gold2", "darkorchid" , "darkorange", "cyan4"),
	  lty = c("solid", "solid", "solid", "solid", "solid", "solid", "solid"),
	  on = 1)
