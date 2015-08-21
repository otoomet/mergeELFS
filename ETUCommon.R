
convertEdu <- function(voc, general, genVoc, name) {
   ## We use coding 1 = <= basic; 2 = high school; 3 = college
   year <- substr(name, 1, 4)
   edu <- rep("<=basic", max(length(voc), length(general)))
                           # 1995 wave does not have voc variable (?)
   if(year == "1997") {
      edu[general == 4] <- "highSchool"
      edu[voc %in% 3:4] <- "highSchool"
      edu[voc %in% 5:7] <- "college"
   }
   else if((year %in% c("1999")) | name %in% c("2000_1", "2000_2")) {
      edu <- ifelse(voc %in% c(8:11), "college",
                    ifelse(voc %in% c(4:7), "highSchool",
                           ifelse(general %in% c(1), "highSchool",
                                  ifelse(voc %in% c(1:3), "<=basic",
                                         ifelse(general %in% c(2:5), "<=basic",
                                                ifelse(is.na(voc), NA, NA))))))
   }
   else if(year == "1995") {
      edu[general %in% c(3,4)] <- "highSchool"
      edu[general %in% c(5,6,7)] <- "college"
   }
   else if(year == "1998") {
      edu[general == 4] <- "highSchool"
      edu[voc %in% 4:7] <- "highSchool"
      edu[voc %in% 8:10] <- "college"
   }
   else if(year == "2000") {
      edu[voc %in% c(9, 10, 11)] <- "college"
      edu[voc %in% 4:8] <- "highSchool"
      edu[edu == "<=basic" & general == 1] <- "highSchool"
   }
   else if(year <= "2007") {
      edu[general == 1] <- "highSchool"
      edu[voc %in% 10:13] <- "college"
      edu[voc %in% 4:9] <- "highSchool"
   }
   else if(year == "2008") {
      edu[general == 1] <- "highSchool"
      edu[voc %in% c(3, 4)] <- "highSchool"
      edu[genVoc == 3] <- "highSchool"
      edu[voc %in% c(5, 6)] <- "college"
   }
   else {
      edu[genVoc %in% 3:5] <- "college"
      edu[genVoc %in% 1:2] <- "highSchool"
      edu[general == 1] <- "highSchool"
      edu[voc %in% 3:4] <- "highSchool"
      edu[voc %in% 5:6] <- "college"
   }
   return(edu)
}

convertEmployeeStatus <- function(stat, etuName) {
   newStat <- stat
   if(etuName <= "1997") {
      newStat[stat == 3] <- 10
      newStat[stat == 5] <- 6
      newStat[stat == 4] <- 7
      newStat[stat == 6] <- 11
   }
   if(etuName >= "2003") {
      newStat[stat == 9] <- 11
      newStat[stat == 8] <- 11
   }
   newStat
}

convertISCED97 <- function(voc, general, genVoc, ISCED=NULL, name) {
   ## ISCED 97 codes
   ## ISCED: can supply it, then takes it directly
   if(!is.null(ISCED)) {
      isced <- ISCED
      if(is.character(ISCED) | (is.factor(ISCED) & length(levels(ISCED)) > 8)) {
         # retain only 1 digit
         isced <- as.numeric(substr(ISCED, 1, 1))
      }
      isced[isced == 9] <- NA
      return(isced)
   }
   year <- substr(name, 1, 4)
   isced <- integer(length(general))
   isced[] <- NA
   if(year == "1995") {
                           # general: g21, higher graduated level
      isced[general %in% 1] <- 1
                           # less than basic -> 1-6 years
      isced[general %in% 2] <- 2
                           # basic -> basic
      isced[general %in% 3] <- 3
                           # secondary -> secondary
      isced[general %in% 4] <- 4
                           # specialized secondary -> vocational + secondary
      isced[general %in% 5] <- 5
      isced[general %in% 6] <- 5
      isced[general %in% 7] <- 5
                           # degree (probably MA) -> higher edu
   }
   else if(year == "1997") {
                           # general: g28, vocational: g30
      ## What is your highest level of completed education?
      ## CHART G28
      ## 1 No primary education
      ## 3 Basic education -> SKIP TO G30
      ## 2 Primary education -> SKIP TO H01
      ## 4 Secondary education -> SKIP TO G30
      ## 
      ## What is your highest level of completed vocational, specialised and professional education?
      ## CHART G30
      ## 1 No vocational, specialised or professional education
      ## 2 Vocational / technical education
      ## 3 Specialised secondary / technical education
      ## 4 Applicational higher education / diploma studies since 1995
      ## 5 Diploma of higher education until 1995 / bachelor's degree
      ## 6 Master's degree
      ## 7 Doctorate / candidate of sciences
      isced[general == 1] <- 0
      isced[general == 2] <- 1
      isced[general == 3] <- 2
      isced[(general == 4) & (voc == 1)] <- 3
      isced[(voc == 2)] <- 3
      isced[(general == 4) & (voc %in% c(2))] <- 4
      isced[(general == 4) & (voc %in% c(3))] <- 5
      isced[(voc %in% 4:6)] <- 5
      isced[(voc == 7)] <- 6
      isced[(general %in% 1:3) & (voc == 3)] <- 3
   }
   ## 1998-2010 we have isced directly in data
   else if(year > "2010"){
      genVoc[is.na(genVoc)] <- 0
      isced[general %in% c(4,5)] <- 0
      isced[general == 3] <- 1
      isced[general == 2] <- 2
      isced[(general == 2) & (voc %in% c(2,3))] <- 3
      isced[(general == 2) & (voc > 3)] <- 3
      isced[(general == 1)] <- 3
      isced[(general == 1) & (voc == 4)] <- 4
      isced[(genVoc > 0)] <- 5
      isced[(general == 2) & (voc == 5)] <- 5
      isced[(voc == 6)] <- 6
   }
   return(isced)
}

convertMaritalStatus <- function(status, etuName, separated, cohabits, trueMarital) {
   if((etuName >= "2008_1") & (etuName < "2009")) {
      newStat <- status
      newStat[(status == 1) & (trueMarital == 1)] <- 1
                           # single
      newStat[(status == 1) & (trueMarital == 2)] <- 2
                           # co-habiting
      newStat[(status == 2) & (trueMarital == 2)] <- 3
                           # married
      newStat[(status == 2) & (trueMarital == 4)] <- 6
                           # separated
      newStat[status == 3] <- 5
                           # divorced
      newStat[(status == 3) & (trueMarital == 2)] <- 2
                           # divorced but cohabiting
      return(newStat)
   }
   else if(etuName >= "2009_1") {
      newStat <- status
      newStat[(status == 1) & (cohabits == 1)] <- 2
                           # cohabiting
      newStat[(status == 2) & (separated == 1)] <- 3
                           # married
      newStat[(status == 2) & (separated != 1)] <- 6
                           # separated
      newStat[status == 3] <- 5
                           # divorced
      return(newStat)
   }
   return(status)
}

convertNWorkers95 <- function(n) {
   nn <- integer(length(n))
   dim(nn) <- dim(n)
   nn[n %in% 1:3] <- 1
   nn[which(n > 3)] <- n[which(n > 3)] - 2
   nn[is.na(n)] <- NA
   nn[which(n > 95)] <- NA
   nn
}

### convertOccupation: convert occupation to a 1-digit form
convertOccupation <- function(o) {
   d <- dim(o)
   p <- as.numeric(substr(trim(as.character(o)), 1, 1))
   dim(p) <- d
   p
}

convertOwnership95 <- function(type, domestic) {
   ## convert the 1995 type of ownership status
   ## type: ownership type (c07)
   ## domesting: domestic/foreign (c09)
   own <- numeric(length(type))
   own[type %in% c(1,3,4,5,6)] <- 10
   own[type %in% c(2)] <- 20
   own[type %in% c(9,14)] <- 90
                           # non-profit
   own[own == 0 & domestic == 1] <- 30
   own[own == 0 & domestic == 2] <- 50
   own[own == 0 & domestic == 3] <- 40
   own[is.na(type) | is.na(domestic)] <- NA
   own
}

isTown<-function(vald){
   ## Distinguish between town/countryside based on community code
## This is necessary as ETU1995 lacks town/countryside information
## NB - the function is not very reliable - visually there appears to be quite a lot of cases
## where the code
## is not what it should be when looking at the name of the county in the data!
   townOrCounty <- rep(NA, length(vald))
   townOrCounty[!is.na(vald)] <- 2
   townOrCounty[vald %in% c(107, 144, 170, 183, 249, 253, 279, 287, 290, 296, 306, 309, 322, 345, 349,
                            371, 412, 424, 446, 485, 490, 511, 513, 556, 566, 580, 617, 620, 625, 645,
                            663, 670, 705, 728, 735, 741, 760, 784, 788, 791, 795, 823, 837, 854, 897,
                            912, 919)] <- 1
   townOrCounty[vald<100] <- 1
   dim(townOrCounty) <- dim(vald)
   townOrCounty
}

county <- function(vald) {
   if(is.factor(vald)) {
      vald <- as.integer(levels(vald)[vald])
   }
    region <- rep(NA, length(vald))
    region[is.na(vald)] <- NA
    region[vald == 999] <- NA
    ## Tallinn
    region[vald==784]<- 37
    ## Harjumaa without Tallinn
   region[vald %in% c(1,2,37,112,140,198,245,290,295,296,297, 298,
                      # Kesklinna linnaosa
                      304,337,352,363,423,
                      424,446,518,562,580,651,653,718,727,728,868,890,894)] <- 37
   ## Hiiumaa
   region[vald %in% c(39, 175,368,371,392,639)] <- 39
   ## Ida-Viru
   region[vald %in% c(13,15,21,44,120,122,154,164,224,229,252,253,309,320,322,323,420,
                      437,449,498,511,513,551,614,645,735,743, 747,
                      # Sompa linnaosa
                      749,751,802,815,851)] <- 44
   ## Jõgevamaa
   region[vald %in% c(22,49,248,249,485,573,576,578,611,616,617,657,713,773,810)] <- 49
   ## Järvamaa
   region[vald %in% c(26,51,129,134,135,234,257,258,271,288,314,325,
                      400,537,565,566,684,836,837,937)] <- 51
   region[vald %in% c(57, 183,195,342,411,412,452,520,531,552,674,680,776,907)] <- 57
   region[vald %in% c(54,59,161,190,268,273,345,381,660,662,663,702,716,738,
                      770,787,788,791,887,900,902,922,927)] <- 59
   ## Lääne-Virumaa
   region[vald %in% c(65,117,124,285,354,385,465,473,547,619,620,705,707,
                      856,872,879,934)] <- 65
   ## Põlvamaa
   region[vald %in% c(18,67,149,159,188,213,276,303,306,334,395,568,625,627,
                      711,730,741,756,782,805,808,826,848,863,930,931)] <- 67
   ## Pärnumaa
   region[vald %in% c(70,240,260,277,292,317,318,375,427,504,505,654,669,670,884)] <- 70
   ## Raplamaa
   region[vald %in% c(10,74,270,301,348,349,373,386,403,440,478,483,550,592,
                      634,689,721,807,858)] <- 74
   ## Saaremaa
   region[vald %in% c(24,25,78,126,170,185,279,282,331,383,432,454,457,501,
                      528,587,595,605,666,694,794,795,831,861,915,949)] <- 78
   ## Tartumaa
   region[vald %in% c(82,203,208,289,555,556,582,608,613,636,724,779,820,823,
                      854,943)] <- 82
   ## Valgamaa
   region[vald %in% c(84,105,107,192,287,328,357,360,490,523,545,570,600,629,715,
                      759,760,797,870,892,897,912)] <- 84
   ## Mulgimaa
   region[vald %in% c(86,143,144,181,389,460,468,493,697,767,843,865,874,918,919)] <- 86
   ## Võromaa
   region[vald == 0] <- NA
                                        # unknown
   region
}

getOwners<-function(owners1, ownercapital){
   owners<-rep(NA, length(owners1))
   owners[owners1%in%c(7,8,10,11,12,13)]<-30
   owners[owners1%in%c(7,8,19)&ownercapital==2]<-50
   owners[owners1%in%c(7,8,19)&ownercapital==3]<-40
   owners[owners1%in%c(1,3)]<-10
   owners[owners1%in%c(2)]<-20
   owners[owners1%in%c(4,5,6,9)]<-90
   return(owners)
}

normLanguage <- function(lang) {
   ## transforms languages to a factor where levels are in order of skills
   ## and missing obs are coded as "".
   lang[!(lang %in% c("1", "2", "3", "home"))] <- NA
   lang[is.na(lang)] <- ""
   factor(lang, levels=c("", "3", "2", "1", "home"))
}
   
whereAtTime<-function(dateToCompare, fromD, untilD) {
   ## This function takes three arguments: dates of interest, start dates and end dates.
   ## Returns matrix of logicals: to which start/end interval the dates of interest fall to
   isDateInside <- (fromD < dateToCompare) & ((is.na(untilD)) | (untilD > dateToCompare))
   isDateInside
}

moveData <- function(date, data, time) {
   ## pick the correct column of data, given movements at time.
   ## ncol(data) must be equal to ncol(time) + 1,
   ## first columns of data is the 'initial' state, and every time
   ## is a move to next column of data
   d <- data[,1]
   for(c in seq(ncol(time))) {
      i <- (time[,c] < date) & !is.na(time[,c])
      d[i] <- data[i, c + 1]
   }
   d
}

retrospectiveEducation <- function(date, edu, endDate, endStatus, schoolType, preSchoolType) {
   ## find education retrospectively based on current edu, dates and types of graduated schools
   ## schoolType:     school level
   ## preSchoolType:  assumed edu level before attending this school
   graduated <- (endDate <= date) & (endStatus == 1)
   notGraduated <- (endDate > date) & (endStatus == 1)
   preSchools <- ngSchools <- gSchools <- matrix(0, nrow(schoolType), ncol(schoolType))
   gSchools[!is.na(graduated) & graduated] <- schoolType[!is.na(graduated) & graduated]
   ngSchools[!is.na(notGraduated) & notGraduated] <-
       schoolType[!is.na(notGraduated) & notGraduated]
   preSchools[!is.na(notGraduated) & notGraduated] <-
       preSchoolType[!is.na(notGraduated) & notGraduated]
   levels <- 1:3
   names(levels) <- c("<=basic", "highSchool", "college")
                           # we have to convert from char to numeric in order to compare the levels
                           # Alternative would be to use a special class w/operateor overloading
   maxG <- levels[apply(gSchools, 1, max)]
   maxNG <- levels[apply(ngSchools, 1, max)]
   minNG <- levels[apply(preSchools, 1, min)]
   rEdu <- levels[edu]
   i <- !is.na(maxNG) & !is.na(edu) & (edu == maxNG)
                           # their highest edu = one of the not yet graduated schools
   rEdu[i] <- pmax(maxG[i], minNG[i])
                           # their retro edu = either one lower than not graduated, or highest graduated
   rEdu <- c("<=basic", "highSchool", "college")[rEdu]
   rEdu
}

## the same as previous but returns if has any value departing from NA at this time
anythingAtTime<-function(dateToCompare, fromD, untilD,convertDatesToMatrixes=FALSE){
       if(convertDatesToMatrixes) {
                fromD<-matrix(fromD, ncol=ncol(dateToCompare), nrow=nrow(dateToCompare))
                untilD<-matrix(untilD, ncol=ncol(dateToCompare), nrow=nrow(dateToCompare))
       }
       #NB - this converts fromD and untilD to matrixes equal in size to dateToCompare
       # It is only needed, when dateToCompare is a matrix!  In some cases we get fromD 
       # and untilD as matrixes and dateToCompare as single value, in which case everything
       # works without the conversion
       isDateInside <- (fromD < dateToCompare) & ((untilD > dateToCompare) | (is.na(untilD)))
       apply(isDateInside, 1, function(x) sum(x, na.rm=TRUE))
}

convertSidejob <- function(sidejob, retrospective) {
   ## homogenize sidejob information.  Input:
   ## retrospective: 1 - yes; 2 - from time to time; 3 - no
   ## survey week:   1 - yes; 2 - no
   ## return coded to as for retrospective
   ## all the other codes <- NA
   sc <- sidejob[!retrospective]
   sc[!(sc %in% c(1,2))] <- NA
   sc[sc == 2] <- 3
   sr <- sidejob[retrospective]
   sr[!(sr %in% c(1,2,3))] <- NA
   s <- numeric(length(sidejob))
   s[retrospective] <- sr
   s[!retrospective] <- sc
   s
}
 
getYearOfFirstHigherEduc<-function(eduQualificationYear, eduQualificationTypeOfSchool, higherSchools) {
   minYear <- function(y) {
      # return minimum year (excluding NA-s), or NA if no obs
      if(length(y) == 0 | all(is.na(y)))
          return(NA)
      return(min(y, na.rm=TRUE))
   }
   eduQualificationYear[!(eduQualificationTypeOfSchool %in% higherSchools)] <- NA
   minYearOfHigherEdu <- apply(eduQualificationYear,1, minYear)
   return(minYearOfHigherEdu)
}

month <- function(x) {
   as.numeric(format.POSIXct(x, "%m"))
}

municipality2008 <- function(m) {
   ## 2008 standard administrative codes by Statistics Estonia
   ## http://metaweb.stat.ee/view_xml_linear.htm
   mm <- m
   mm[m == 1] <- 784
   mm[m == 13] <- 322
                                        # Kohtla-Järve
   mm[m == 124] <- 707
                                        # Alaküla renamed to Räpina 1991
   mm[m == 18] <- 625
                                        # Pärnu
   mm[m == 24] <- 795 # Tartu linn
   mm[m == 15] <- 511 # Narva
   mm[m == 21] <- 735 # Sillamäe
   mm[m == 107] <- 105 # Abja-Paluoja -> Abja vald
   mm[m == 120] <- 322 # Ahtme -> Kohtla-Järve
   mm[m == 176] <- 784 # Haabersti
   mm[m == 249] <- 248 # Jõgeva linn -> vald
   mm[m %in% c(252, 253)] <- 251 # Jõhvi linn, vald -> vald
   mm[m == 265] <- 322 # Järve -> Kohtla-Järve
   mm[m == 273] <- 272 # Kadrina
   mm[m == 287] <- 600 # Karksi-Nuia -> Karksi vald
   mm[m == 290] <- 290 # Kehra -> Anija
   mm[m == 296] <- 295 # Keila -> vald
   mm[m == 298] <- 784 # Kesklinn -> Tallinn
   mm[m == 318] <- 317 # Kohila -> vald
   mm[m == 339] <- 784 # Kristiine -> Tallinn
   mm[m == 352] <- 353 # Kuusalu
   mm[m == 387] <- 784 # Lasnamäe -> Tallinn
   mm[m == 482] <- 784 # Mustamäe -> Tallinn
   mm[m == 423] <- 424 # Loksa
   mm[m == 524] <- 784 # Nõmme -> Tallinn
   mm[m == 553] <- 322 # Oru linnaosa -> Kohtla-Järve
   mm[m == 596] <- 784 # Pirita -> Tallinn
   mm[m == 614] <- 784 # Põhja-Tallinn -> Tallinn
   mm[m == 670] <- 669 # Rapla linn -> vald
   mm[m == 705] <- 707 # Räpina vald
   mm[m == 711] <- 710 # Saarde vald
   mm[m == 716] <- 790 # Saksi vald -> Tapa vald.  Note: some of the villages joined Kadrina vald instead
   mm[m %in% c(759, 760)] <- 758 # Suure-Jaani
   mm[m %in% c(787, 788)] <- 786 # Tamsalu
   mm[m == 791] <- 790 # Tapa linn -> vald
   mm[m %in% c(836, 837)] <- 835 # Türi linn -> vald
   mm[m == 893] <- 322 # Viivikonna -> Kohtla-Järve
   mm[m == 927] <- 926 # Väike-Maarja
   mm[m %in% c(0, 9, 10, 37, 39, 44, 49, 51, 57, 65, 67, 70, 74, 78, 82, 84, 86, 100)] <- NA
   mm
}
