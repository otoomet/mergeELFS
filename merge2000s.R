### Functions here take data from ETU 2000 Q3 -- 2008 (all quarters)
### and save it either to file 'dataOf2000q3-2008All.Rdat' for
### standalone use or return it as a data.frame
### (function doAll2000q3.2008) for use in other scripts.
###
mergePost2000 <- function(nrows=25,
                           print.level=3) {
   ## nrows        how many rows to read from the tables (-1 = all).  Useful for testing
   ## print.level  how much tracing info to print (higher number = more)
   if(print.level > 0) {
      cat("ETU2000_Q3 .. 2013\n")
   }
   source("ETUCommon.R")
   saveIndividualDataframes<-TRUE
source("ETUCommon.R")
source("Parameters.R")
source("etuUtils.R")
   ##
##
##       The parameters starting with "ES_" form a compatability layer 
##       for different ETUs. "ES_P" means that last two digits are substracted,
##       for example C04A0000->C04A00 
##
##
ES_addToYear<-0  #some ETUs have years as YYYY, some YY, in case of YY set this to 1900
ES_male<-"k01d"
   ES_residenceMuni <- "K03LKG"
   ES_unionMember <- "k15"
   ## other jobs
   ES_sidejob <- "e01"
                                        # 1 = yes, 2 = no
   ## language
LFSLang1<-"code1"     
LFSLang2<-"code2"
LFSLang3<-"code3"
LFSLang4<-"code4"
LFSLang5<-"code5"
LFSLangLevel1<-"level1"
LFSLangLevel2<-"level2"
LFSLangLevel3<-"level3"
LFSLangLevel4<-"level4"
LFSLangLevel5<-"level5"
   ##
ES_quarter<-"quarter"
ES_wave<-"wave"
ES_workForceStatus<-"status"
   ## Inner loop for all individual waves:
   doEtu<-function(etuName) {
      cat(etuYQ <- paste("ETU", etuName, sep=""))
      surveyYear <- as.numeric(substr(etuName, 1, 4))
      surveyQuarter <- substring(etuName, 6)
      if(surveyQuarter == "1-3")
                           # 2009 data is coded in this way
          surveyQuarter <- 2
      else if(surveyQuarter == "1-4")
          surveyQuarter <- 2
      surveyQuarter <- as.numeric(surveyQuarter)
      ## Variables
      ## The following variables represent the _end of survey week_.  They may or may not coincide with actualy week the
      ## questionnary was filled.
      ES_SWDay<-"dayi"
      ES_SWMonth<-"monthi"
      ES_SWYear <- "year"
      ##
      ES_HHno <- if(etuName >= "2009") "leibkond" else "houshold"
      ES_respondent<- if(etuName >= "2009") "kysitletav" else "member"
      ES_memberi <- switch(substr(etuName, 1, 4),
                           "2000" = "member",
                           "2011"=, "2012"=, "2013" = "kysitletav",
                           "memberi")
                           # the member id during the first wave
      ##
      ES_yearOfBirth<-"k01c"
      ES_P_ethnicity <- ES_ethnicity <- switch(substr(etuName, 1, 4),
                                               "2009"=, "2010"=,
                                               "2011"=, "2012"=, "2013" = "yc1",
                                               "b08")
      ES_immigrYear<- switch(substr(etuName, 1, 4),
                             "2009" =, "2010" =, "2011" =,
                             "2012"=, "2013" = "yc4a",
                             "b07a")
      ## Main job stuff
      ES_howFoundJob <- "d11"
                           # how did you find this job
      ES_firstJobYear <- switch(substr(etuName, 1, 4),
                                "2000" =, "2001" = "",
                                "i00a")
      ES_firstJobMonth <- switch(substr(etuName, 1, 4),
                                "2000" =, "2001" = "",
                                "i00b")
      ES_C_jobStartYear <-"d09a"
      ES_C_jobStartMonth <-"d09b"
      ES_C_employeeStatus <- "d05"
      ES_wageCurrent<-"d25b"
      ES_C_occupation<-switch(substr(etuName, 1, 4),
                                "2011"=, "2012"=, "2013" = "d03ku",
                                "d03k")
      ES_C_partOrFullTime<-"d17"
      ES_C_partTimeHours<-"d19"
      ES_C_industry<- switch(substr(etuName, 1, 4),
                             "2011"=, "2012"=, "2013" = "d02au",
                             "d02a")
                           # simply "industry"
      ES_C_industryEstablishment <- switch(substr(etuName, 1, 4),
                                           "2011" = "d02bu",
                                           "")
                           # industry for the particular establishment
      ES_C_nWorkers<-"d07a"
      ES_C_publicSector<-"d13"
      ES_C_cityOfWork<-"d08lk"               #NB - after transformation, in original data L and R go from 9 to 14 and M from 0 to 5...
      ES_C_workMuni <- "D08LKG"
      ES_C_workCountry<-"d08rk"          #NB - after transformation 
      ES_Current<-"d25b"
      ES_C_ownership<-"d12"
      ## Household
      ## Since 2009, this data is in a different file (ETU2009_XXmembers).  ES_P_xx variables
      ## are for extracting the corresponding members from the main data, ES_xx from the separate member file
      separateMembers <- etuName >= "2009"
      ES_columnsOfHHMembers<- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10","11")
      ES_P_HHBirthday_Year <- "b05c"
      ES_HHBirthday_Year <- switch(substr(etuName, 1, 4),
                                   "2012"="ya2.a",
                                   "ya2_a")
      ES_HHBirthday_Month <- switch(substr(etuName, 1, 4),
                                    "2012"="ya2.k",
                                    "ya2_k")
      ##
      ## Education
      ES_currentlyStudying = "j01"
                           # 1 = yes, 2 = no
      ES_vocationalEdu <- switch(substr(etuName, 1, 4),
                                 "2008" = "j14c",
                                 "2009" =, "2010" =, "2011" =,
                                 "2012" =, "2013" = "ye3",
                                 "j13a")
                           # highest completed level of vocational edu (including scientific degree)
      ES_generalEdu <- switch(substr(etuName, 1, 4),
                              "2009" =, "2010" =, "2011" =,
                              "2012" =, "2013" = "ye1",
                              "j14a")
                           # highes level of general edu: 1: secondary; 2-5:  < secondary
      ES_generalVocationalEdu <- switch(substr(etuName, 1, 4),
                                        "2008" = "j14e",
                                        "2009" =, "2010" =, "2011" =,
                                        "2012" =, "2013" = "ye4", 
                                        "")
                           # received general degree together with vocational.  Present in 2008.
      LFSISCED <- switch(substr(etuName, 1, 4),
                         "2009"=, "2010" = "isced97",
                         "isced")
                           # some databases have isced already entered
      ## Residence
      if(surveyYear %in% 2000:2007) {
         ES_residenceCounty <- "k03mk"
         ES_C_workCounty<-"d08mk"
         ES_activityStatus<-"k08"
      }
      else {
         ES_residenceCounty <- "k03mkkoo"
         ES_C_workCounty<-"d08mkkoo"
         ES_activityStatus<-"k08a"
      }
      ## Family
      ES_maritalStatus <- switch(substr(etuName, 1, 4),
                                 "2008" = "b10a",
                                 "2009" =, "2010" =, "2011" =,
                                 "2012" =, "2013" = "ya3",
                                 "k02")
      ES_separated <- switch(substr(etuName, 1, 4),
                             "2008" = "b10b",
                             "2009" =, "2010" =, "2011" =,
                             "2012" =, "2013" = "ya4",
                             "")
      ES_cohabits <-  switch(substr(etuName, 1, 4),
                             "2008" = "b10c",
                             "2009" =, "2010" =, "2011" =,
                             "2012" =, "2013" = "ya5",
                             "")
      ES_trueMarital <- switch(substr(etuName, 1, 4),
                               "2008" = "marital",
                               "")
      ES_homeLang1<- switch(substr(etuName, 1, 4),
                                      "2009" =, "2010" = "k05akk",
                                      "k05ak")
      ES_homeLang2 <- switch(substr(etuName, 1, 4),
                                      "2009" =, "2010" = "k05bkk",
                                      "k05bk")
      ES_homeLang3 <- switch(substr(etuName, 1, 4),
                                      "2009" =, "2010" = "k05ckk",
                                      "k05ck")
      ES_C_partTimeReason <- switch(substr(etuName, 1, 4),
                                    "2000" =, "2001" =, "2002" =, "2003" =,
                                    "2005" = "d18",
                                    "d18a")
      ES_howFoundJob <- if(surveyYear < 2006) "d11" else "d11a"
      ## unemployment
      ES_jobseekStartY <- "h05a"
      ES_jobseekStartM <- "h05b"
      ES_UBEligibility <- "h21"
      ES_UIBenefits <- if(etuName >= "2003_1") "h22aa" else ""
      ES_UAllowance <- "h22a"
      ES_howSearchJob <- "h13v"
      ES_mainSearchMethod <- "h14"
      ES_reservationWage <- if(etuName >= "2008_1") "h11a" else ""
                           # reservation wage, in money
      ES_reservationWageLevels <- if(etuName >= "2008_1") "h11b" else "h11"
      ES_searchParttime <- "h09"
      ## Inactivity
      ES_prevFirmIndustry <- "g05a"
      ES_prevEstablishIndustry <- switch(substr(etuName, 1, 4),
                                         "2011"=, "2012"=, "2013" = "g05bu",
                                         "g05b")
      ES_prevOccupation <- switch(substr(etuName, 1, 4),
                                         "2011"=, "2012"=, "2013" = "g06ku",
                                         "g06k")
      ## technical stuff
      ES_weight <-
          if(surveyYear == 2000) {
             "wgt00new"
          }
          else if(surveyYear < 2013) {
             paste("wgt", substr(etuName,3,4),sep="")
          }
      else
         "wgt_q"
                           # quarterly weight (wgt_y - yearly)

      ##
      defaultSurveyDate <- ISOdate(surveyYear,
                                   3*(surveyQuarter - 1) + 2, 15)
                           # there are NA-s in dates
      ##
      ## We stop the ASCII step, needed for low-memory machines.  Man, it is 2010 ;-)
      load(paste("../", etuYQ, ".Rdat", sep=""))
                           # main dataset.  Dataframe called 'etu'
      m <- etu
      names(m) <- tolower(names(m))
      rm(etu)
      ## m <- eraldaMuutujad(etu, vars, nrows)
      if(print.level > 0) {
         cat("\n", nrow(m), "rows read")
      }
      dateCurrent <- ISOdate(m[[ES_SWYear]], m[[ES_SWMonth]], m[[ES_SWDay]])
      dateCurrent[is.na(dateCurrent)] <- defaultSurveyDate
      if(separateMembers) {
                           # separate file for HH members.  Will be
                           # loaded and called 'members'
         memberFName <- paste("../ETU", etuName, "members.Rdat", sep="")
         if(print.level > 1) {
            cat("\nload household members from", memberFName)
         }
         load(memberFName)
         ## The data is called 'members'
         ## The dataframe is also called 'members'
         names(members) <- tolower(names(members))
         if(print.level > 1) {
            cat("  ", nrow(members), "obs\n")
         }

      }
      ##
      ## ---------- ID-s and some general info ---------------
      ##
      if(surveyYear == 2000)
          wave <- 1
      else
          wave<-m[[ES_wave]]
      year <- m[[ES_SWYear]]
      year[is.na(year)] <- surveyYear
      quarter <- m[[ES_quarter]]
      quarter[is.na(quarter)] <- surveyQuarter
      ## find out when the respondent should have entered the survey the first time
      yearsInSurvey <- numeric(nrow(m))
      yearsInSurvey[wave == 1] <- 0
                           # this is the first time in the survey
      yearsInSurvey[wave == 2] <- 0
                           # the first was the same year ...
      yearsInSurvey[wave == 2 & quarter == 1] <- 1
                           # ... except if the first quarter
      yearsInSurvey[wave == 3] <- 1
      yearsInSurvey[wave == 4] <- 1
      yearsInSurvey[wave == 4 & quarter == 1] <- 2
      ## original quarter: quarter the individual was in the survey for 1st time
      originalQ <- numeric(nrow(m))
                           # the quarter for the first wave HH is included
      originalQ[wave == 1] <- quarter[wave == 1]
      originalQ[wave %in% c(2,4) & quarter == 1] <- 4
      originalQ[wave %in% c(2,4) & quarter > 1] <- quarter[wave %in% c(2,4) & quarter > 1] - 1
                           # waves 2 & 4 conducted a quarter later (same year + following year)
      originalQ[wave == 3] <- quarter[wave == 3]
                           # wave 3 conducted in the same quarter following year
      household <- m[[ES_HHno]]
      ## since 2004 we have 5-digits HH codes.  Earlier we had 6-digits, ending with '1' (mostly)
      household <- ifelse((surveyYear >= 2004) & ((surveyYear - yearsInSurvey) < 2004),
                          10*m[[ES_HHno]] + 1, m[[ES_HHno]])
      ##
      idHousehold<-((2000+(year %% 100) - yearsInSurvey)*10+ originalQ)*1000000 + household
                           # common HH id over different waves
      idMember<-m[[ES_respondent]]
      idPerson<-idHousehold*100+ idMember
      if(!(ES_memberi %in% names(m))) {
         stop("The household member ID for the first wave ", ES_ethnicity,
              " not in data\n")
      }
      idObs <- ((2000+(m$year %% 100))*10+quarter)*100000000+
          household*100 + m[[ES_memberi]]
      idHHQ <- m[[ES_quarter]]*1000000 + m[[ES_HHno]]
                           # uniqu household-quarter id.  Needed when the same household is surveyed
                           # in several quarters in the same datafile, their composition may change
                           # between waves.
      if(separateMembers) {
         membersHHQ <- 1000000*members[[ES_quarter]] + members[[ES_HHno]]
         idHHCurrent <- (2000+(m$year %% 100)*10+ quarter)*1000000 + m[[ES_HHno]]
         membersHHCurrent <- ((members[[ES_SWYear]])*10+ members[[ES_quarter]])*1000000 + members[[ES_HHno]]
         idPersCurrent <- idHHCurrent*100 + idMember
         membersPersCurrent <- membersHHCurrent*100 + members[[ES_respondent]]
         row.names(members) <- membersPersCurrent
      }
                           # ID, merging different datasets for this wave
      ## Read the municipality data.  It seems to be ordered exactly in the same way as for the main data.
      ## However, we perform a few checks
      if(print.level > 1) {
         cat("\nreading municipality data..")
      }
      muniFile <- paste(etuDir, "/municipality/etu", surveyYear, surveyQuarter, "_vald.csv.gz", sep="")
      if(file.exists(muniFile)) {
         ## muni <- read.csv(gzfile(muniFile, encoding="latin1"), nrows=nrows)
         muni <- read.csv(gzfile(muniFile), nrows=nrows)
         if(!all(m$ES_HHno == muni$ES_HHno)) {
            stop("data and municipality household codes do not match")
         }
         if(!all(m$ES_respondentNo == muni$ES_respondentNo)) {
            stop("data and municipality responent codes do not match")
         }
      }
      else {
         mNames <- c(ES_residenceMuni, ES_C_workMuni)
         muni <- lapply(mNames, function(x) NA)
         names(muni) <- mNames
                           # make corresponding list of NA-s
         cat("Warning: municipality data", muniFile, "does not exist")
      }
      ## background
      sex<-m[[ES_male]]
      dateOfBirth<-ISOdate((ES_addToYear+m[[ES_yearOfBirth]]), defaultBirthMonth, defaultBirthday)
      if(print.level > 1) {
         cat("\neducation")
      }
      ##
      ## Education
      ##
      eduCurrent <- convertEdu(m[[ES_vocationalEdu]], m[[ES_generalEdu]], m[[ES_generalVocationalEdu]], etuName)
      isced <- convertISCED97(m[[ES_vocationalEdu]], m[[ES_generalEdu]], m[[ES_generalVocationalEdu]],
                              m[[LFSISCED]],
                              etuName)
      ## Unemployemnt
      jobseekStart <- ETUdate(m[[ES_jobseekStartY]],
                              m[[ES_jobseekStartM]],
                              defaultJobseekDay)
      UBEligibility <-
                           # 0 - not eligible, 1 - UI BEnefits, 2 - U Allowance
          if(etuName < "2003_1")
              2*(2 - m[[ES_UBEligibility]])
          else
              3 - m[[ES_UBEligibility]]
      UIBenefits <-
          if(etuName < "2003_1")
              0
          else
              m[[ES_UIBenefits]]
      howSearchJob <- integer(nrow(m))
      reservationWage <- if(etuName < "2008_1") NA else m[[ES_reservationWage]]
      library(bitops)
      for(i in 1:16) {
         howSearchJob <- bitShiftL(howSearchJob, 1)
         qi <- paste(ES_howSearchJob, formatC(i, width=2, flag="0"), sep="")
         howSearchJob <- bitOr(howSearchJob, m[,qi])
      }
      ## Inactivity
      prevIndustry <- ifelse(trim(m[[ES_prevEstablishIndustry]]) != "",
                             as.character(trim(m[[ES_prevEstablishIndustry]])),
                             as.character(trim(m[[ES_prevFirmIndustry]])))
                           # as.character: g05a is factor in some waves.
      ##
      ## Language and ethnicity
      ##
      if(print.level > 2)
          cat("\nlanguage")
      FILevel <- numeric(nrow(m))
      if(etuName < "2008_1") {
         # languages have numerical codes
         estLevel<-ifelse(!is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == 15), m[[LFSLangLevel1]],
                          ifelse(!is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == 15), m[[LFSLangLevel2]],
                                 ifelse(!is.na(m[[LFSLang3]]) & (m[[LFSLang3]]==15), m[[LFSLangLevel3]],
                                        ifelse(!is.na(m[[LFSLang4]]) &(m[[LFSLang4]] == 15),
                                               m[[LFSLangLevel4]], NA))))
         rusLevel<-ifelse(!is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == 1), m[[LFSLangLevel1]],
                          ifelse(!is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == 1), m[[LFSLangLevel2]],
                                 ifelse(!is.na(m[[LFSLang3]]) & (m[[LFSLang3]]==1), m[[LFSLangLevel3]],
                                        ifelse(!is.na(m[[LFSLang4]]) &(m[[LFSLang4]] == 1), m[[LFSLangLevel4]], NA))))
         engLevel<-ifelse(!is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == 153), m[[LFSLangLevel1]],
                          ifelse(!is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == 153), m[[LFSLangLevel2]],
                                 ifelse(!is.na(m[[LFSLang3]]) & (m[[LFSLang3]]==153), m[[LFSLangLevel3]],
                                        ifelse(!is.na(m[[LFSLang4]]) &(m[[LFSLang4]] == 153), m[[LFSLangLevel4]], NA))))
         i <- !is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == 179)
         FILevel[i] <- m[[LFSLangLevel1]][i]
         i <- !is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == 179)
         FILevel[i] <- m[[LFSLangLevel2]][i]
         i <- !is.na(m[[LFSLang3]]) & (m[[LFSLang3]] == 179)
         FILevel[i] <- m[[LFSLangLevel3]][i]
         i <- !is.na(m[[LFSLang4]]) & (m[[LFSLang4]] == 179)
         FILevel[i] <- m[[LFSLangLevel4]][i]
         i <- !is.na(m[[LFSLang5]]) & (m[[LFSLang5]] == 179)
         FILevel[i] <- m[[LFSLangLevel5]][i]
      }
      else {
         # languages have 3-letter codes
         estLevel<-ifelse(!is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == "est"), m[[LFSLangLevel1]],
                          ifelse(!is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == "est"), m[[LFSLangLevel2]],
                                 ifelse(!is.na(m[[LFSLang3]]) & (m[[LFSLang3]]== "est"),
                                        m[[LFSLangLevel3]],
                                        ifelse(!is.na(m[[LFSLang4]]) &(m[[LFSLang4]] == 15),
                                               m[[LFSLangLevel4]], NA))))
         rusLevel<-ifelse(!is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == "rus"), m[[LFSLangLevel1]],
                          ifelse(!is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == "rus"), m[[LFSLangLevel2]],
                                 ifelse(!is.na(m[[LFSLang3]]) & (m[[LFSLang3]]=="rus"),
                                        m[[LFSLangLevel3]],
                                        ifelse(!is.na(m[[LFSLang4]]) &(m[[LFSLang4]] == "rus"),
                                               m[[LFSLangLevel4]], NA))))
         engLevel<-ifelse(!is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == "eng"), m[[LFSLangLevel1]],
                          ifelse(!is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == "eng"), m[[LFSLangLevel2]],
                                 ifelse(!is.na(m[[LFSLang3]]) & (m[[LFSLang3]]== "eng"), m[[LFSLangLevel3]],
                                        ifelse(!is.na(m[[LFSLang4]]) &(m[[LFSLang4]] == "eng"),
                                               m[[LFSLangLevel4]], NA))))
         i <- !is.na(m[[LFSLang1]]) & (m[[LFSLang1]] == "fin")
         FILevel[i] <- m[[LFSLangLevel1]][i]
         i <- !is.na(m[[LFSLang2]]) & (m[[LFSLang2]] == "fin")
         FILevel[i] <- m[[LFSLangLevel2]][i]
         i <- !is.na(m[[LFSLang3]]) & (m[[LFSLang3]] == "fin")
         FILevel[i] <- m[[LFSLangLevel3]][i]
         i <- !is.na(m[[LFSLang4]]) & (m[[LFSLang4]] == "fin")
         FILevel[i] <- m[[LFSLangLevel4]][i]
         i <- !is.na(m[[LFSLang5]]) & (m[[LFSLang5]] == "fin")
         FILevel[i] <- m[[LFSLangLevel5]][i]
      }
      ## Now check if est and rus are any of the domestic languages
      homeLang <- logical(nrow(m))
      if(etuName < "2011") {
         ## Earlier, 1 == ET, 2 == Other
         homeLang[!is.na(m[[ES_homeLang1]]) & (as.numeric(m[[ES_homeLang1]]) == 1)] <-  TRUE
                           # in ETU2008 it is a character vector (Thnx to Neha Thakrar)
         homeLang[!is.na(m[[ES_homeLang2]]) & (as.numeric(m[[ES_homeLang2]]) == 1)] <-  TRUE
         if(!is.null(m[[ES_homeLang3]]))
             homeLang[!is.na(m[[ES_homeLang3]]) & (as.numeric(m[[ES_homeLang3]]) == 1)] <-  TRUE
         estLevel[homeLang] <- "home"
         homeLang <- logical(nrow(m))
         homeLang[!is.na(m[[ES_homeLang1]]) & (as.numeric(m[[ES_homeLang1]]) == 2)] <-  TRUE
         homeLang[!is.na(m[[ES_homeLang2]]) & (as.numeric(m[[ES_homeLang2]]) == 2)] <-  TRUE
         if(!is.null(m[[ES_homeLang3]]))
             homeLang[!is.na(m[[ES_homeLang3]]) & (as.numeric(m[[ES_homeLang3]]) == 2)] <-  TRUE
                           # assume domestic language 'other' means Russian.  Note: since LFS2008, 1 = ET,
                           # 2 = RU, 3 = other
         rusLevel[homeLang] <- "home"
      }
      else {
         ## 2011 3-digit codes
         homeLang[!is.na(m[[ES_homeLang1]]) & (m[[ES_homeLang1]] == "est")] <-  TRUE
                           # in ETU2008 it is a character vector (Thnx to Neha Thakrar)
         homeLang[!is.na(m[[ES_homeLang2]]) & (m[[ES_homeLang2]] == "est")] <-  TRUE
         if(!is.null(m[[ES_homeLang3]]))
             homeLang[!is.na(m[[ES_homeLang3]]) & (m[[ES_homeLang3]] == "est")] <-  TRUE
         estLevel[homeLang] <- "home"
         homeLang <- logical(nrow(m))
         homeLang[!is.na(m[[ES_homeLang1]]) & (m[[ES_homeLang1]] == "rus")] <-  TRUE
         homeLang[!is.na(m[[ES_homeLang2]]) & (m[[ES_homeLang2]] == "rus")] <-  TRUE
         if(!is.null(m[[ES_homeLang3]]))
             homeLang[!is.na(m[[ES_homeLang3]]) & (m[[ES_homeLang3]] == "rus")] <-  TRUE
                           # assume domestic language 'other' means Russian.  Note: since LFS2008, 1 = ET,
                           # 2 = RU, 3 = other
         rusLevel[homeLang] <- "home"
      }
      nonEst <- m[[ES_ethnicity]] != 1
      ##
      ##  Work
      ##
      #General information
      if(etuName >= "2002_1") {
         firstJob <- ISOdate(m[[ES_firstJobYear]], m[[ES_firstJobMonth]], 1)
      }
      else
          firstJob <- ISOdate(NA, NA, NA)
      #Info about particular employer/job
        wageCurrent<-m[[ES_wageCurrent]]
        nWorkersCurrent<-m[[ES_C_nWorkers]]
        publicSectorCurrent<- (m[[ES_C_publicSector]] == 12) & (m[[ES_C_ownership]] %in% c(10, 20))
                           # owned by public sector, and not market-oriented firm
      if(!(ES_C_industry %in% names(m))) {
         stop("The current intdustry ", ES_C_industry,
              " not in data\n")
      }
      industryCurrent<- m[[ES_C_industry]]
      if(ES_C_industryEstablishment != "") {
         ## Since 2011 (?) separate industry for the establishment
         industryCurrent[m[[ES_C_industryEstablishment]] != ""] <-
             m[[ES_C_industryEstablishment]][m[[ES_C_industryEstablishment]] != ""]
      }
      industryCurrent[trim(industryCurrent) %in% c("X", "")] <- NA
                           # "X" - not answered, "" - NA
        ownershipCurrent<-m[[ES_C_ownership]]
        townOrCountryWorkCurrent<-m[[ES_C_cityOfWork]]
        workCountryCurrent<-m[[ES_C_workCountry]]
      if(!(ES_C_occupation %in% names(m))) {
         stop("The household ethnicity ID ", ES_C_occupation,
              " not in data\n")
      }
      occupationCurrent<-m[[ES_C_occupation]]
        partOrFullTimeCurrent<-m[[ES_C_partOrFullTime]]
        partTimeHoursCurrent<-m[[ES_C_partTimeHours]]
      jobStartYear <- m[[ES_C_jobStartYear]]
      jobStartYear <- ifelse(jobStartYear > 9000, NA, jobStartYear)
      jobStartMonth <- m[[ES_C_jobStartMonth]]
      jobStartMonth <- ifelse(jobStartMonth > 90, NA, jobStartMonth)
      jobStartDate <- ISOdate(jobStartYear, jobStartMonth, 1)
      experience <- numeric(nrow(m))
      experience[!is.na(jobStartDate)] <- difftime(dateCurrent[!is.na(jobStartDate)], jobStartDate[!is.na(jobStartDate)],
                                                   units="days")/365.2422
        activityStatusCurrent<-m[[ES_activityStatus]]
                                     ###
                                     ###      Information about household
                                     ###
      if(print.level > 1) {
         cat("\nHouseholds: children & ethnicity")
      }
      if(separateMembers) {
         HHList <- unique(membersHHQ)
                           # could use idHHQ instead, but if low #of rows read, creates problems
         MList <- unique(members[[ES_respondent]])
                           # list of all household member ID-s (not household ID-s!)
         HHEthnicity <- HHBirthDate <- matrix(0, length(HHList), max(MList))
                           # matrix of ethnicity, birthday for all household members
         class(HHBirthDate) <- c("POSIXt", "POSIXct")
         HHEthnicity[] <- HHBirthDate[] <- NA
         row.names(HHEthnicity) <- row.names(HHBirthDate) <- HHList
         colnames(HHEthnicity) <- colnames(HHBirthDate) <- MList
         ## Next, fill in the dates
         BDay <- ISOdate(members[[ES_HHBirthday_Year]], members[[ES_HHBirthday_Month]],
                         defaultBirthday)
                           # birthdays by all HH members
         if(!(ES_ethnicity %in% names(members))) {
            stop("The household ethnicity ID ", ES_ethnicity,
                 " not in 'members' data\n")
         }
         for(mem in MList) {
            im <- members[[ES_respondent]] == mem
                           # pick the individual 'mem' from the household members
            HHBirthDate[as.character(membersHHQ[im]), mem] <- BDay[im]
            HHEthnicity[as.character(membersHHQ[im]), mem] <-
                members[[ES_ethnicity]][im]
         }
         HHAges <- (dateCurrent[!duplicated(idHHQ)] - HHBirthDate[as.character(idHHQ[!duplicated(idHHQ)]),])/365.2422
                           # ages of HH members at the survey date.  Note: a single HH may be included
                           # several times, their ages should be increasing (and # of members changing)
         HHEthnicity <- HHEthnicity[as.character(idHHQ[!duplicated(idHHQ)]),]
      }
      else {
         HHEthnicity <- HHBirthday <- matrix(0, nrow(m), length(ES_columnsOfHHMembers))
                           # ethnicity and birthday of HH members
                           # 
                           # HHBirthday will be a matrix of the birthdates of
                           # the members of the household. One row for every
                           # household and one column for every member.
                           #
                           # Initially, it is a matrix where every row corresponds to an individual in the data
         class(HHBirthday) <- c("POSIXt", "POSIXct")
         colnames(HHEthnicity) <- colnames(HHBirthday) <- ES_columnsOfHHMembers
         row.names(HHEthnicity) <- row.names(HHBirthday) <- idHHQ
         for(i in colnames(HHBirthday)) {
            HHBirthday[,i] <- ISOdate((ES_addToYear + m[[paste(ES_P_HHBirthday_Year,i,sep="")]]),
                                          defaultBirthMonth, defaultBirthday)
            HHEthnicity[,i] <- m[[paste(ES_P_ethnicity, i, sep="")]]
         }
                           # now we have the birthdates of all the member.  However
                           # note that this data is only written for the hh 'heads'
         HHBirthday <- HHBirthday[!duplicated(m[[ES_HHno]]),]
                           # now we have birthdates for all household members.
                           # row names can be used for indexing by household id
         HHAges <- (dateCurrent[!duplicated(m[[ES_HHno]])] - HHBirthday)/365.2422
                           # ages of HH members
         HHEthnicity <- HHEthnicity[!duplicated(m[[ES_HHno]]),]
      }
      HHEthnicity <- HHEthnicity == 1
                           # only distinguish Estonian - non-Estonian
      interEthHH <- apply(HHEthnicity, 1, function(x) any(x[!is.na(x)] != x[1]))
                           # logical, whether households include members of more than one ethnic group
      age0.3 <- apply(HHAges > -0.5 & HHAges < 4, 1,
                      function(x) sum(x, na.rm=TRUE))
                           # note: ages may be slightly negative because we don't know exat b'day
age4.6 <- apply(HHAges >= 4 & HHAges < 7, 1, function(x) sum(x, na.rm=TRUE))
age7.17 <- apply(HHAges >= 7 & HHAges < 18, 1, function(x) sum(x, na.rm=TRUE))
      age63.64 <- apply(HHAges >= 63 & HHAges < 65, 1, function(x) sum(x, na.rm=TRUE))
      age65. <- apply(HHAges >= 65, 1, function(x) sum(x, na.rm=TRUE))
#
      ageCurrent <- as.numeric(difftime(dateCurrent, dateOfBirth, units="days")/365.2422)
                                        # as.numeric: remove class "difference in days"
      maritalStatus <- convertMaritalStatus(m[[ES_maritalStatus]],
                                            etuName,
                                            m[[ES_separated]],
                                            m[[ES_cohabits]],
                                            m[[ES_trueMarital]])
      ##
      if(!(ES_immigrYear %in% names(m))) {
         stop("The immigration year ", ES_ethnicity,
              " not in data\n")
      }
      ##
      if(print.level > 1)
          cat("\ncompiling data..")
      d <- list(
                      id=idObs, idPerson=idPerson,
                      idHousehold=idHousehold, idMember=idMember, 
                   etuName=etuYQ,
                   wave=wave,
                   retrospective=FALSE,
                   sex=sex, 
                   birthYear=ES_addToYear+m[[ES_yearOfBirth]],
                   nonEst=nonEst,
                   age=ageCurrent,
                   immigrYear=ES_addToYear+m[[ES_immigrYear]],
                kids0.3 = age0.3[as.character(idHHQ)],
                kids4.6 = age4.6[as.character(idHHQ)],
                      kids7.17 = age7.17[as.character(idHHQ)],
                age63.64 = age63.64[as.character(idHHQ)],
                age65. = age65.[as.character(idHHQ)],
                   estLevel=estLevel,
                   rusLevel=rusLevel,
          engLevel=engLevel,
          FILevel = FILevel,
                      residenceCounty = county(m[[ES_residenceCounty]]),
                      residenceMuni = muni[[ES_residenceMuni]],
                   wage=wageCurrent,
                   date=dateCurrent,
                   firstJob=firstJob,
                      howFoundJob = m[[ES_howFoundJob]],
                   nWorkers=nWorkersCurrent,
                   publicSector=publicSectorCurrent,
                   industry=industryCurrent,
                   ownership=ownershipCurrent,
                   workCounty= county(m[[ES_C_workCounty]]),
                   workMuni= muni[[ES_C_workMuni]],
                   townOrCountryWork=townOrCountryWorkCurrent,
                   workCountry=workCountryCurrent,
                   occupation=occupationCurrent,
                      employeeStatus =
                      convertEmployeeStatus(m[[ES_C_employeeStatus]], etuName),
                   partOrFullTime=partOrFullTimeCurrent,
                   partTimeHours=partTimeHoursCurrent,
                   partTimeReason = convertPartReason(m[[ES_C_partTimeReason]], FALSE, surveyYear, surveyQuarter),
                   workhoursTotal= NA,
                      experienceInCompany=experience,
                      ## other work-related stuff
                      sidejob = m[[ES_sidejob]],
                      unionMember = m[[ES_unionMember]] == 1,
                      ## Unemployment
                      UDuration = as.numeric(difftime(dateCurrent,
                                             jobseekStart, units="weeks")),
                      UBEligibility = UBEligibility,
                      UIBenefits = UIBenefits,
                      UAllowance = m[[ES_UAllowance]],
                howSearchJob = howSearchJob,
                mainSearchMethod = m[[ES_mainSearchMethod]],
                reservationWage = reservationWage,
                reservationWageLevels = m[[ES_reservationWageLevels]],
                searchParttime = m[[ES_searchParttime]],
                prevIndustry = prevIndustry,
                prevOccupation = m[[ES_prevOccupation]],
                      ## Education
                      edu=eduCurrent,
          isced97=isced,
                      studying = m[[ES_currentlyStudying]] == 1,
                      ##
          maritalStatus=maritalStatus,
                interEthHH=interEthHH[as.character(idHHQ)],
                   workForceStatus = m[[ES_workForceStatus]],
                   activityStatus=activityStatusCurrent,
                   personWeight=m[[ES_weight]]
             )
      if(print.level > 0) {
         cat("\n")
      }
      l <- sapply(d, length)
      if(!all(l %in% c(1, nrow(m)))) {
         print(l)
         stop("variable lengths differ in data frame")
      }
      return(as.data.frame(d, stringsAsFactors=FALSE))
}
   d2000q3.2010 <-doEtu("2000_3")
 for(etu in c(                              "2000_4",
              "2001_1", "2001_2", "2001_3", "2001_4",
              "2002_1", "2002_2", "2002_3", "2002_4",
              "2003_1", "2003_2", "2003_3", "2003_4",
              "2004_1", "2004_2", "2004_3", "2004_4",
              "2005_1", "2005_2", "2005_3", "2005_4",
              "2006_1", "2006_2", "2006_3", "2006_4",
              "2007_1", "2007_2", "2007_3", "2007_4",
              "2008_1", "2008_2", "2008_3", "2008_4",
              "2009_1-4",
              "2010_1-4",
              "2011_1-4",
              "2012_1-4",
              "2013_1-4"
              )) {
    etuC<-doEtu(etu)
    d2000q3.2010 <-rbind(d2000q3.2010, etuC)
 }
   fName <- "dataOf2000q3-2010All.Rdat"
   save(d2000q3.2010, file=fName)
   invisible(d2000q3.2010)
}
