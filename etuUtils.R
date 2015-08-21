
convertSav <- function(savName,
                       newName=paste("ETU", substr(savName, 4, 7), "_", substr(savName, 8, 8), ".Rdat", sep=""),
                       overwrite=FALSE) {
   ## Convert the .sav files from Statistical Office (?) to a
   ## We will try to guess the name of the ETU from the name of
   ## .sav by assuming naming convention like etuYYYYQ.sav
   library(Hmisc)
   cat("importing ", savName, "and converting to", newName, "\n")
   etu <- spss.get(savName, lowernames=TRUE)
   ## we need to trim the factor names, as they have some
   ## whitespace at the end.  The function trim() itself
   ## (found at http://wiki.math.yorku.ca/index.php/R:_Data_conversion_from_SPSS)
   ## is defined later in this file.
   etu <- trim(etu)
   ## Guessing the filename:
   cat("saving the file as ", newName, "...")
   members <- etu
   save(members, file=newName)
   cat("\ndone\n")
}

convertIndustry95 <- function(activity1) {
   ## Transform the activity codes of ETU1995, 1997 to a more standard one
   ## 1:2 <-A
   ## 5 <- B
   ## 10:14 <-C
   ## 15:37<-D
   ## 40:41<-E
   ## 45<-F
   ## 50:52<-G
   ## 55<-H
   ## 60:64<-I
   ## 65:67<-J
   ## 70:74<-K
   ## 75 <-L
   ## 80<-M
   ## 90:93<-O
   ## 95<-P
   ## 99<-Q
   activity <- character(length(activity1))
   activity[activity1%in%c(1:2)]<-"A"
   activity[activity1%in%c(5)]<-"B"
   activity[activity1%in%c(10:14)]<-"C"
   activity[activity1%in%c(15:37)]<-"D"
   activity[activity1%in%c(40:41)]<-"E"
   activity[activity1%in%c(45)]<-"F"
   activity[activity1%in%c(50:52)]<-"G"
   activity[activity1%in%c(55)]<-"H"
   activity[activity1%in%c(60:64)]<-"I"
   activity[activity1%in%c(65:67)]<-"J"
   activity[activity1%in%c(70:74)]<-"K"
   activity[activity1%in%c(75)]<-"L"
   activity[activity1%in%c(80)]<-"M"
   activity[activity1%in%c(90:93)]<-"O"
   activity[activity1 %in% c(85)] <- "N"
                                        # healt & social work
   activity[activity1%in%c(95)]<-"P"
   activity[activity1%in%c(99)]<-"Q"
   activity[activity1 %in% c(0)] <- NA
                                        # what's that?
   activity[is.na(activity1)] <- NA
   activity[activity1 > 900] <- NA
   dim(activity) <- dim(activity1)
   return(activity)
}

convertPartReason <- function(reason, retrospective, year, quarter=1) {
   ## convert various questions about the reason for working part-time to a standard form:
   ## year     Year of the survey
   ## quarter  quarter of the survey
   ## 
   ## 1	Õpingud
   ## 2	Enda haigus või vigastus
   ## 3	Rasedus- või sünnituspuhkus
   ## 4	0
   ## 5	Vajadus hoolitseda laste eest
   ## 6	
   ## 7	
   ## 8	Vajadus hoolitseda teiste pereliikmete eest
   ## 9	Tellimuste või töö vähesus
   ## 10	Remont, tehniline rike vms
   ## 11	Materjali- või tooraine vähesus
   ## 12	Minu tööl loetakse täistööajaks vähem kui 35 tunnist töönädalat
   ## 13	Ei ole täisajatööd leidnud
   ## 14	Soovisin säilitada täispensioni
   ## 15	Ei soovinud täisajaga töötada
   ## 16	MUU [KIRJUTAGE TABELISSE
   t <- 1:16
   if(year == 1995) {
      if(retrospective) {
         names(t) <- c("1", "2", "3", "", "4", "", "", "5", "6", "", "", "", "8", "9", "10", "11")
      }
      else {
         names(t) <- c("1", "2", "3", "", "4", "", "", "5", "6", "", "7", "8", "9", "", "10", "11")
      }
   }
   else if(year == 1997) {
      if(retrospective) {
         names(t) <- c("1", "2", "3", "4", "5", "", "", "6", "7", "8", "", "", "9", "", "10", "11")
      }
      else {
         names(t) <- c("1", "2", "3", "4", "5", "", "", "6", "7", "", "8", "9", "10", "", "11", "12")
      }
   }
   else if((year %in% c(1998, 1999)) | ((year == 2000) & (quarter < 3))) {
      if(retrospective) {
         names(t) <- c("1", "2", "3", "4", "5", "", "", "6", "7", "8", "", "", "9", "", "10", "11")
      }
      else {
         names(t) <- c("1", "2", "3", "4", "5", "", "", "6", "7", "", "8", "9", "10", "", "11", "12")
      }
   }
   else if((year %in% 2001:2004) | ((year == 2000) & (quarter > 2))) {
      names(t) <- c("1", "2", "3", "4", "5", "", "", "6", "7", "", "8", "9", "10", "", "11", "12")
   }
   else if(year %in% 2005:2013) {
      # 2011-2013 not tested!
      names(t) <- c("1", "2", "3", "4", "", "5", "6", "", "7", "", "8", "9", "10", "", "11", "12")
   }
   else
       stop(paste("unknown year", year))
   t[as.character(reason)]
}

convertSchool <- function(school, wave) {
   s <- character(length(school))
   if(wave == "1995") {
      s[school %in% c(1)] <- "<=basic"
      s[school %in% c(2,3,4,5,6,7,8)] <- "highSchool"
      s[school %in% c(9,10)] <- "college"
      s[school %in% c(11)] <- NA
   }
   else if(wave %in% c("1997", "1998")) {
      s[school %in% c(1,2,4)] <- "<=basic"
      s[school %in% c(3,5,6,7,8,9)] <- "highSchool"
      s[school %in% c(10,11,12)] <- "college"
      s[school %in% 13] <- NA
   }
   else if(wave %in% c("1999","2000_1j2")) {
      s[school %in% c(1,2,4,5,7)] <- "<=basic"
      s[school %in% c(3,6,8,9,10,11,12)] <- "highSchool"
      s[school %in% c(13,14,15)] <- "college"
      s[school %in% c(16)] <- NA
   }
   else
       stop("wrong wave", wave)
   dim(s) <- dim(school)
   s
}

convertSearch95 <- function(s) {
## convertSearch95: convert the way of getnvting a job in ETU1995-2000-12 to later form
   s - 1
}

defactor <- function(int) {
   ## convert factor to integer, if not already integer
   if(is.factor(int)) {
      int <- levels(int)[int]
   }
   as.integer(int)
}

eraldaMuutujad <- function(etu, variables,
                           nrows=-1,
                           print.level=0) {
   ## extracts certain variables from .tsv.gz file
   ## variable name '""' is ignored
   ## nrows       how many rows to read (-1 = all)
   ##
   ## eduDir, eraldaCommand are defined in Parameters.R
   etuFName <- paste(etuDir, etu, ".tsv.gz", sep="")
   outFName <- paste(etuDir, "variables.tsv", sep="")
   con <- gzfile(etuFName)
   header <- readLines(con, n=1, encoding="lat1")
   close(con)
   header <- strsplit(header, "\t")[[1]]
   variables <- variables[nchar(variables) > 0]
   iVar <- integer(0)
   for(v in variables) {
      iVar <- c(iVar, grep(paste("^", v, sep=""), header))
   }
   if(length(iVar) == 0)
       stop("No variables!")
   cols <- paste(iVar - 1, collapse=" ")
   command <- paste(eraldaCommand, etuFName, outFName, cols)
   if(print.level > 0)
       cat(command, "\n")
   res <- system(command)
   if(res > 0)
       cat("command resulted exit code", res, "\n")
   ## seems like 64-bit version does not want 'encoding' argument ??  Not any more in 2.9.2 ??
   if(R.Version()$arch %in% c("i486", "i686"))
       data <- read.delim(con <- file(outFName, encoding="ISO8859-1"), quote="", nrows=nrows)
   else
       data <- read.delim(con <- file(outFName, encoding="ISO8859-1"), quote="", nrows=nrows)
   # file.remove(outFName)
   invisible(data)
}

ETUdate <- function(y, m, d) {
   y <- ifelse(y < 100, y + 1900, y)
   ISOdate(y, m, d)
}

getEducation<-function(dateOfEdu, eduLast, eduFinishedDates, eduTypeOfSchool, eduFinishingStatus, yearOfFirstHigherEduc){
    graduatedAfter<-ifelse((eduFinishedDates>dateOfEdu&eduFinishingStatus==1), eduTypeOfSchool, NA)
    secSchoolType<-c(2,3,6)
    higherSchoolType<-c(8,9)
# if finished secondary, vocational secondary or technical on the basis of secondary education, then education must have been
# primary before it.
# if finished applied higher school or higher school, then education must have been secondary before it (NB - in case 
# it is second education, we will be wrong!).
# else the last education is already attained at the date
    eduAtDate<-ifelse(graduatedAfter[,1]%in%secSchoolType|graduatedAfter[,2]%in%secSchoolType|graduatedAfter[,3]%in%secSchoolType|graduatedAfter[,4]%in%secSchoolType|graduatedAfter[,5]%in%secSchoolType, "<=basic",
           ifelse(graduatedAfter[,1]%in%higherSchoolType|graduatedAfter[,2]%in%higherSchoolType|graduatedAfter[,3]%in%higherSchoolType|graduatedAfter[,4]%in%higherSchoolType|graduatedAfter[,5]%in%higherSchoolType, "highSchool", eduLast))
           #edu<-factor(edu)
#     eduAtDate<-ifelse(ISOdate(yearOfFirstHigherEduc,12,31)<dateOfEdu, "college", eduAtDate)
     # Checking for previous higher educations...
  return(eduAtDate)
 }

hasValue<-function(z) {
   ## is TRUE and not NA
    !is.na(match(TRUE,z))
 }

matIndex <- function(index, data) {
   ## picks from the matrix 'data' vector of elemets, pointed to by logical matrix 'index'.
   ## Essentially logical indexing of matrix, however, only on NA per row is returned.
   ## index     logical index matrix, NA-s allowed
   ## data      matrix, same size as 'index'
   out <- vector(storage.mode(data), nrow(data))
   out[] <- NA
   for(i in seq(length=ncol(index))) {
      colI <- index[,i]
      colI[is.na(colI)] <- FALSE
      out[colI] <- data[colI, i]
   }
   out
}

preSchool <- function(school, wave) {
   ## education, assumed _before_ graduating this school
   s <- character(length(school))
   if(wave == "1995") {
      s[school %in% c(1, 2, 3, 5, 7, 8)] <- "<=basic"
      s[school %in% c(4, 6, 9)] <- "highSchool"
      s[school %in% c(10)] <- "college"
      s[school %in% c(11)] <- NA
   }
   else if(wave %in% c("1997", "1998")) {
      s[school %in% c(1,2,3,4,5, 7)] <- "<=basic"
      s[school %in% c(6,8,9,10)] <- "highSchool"
      s[school %in% c(11,12)] <- "college"
      s[school %in% 13] <- NA
   }
   else if(wave %in% c("1999", "2000_1j2")) {
      s[school %in% c(1,2,4,5,6,7,9)] <- "<=basic"
      s[school %in% c(8,10,11,12,13)] <- "highSchool"
      s[school %in% c(14,15)] <- "college"
      s[school %in% c(16)] <- NA
   }
   else
       stop("wrong wave", wave)
   dim(s) <- dim(school)
   s
}

testPanel <- function(data, print.level=1, backQuarters=8) {
   ## we construct a table where we check how many individuals are present during which quarters
   ex <- function(date) {
      ## return x coordinate (in range [0,1]) for plotting date
      (year(date) + quarter(date)/4 - year(minDate) - quarter(minDate)/4)/
          (year(maxDate) + quarter(maxDate)/4 - year(minDate) - quarter(minDate)/4)
   }
   linQuarter <- function(date) {
      (year(date) - year(minDate))*4 + quarter(date) - quarter(minDate) + 1
   }
   quarter <- function(date) {
      m <- as.numeric(format.POSIXct(date, "%m"))
      Q <- (m - 1) %/% 3 + 1
   }
   toText <- function(date) {
      ## Convert survey quarter to text
      ## first order the dates
      date <- date[order(date)]
      paste(year(date), ":", quarter(date), sep="", collapse="-")
   }
   toNumeric <- function(date) {
      yq <- numeric(2*length(date))
      iYear <- seq(from=1, to=length(yq), by=2)
      iQuarter <- seq(from=2, to=length(yq), by=2)
      yq[iYear] <- year(date)
      yq[iQuarter] <- quarter(date)
      attr(yq, "iYear") <- iYear
      attr(yq, "iQuarter") <- iQuarter
      yq
   }
   cat("Original:", nrow(data), "rows\n")
   data <- data[!data$retrospective,]
   cat("Non-retrospective", nrow(data), "rows\n")
   minDate <- min(data$date)
   maxDate <- max(data$date)
   panelQ <- tapply(data$date, factor(data$idPerson),
                           # re-factor to remove unused levels
                    function(x) {
                       attr(x, "string") <- toText(x)
                       x
                       }
                    , simplify=FALSE)
                           # collect the individual observation dates over the panel
   cat("ordering")
   panelQ <- panelQ[order(sapply(panelQ, function(x) attr(x, "string")))]
                           # order it starting from the first quarter
   cat("\n")
   strDate <- sapply(panelQ, function(x) attr(x, "string"))
   panelF <- factor(strDate)
                           # integer, describing the categories
   nq <- (year(maxDate) - year(minDate))*4 + quarter(maxDate) - quarter(minDate) + 1
                           # # of quarters on x-axis
   cat(length(levels(panelF)), "different survey quarters\n")
   if(print.level > 1) {
      print(levels(panelF))
   }
   ## Calculate the graph positions
   maxEY <- 0
                           # maximum height where there is something plotted
   minEY <- numeric(nq)
   minEY[] <- Inf
                           # minimum height where there is something plotted for this quarter
   ey <- 0
   for(f in levels(panelF)) {
      ## we go over all existing combinations of survey quarters
      i <- which(f == panelF)
      N <- length(i)
      repDate <- panelQ[[i[1]]]
                           # get the quarters of one (representative) individual
      qs <- toNumeric(repDate)
      ex0 <- ex(repDate[1])
      ex1 <- ex(tail(repDate, 1))
      ey0 <- ey
      ey <- ey1 <- ey + N
      lastQs <- seq(from=max(linQuarter(repDate[1]) - backQuarters, 1), to=linQuarter(repDate[1]))
      if(all(N < minEY[lastQs])) {
         ey0 <- 0
         ey <- ey1 <- N
      }
      maxEY <- max(maxEY, ey1)
      for(d in repDate) {
         class(d) <- class(repDate)
         minEY[linQuarter(d)] <- min(minEY[linQuarter(d)], ey0)
      }
   }
   if(print.level > 1)
       cat("maxEY", maxEY, "\n")
   ## Now we have calculated the graph positions.  Plot.
   plot(0:1, c(0,maxEY), type="n", xaxt="n", xlab="", ylab="Number of individuals")
   axis(1, at=seq(from=0, to=1, length=nq), tcl=-0.3, labels=FALSE)
   axis(1, at=seq(from=0, to=1, length=nq), tck=1, lty=3, col="gray", labels=FALSE)
   yt0 <- (5 - quarter(minDate)) %% 4
                           # which tick is the first quarter of the first full year
   yt1 <- nq - quarter(maxDate) + 1
   year0 <- year(minDate) + (quarter(minDate) > 1)
   axis(1, at=seq(from=yt0, to=yt1, by=4)/(nq - 1), tcl=-0.8,
        labels=seq(from=year0, to=year(maxDate)))
   axis(1, at=seq(from=yt0, to=yt1, by=4)/(nq - 1), tck=1, lty=2, col="gray", labels=FALSE)
   axis(4, tck=1, lty=3, col="gray")
   minEY <- numeric(nq)
   minEY[] <- Inf
                           # minimum height where there is something plotted for this quarter
   ey <- 0
   for(f in levels(panelF)) {
      ## we go over all existing combinations of survey quarters
      i <- which(f == panelF)
      N <- length(i)
      repDate <- panelQ[[i[1]]]
                           # get the quarters of one (representative) individual
      qs <- toNumeric(repDate)
      ex0 <- ex(repDate[1])
      ex1 <- ex(tail(repDate, 1))
      ey0 <- ey
      ey <- ey1 <- ey + N
      lastQs <- seq(from=max(linQuarter(repDate[1]) - backQuarters, 1), to=linQuarter(repDate[1]))
      if(all(N < minEY[lastQs])) {
         ey0 <- 0
         ey <- ey1 <- N
      }
      rect(ex0, ey0, ex1, ey1, col=rgb(0,0,0, 0.3), border=NA)
                           # draw the bloc for all the survey period
      for(d in repDate) {
         class(d) <- class(repDate)
         segments(ex(d), ey0, ex(d), ey1, col="red", lwd=3)
         minEY[linQuarter(d)] <- min(minEY[linQuarter(d)], ey0)
      }
   }
}

### Functions for trimming the factornames of SPSS
### "loaned" from http://wiki.math.yorku.ca/index.php/R:_Data_conversion_from_SPSS
trim <- function(x)
    UseMethod("trim")

trim.data.frame <- function(x) {
    for ( nn  in names(x)) x[[nn]] <- trim(x[[nn]])
    x
}

trim.factor <- function( x ) {
    levels(x) <- sub(" +$", "", levels(x))
    x
}

trim.character <- function( x ) {
   x <- sub("^[[:space:]]+", "", x)
   x <- sub("[[:space:]]+$", "", x)
   x
}
trim.default <- function(x) x

year <- function(x) {
   as.numeric(format.POSIXct(x, "%Y"))
}

yQuarter <- function(x) {
   paste(format.POSIXct(x, "%Y"), quarters(x), sep="-")
}

writeDta <- function(panel) {
   library(foreign)
   panel$idPerson <- (levels(panel$idPerson)[panel$idPerson])
   write.dta(panel, file="etu.dta")
   system("gzip -f etu.dta")
   cat("file is etu.dta.gz\n")
}

writeTable <- function(etu, fName="etu") {
   write.table(etu, file=gzfile(paste(fName, ".tsv.gz", sep="")),
               sep="\t", quote=FALSE, row.names=FALSE)
}
