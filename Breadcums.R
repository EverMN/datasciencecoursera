GETTING AND CLEANING DATA

======

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile="housing.csv"￼￼)

housing <- read.csv("./Data/housing.csv")

sum(housing[,"VAL"]>=24, na.rm=TRUE)

housing[,"FES"]=="b"
dat <- read.table("./Data/NGAP.xlsx")

library(xlsx)
dat <- read.csv("./Data/Dat.csv")
dat
sum(dat$Zip*dat$Ext,na.rm=T) 
library(xml)

?fread

DT <- read.csv("./Data/ss06pid.csv")

mean(DT$pwgtp15,by=DT$SEX)

DT[,mean(pwgtp15),by=SEX]

rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]

sapply(split(DT$pwgtp15,DT$SEX),mean)

tapply(DT$pwgtp15,DT$SEX,mean)

mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)

#QUIZZ 2

install.packages("sqldf")
library("sqldf")
sqldf(select * from Dat)


con = url("http://biostat.jhsph.edu/~jleek/contact.html")
html <- readLines(con)
html
close(con)
html
cat(html)
html[1]
nchar(html[100])
read.table("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
read.fwf(file="Data/wksst8110.for",widths=c(14, 5, 8, 5, 8, 5, 8, 5, 4)) -> datosFeos
sum(datosFeos[,4])


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, destfile="./Data/Dict06.csv", method="curl")
list.files("./Data")
"./Data/Dict06.csv"
Dict06 <- read.csv("./Data/Dict06.csv")
Dict06
summary(Dict06)
str(Dict06)

agricultureLogical <- Dict06$ACR == 3 & Dict06$AGS == 6
agricultureLogical

which(agricultureLogical)
library("jpeg")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url, destfile="./Data/Fjeff.jpg", method="curl")
jpegValues <- readJPEG("./Data/Fjeff.jpg", native = TRUE)
quantile(jpegValues, probs = c(0.30, 0.80))

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url, destfile="./Data/GDP.csv", method="curl")
gdp <- read.csv("./Data/GDP.csv")
gdp
summary(gdp)
str(gdp)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url, destfile="./Data/educational.csv", method="curl")
educational <- read.csv("./Data/educational.csv")
str(educational)

gdp1 <- gdp[,c(1,2,4,5)]

head(gdp1)

str(gdp1)

names(gdp1)
names(educational)
mergedData <- merge(gdp1, educational, by.x="X" , by.y="CountryCode",all=TRUE)
head(mergedData)

joined <- join(educational, gdp1, by=c("CountryCode"), match="first")
joined
str(joined)
matchCounter <- all(!is.na(joined$Ranking),!is.na(joined$Long.Name))
sum(matchCounter)
matchCounter
sum(!is.na(joined$Ranking))
sort(joined$Ranking, decreasing=TRUE)
joinedSorted <- joined[order(joined$Ranking,decreasing = TRUE),]
order(as.numeric(joined1$Ranking), decreasing=TRUE)
sort(gdp$Ranking, decreasing=TRUE)

gdp <- transform(gdp,
                 X = X,
                 Ranking = as.numeric(Ranking),
                 Economy = Economy,
                 US.dollars = as.numeric(US.dollars.)
                 )
head(gdp[sort(gdp$Ranking, decreasing=TRUE),],13)
gdpAverage<- data.frame(joined$Income.Group, joined$Ranking)
str(gdpAverage)

tapply(gdpAverage$joined.Ranking, gdpAverage$joined.Income.Group, mean)

write.table(gdpAverage, "./Data/GDPAverage.csv",sep = ",")

library(plyr)







install.packages("dplyr")
library(dplyr)

gdp <- read.csv("./Data/GDP.csv", stringsAsFactors = FALSE)
educational <- read.csv("Data/educational.csv")

names(gdp)
names(educational)

gdp.f <- select(gdp, c(1,2,4,5))

names(gdp.f) <- c("CountryCode", "Ranking", "Economy", "USD")
names(gdp.f)
names(educational)

head(gdp.f)
tail(gdp.f)
head(educational)
tail(educational)

gdpAndEducation <- join(gdp.f,educational)

head(gdpAndEducation[,1:6])
tail(gdpAndEducation[,1:6])
sum(gdpAndEducation$Ranking != "",na.rm = TRUE)
gdpAndEducation.f <- filter(gdpAndEducation, Ranking != "")

subGdpEd <- gdpAndEducation.f[1:190,1:7]
names(subGdpEd)

subGdpEd <- transform(subGdpEd,
                 CountryCode = CountryCode,
                 Ranking = as.numeric(as.character(Ranking)),
                 Economy = Economy,
                 USD = USD,
                 Long.Name = Long.Name,
                 Income.Group = Income.Group,
                 Region = Region
)

subGdpEd.a <- arrange(subGdpEd, desc(Ranking))
head(subGdpEd.a,n=13)
str(subGdpEd)
tapply(subGdpEd$Ranking, subGdpEd$Income.Group, mean)



subGdpEd$RankingGroups = cut(subGdpEd$Ranking, breaks=quantile(subGdpEd$Ranking, c(0, 0.2, 0.4, 0.6, 0.8, 1)))

table(subGdpEd$RankingGroups, subGdpEd$Income.Group)


fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileurl, destfile="./Data/ss06hid.csv", method="curl")

ss06hid <- read.csv("./Data/ss06hid.csv")
names(ss06hid)

splitnames <- strsplit(names(ss06hid),"wgtp")
splitnames[123]

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileurl, "./Data/datagdp.csv", method = "curl")
datagdp <- read.csv("./Data/datagdp.csv")

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileurl, "./Data/fedstatscountry.csv", method = "curl")
fedstatscountry <- read.csv("./Data/fedstatscountry.csv")


"" "15"
377652.4
grep("^United",countryNames), 3
13
250, 47