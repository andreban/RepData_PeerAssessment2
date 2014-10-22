# Comparison of Economic and Public Health Damages on the NOAA storm database
Andre Bandarra  
17-10-2014  
#Introduction

The objective of this analysis is provide a brief comparison of the economic and public health damage caused by events on the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.

We show the top 10 causes for economic damage and the top 10 causes for public health damage, and compare the biggest causes of each one. 

More information on the original database can be found on the following URLs:
 - National Weather Service Storm Data Documentation[https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf]
 - National Climatic Data Center Storm Events FAQ[https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf]


#Processing the Data File

Download the Storm Data file to the *data* directory.

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2";
localfile <- "data/stormdata.csv.bz2";

if (!file.exists("data")){
    dir.create("data")
}

if (!file.exists(localfile)) {
    download.file(url, localfile,method="curl")    
}
```


Read the data from the bzfile

```r
stormdata <- read.csv(bzfile(localfile))
```

Make the column names of the dataset tidy by transforming the column names to lowercase.

```r
colnames(stormdata) <-tolower(colnames(stormdata))
colnames(stormdata)
```

```
##  [1] "state__"    "bgn_date"   "bgn_time"   "time_zone"  "county"    
##  [6] "countyname" "state"      "evtype"     "bgn_range"  "bgn_azi"   
## [11] "bgn_locati" "end_date"   "end_time"   "county_end" "countyendn"
## [16] "end_range"  "end_azi"    "end_locati" "length"     "width"     
## [21] "f"          "mag"        "fatalities" "injuries"   "propdmg"   
## [26] "propdmgexp" "cropdmg"    "cropdmgexp" "wfo"        "stateoffic"
## [31] "zonenames"  "latitude"   "longitude"  "latitude_e" "longitude_"
## [36] "remarks"    "refnum"
```

Create a new dataset, containing only the rows that have property damage, crop damage, injury or fatality. Also, preserve only the columns that have information about the event type, property damage, crop damage, injury and fatality.

The columns we need for the comparison are:

- evtype: the event type.
- propdmg: the 3 most significant digits property damages.
- propdmgexp:  the magnitude of the probdmg.
- cropdmg: the 3 most sinificant digits of the crop damages.
- cropdmgexp: the magnitude of cropdmg.
- injuries: the number of injuries, including direct and indirect.
- fatalities: the number of fatalities, including direct and indirect.

```r
damageInjurydata <- stormdata[stormdata$propdmg > 0 | stormdata$cropdmg > 0
                              | stormdata$fatalities > 0 | stormdata$injuries > 0, c(8,23,24,25,26,27,28)]
```


The evttype, propdmgexp and cropdmgexp lack standartization. So, we try to clean the data as much as possible by transforming the propdmbexp, cropdmgexp and evtype to lowercase and then trim the white spaces.

```r
damageInjurydata$evtype <- gsub("^\\s+|\\s+$", "",tolower(damageInjurydata$evtype))

damageInjurydata$propdmgexp <- gsub("^\\s+|\\s+$", "",tolower(damageInjurydata$propdmgexp))

damageInjurydata$cropdmgexp <- gsub("^\\s+|\\s+$", "",tolower(damageInjurydata$cropdmgexp))
```

The documentations describes that on the damage columns (cropdmg and cropdmg), the variable stores the 3 most significant digits from the value. cropdmgexp and propdmgexp describe the magnitude of the values, such that:

- "k" equals thousants
- "m" equals millions
- "b" equals billions.

Transform the values to the correct magnitude


```r
damageInjurydata$propdmg[damageInjurydata$propdmgexp == 'k'] <- damageInjurydata$propdmg[damageInjurydata$propdmgexp == 'k'] * 1000

damageInjurydata$propdmg[damageInjurydata$propdmgexp == 'm'] <- damageInjurydata$propdmg[damageInjurydata$propdmgexp == 'm'] * 1000000

damageInjurydata$propdmg[damageInjurydata$propdmgexp == 'b'] <- damageInjurydata$propdmg[damageInjurydata$propdmgexp == 'b'] * 1000000000


damageInjurydata$cropdmg[damageInjurydata$cropdmgexp == 'k'] <- damageInjurydata$cropdmg[damageInjurydata$cropdmgexp == 'k'] * 1000

damageInjurydata$cropdmg[damageInjurydata$cropdmgexp == 'm'] <- damageInjurydata$cropdmg[damageInjurydata$cropdmgexp == 'm'] * 1000000

damageInjurydata$cropdmg[damageInjurydata$cropdmgexp == 'b'] <- damageInjurydata$cropdmg[damageInjurydata$cropdmgexp == 'b'] * 1000000000
```


Sum the crop damage data with the property damage data, so as to get the total economic damage. Also, sum the number of injuries and the number of fatalities to calculate to total public health damage.

```r
damageInjurydata$totaleconomicdamage <- damageInjurydata$cropdmg + damageInjurydata$propdmg;
damageInjurydata$totalhealthdamage <- damageInjurydata$injuries + damageInjurydata$fatalities;
```

Aggregate the total human losses and total damage by event type.

```r
lossbyevttype <- aggregate(cbind(totalhealthdamage, totaleconomicdamage) ~ evtype, damageInjurydata, FUN=sum)

summary(lossbyevttype)
```

```
##     evtype          totalhealthdamage totaleconomicdamage
##  Length:444         Min.   :    0     Min.   :0.00e+00   
##  Class :character   1st Qu.:    0     1st Qu.:5.00e+03   
##  Mode  :character   Median :    0     Median :1.00e+05   
##                     Mean   :  351     Mean   :1.07e+09   
##                     3rd Qu.:    5     3rd Qu.:5.00e+06   
##                     Max.   :96979     Max.   :1.50e+11
```


Calculate the Top 10 Event Types by Economic Damage and Top 10 Event Type by Health Damage.

```r
sortedDmg <- lossbyevttype[order(lossbyevttype$totaleconomicdamage, decreasing=T),]

sortedHuman <- lossbyevttype[order(lossbyevttype$totalhealthdamage, decreasing=T),]

top10Dmg <- sortedDmg[c(1:10),]

nrows <- length(sortedDmg[,1])

othersDmg <- sum(sortedDmg$totaleconomicdamage[11:nrows])
othersInjuries <- sum(sortedDmg$totalhealthdamage[11:nrows])

top10Dmg <- rbind(top10Dmg, c("others", othersInjuries, othersDmg))

top10Inj <- sortedHuman[c(1:10),]
othersDmg <- sum(sortedHuman$totaleconomicdamage[11:nrows])
othersInjuries <- sum(sortedHuman$totalhealthdamage[11:nrows])

top10Inj <- rbind(top10Inj, c("others", othersInjuries, othersDmg))
```

#Results

```r
par(mfrow = c(1,2))
pie(as.numeric(top10Dmg$totaleconomicdamage), labels=top10Dmg$evtype, main="Top Events by Economic Damage", cex=0.7)
pie(as.numeric(top10Inj$totalhealthdamage), labels=top10Inj$evtype, main="Top Events by Health Damage", cex=0.7)
```

![plot of chunk unnamed-chunk-10](./PeerAssessment2_files/figure-html/unnamed-chunk-10.png) 


###Top Events by Economic Damage  

```r
library(xtable)
print(xtable(top10Dmg[,c(1,3)]), type="html", ,comment=FALSE)
```

<table border=1>
<tr> <th>  </th> <th> evtype </th> <th> totaleconomicdamage </th>  </tr>
  <tr> <td align="right"> 74 </td> <td> flood </td> <td> 150319678257 </td> </tr>
  <tr> <td align="right"> 200 </td> <td> hurricane/typhoon </td> <td> 71913712800 </td> </tr>
  <tr> <td align="right"> 368 </td> <td> tornado </td> <td> 57352114048.7 </td> </tr>
  <tr> <td align="right"> 314 </td> <td> storm surge </td> <td> 43323541000 </td> </tr>
  <tr> <td align="right"> 112 </td> <td> hail </td> <td> 18758221520.7 </td> </tr>
  <tr> <td align="right"> 61 </td> <td> flash flood </td> <td> 17562179167.1 </td> </tr>
  <tr> <td align="right"> 39 </td> <td> drought </td> <td> 15018672000 </td> </tr>
  <tr> <td align="right"> 191 </td> <td> hurricane </td> <td> 14610229010 </td> </tr>
  <tr> <td align="right"> 277 </td> <td> river flood </td> <td> 10148404500 </td> </tr>
  <tr> <td align="right"> 213 </td> <td> ice storm </td> <td> 8967041360 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> others </td> <td> 68449048816.21 </td> </tr>
   </table>



###Top Events by Health Damage  

```r
print(xtable(top10Inj[,c(1,2)]), type="html", ,comment=FALSE)
```

<table border=1>
<tr> <th>  </th> <th> evtype </th> <th> totalhealthdamage </th>  </tr>
  <tr> <td align="right"> 368 </td> <td> tornado </td> <td> 96979 </td> </tr>
  <tr> <td align="right"> 50 </td> <td> excessive heat </td> <td> 8428 </td> </tr>
  <tr> <td align="right"> 384 </td> <td> tstm wind </td> <td> 7461 </td> </tr>
  <tr> <td align="right"> 74 </td> <td> flood </td> <td> 7259 </td> </tr>
  <tr> <td align="right"> 228 </td> <td> lightning </td> <td> 6046 </td> </tr>
  <tr> <td align="right"> 129 </td> <td> heat </td> <td> 3037 </td> </tr>
  <tr> <td align="right"> 61 </td> <td> flash flood </td> <td> 2755 </td> </tr>
  <tr> <td align="right"> 213 </td> <td> ice storm </td> <td> 2064 </td> </tr>
  <tr> <td align="right"> 329 </td> <td> thunderstorm wind </td> <td> 1621 </td> </tr>
  <tr> <td align="right"> 438 </td> <td> winter storm </td> <td> 1527 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> others </td> <td> 18496 </td> </tr>
   </table>



#Conclusion
Altough Tornados are, by far, the greatest threat to public health, it is only on the 3rd place when looking at the economic damage. 

Floods and Hurricanes/Typhoons are the events that cause more economic damage, but Floods are only the 4th cause of Health Damages and Hurricanes/Typhoons dont show up on the list. 

Excessive Heat and Tsmt Wind are the 2nd and 3rd greatest causes of health damage, but dont show on the top 10 list of economic damage. 








