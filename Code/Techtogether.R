crime <- read.csv("crime2018.csv")

#map to census block
lat18 <- as.numeric(crime$lat)
lon18 <- as.numeric(crime$long)
l <- nrow(crime)
result <- list()
for(i in 1:l){
  api18 <- paste0("https://geo.fcc.gov/api/census/block/find?latitude=",lat18[i],"&longitude=",lon18[i],"&showall=true&format=xml")
  result[[i]] <- GET(api18, content_type(".xml"))
  xr18 <- content(result[[i]], "text")
  xr.18 <- xmlParse(xr18[[1]])
  xr.list18 <- xmlToList(node = xr.18)
  fips18 <- xr.list18$Block
  as.character(fips18)
  
  # sort out the parts of the block ID
  fips18.1 <- fips18[1] 
  as.data.frame(fips18.1)
  splintered18 <- t(sapply(fips18.1, function(x) substring(x, first = c(1,6,12), last = c(5,11,15)))) 
  
  cbind(splintered18) -> bound18
  
  bound18[[2]] -> tract18 #The second item is the census tract number
  as.integer(tract18)
  try18 <- gsub("(?<![0-9])0+", "", tract18, perl = TRUE)
  try18 <- as.numeric(as.character(try18))
  
  try18 <- try18/100
  try18 -> crime$block[i]
}


#map to schools
school <- read.csv("school_3.csv")
nsch <- nrow(school)
crimetype <- unique(crime$OFFENSE_CODE_GROUP)
crimetype <- crimetype[order(crimetype)]
school_crime <- data.frame(t(rep(0, 60)))
colnames(school_crime) <-crimetype

#select one school name(?)
#sch <- c("Blackstone Elementary")
for(i in 1:nsch){
  sch_lat <- school$Y[i]
  sch_long <- school$X[i]
  sch_crime <- crime[crime$lat < sch_lat + 0.005 & crime$lat > sch_lat - 0.005 &
                       crime$long < sch_long + 0.005 & crime$long > sch_long - 0.005, ]
  sch_crime %>%
    group_by(sch_crime$OFFENSE_CODE_GROUP) %>%
    summarise(count = n()) %>%   
    t() -> sch_crime_n
  school_crime <- rbind(school_crime, as.numeric(sch_crime_n[2, ]))
}  
school_crime <- school_crime[-1, ]
rownames(school_crime) <- school$SCH_LABEL

#PCA
pr.out <- prcomp(school_crime, scale = T, tol = .5)
library(ggfortify)
autoplot(pr.out, data = school_crime, colour = "city",
         loadings = TRUE, loadings.colour = 'light blue',
         loadings.label = TRUE, loadings.label.size = 3)


#aggregate crimes

#Crime Type
crime$Crime_Type <- " "
for (i in 1:80001){
  if(crime$OFFENSE_CODE_GROUP[i] == "Evading Fare" | crime$OFFENSE_CODE_GROUP[i] =="Ballistics"| crime$OFFENSE_CODE_GROUP[i] =="Other Burglary"| crime$OFFENSE_CODE_GROUP[i] =="Fire Related Reports"| crime$OFFENSE_CODE_GROUP[i] =="Commercial Burglary" | crime$OFFENSE_CODE_GROUP[i] == "Violations" | crime$OFFENSE_CODE_GROUP[i] == "Firearm Discovery" | crime$OFFENSE_CODE_GROUP[i] == "Assembly or Gathering Violations" | crime$OFFENSE_CODE_GROUP[i] == "Firearm Violations" | crime$OFFENSE_CODE_GROUP[i] == "Disorderly Conduct" | crime$OFFENSE_CODE_GROUP[i] == "Landlord/Tenant Disputes" | crime$OFFENSE_CODE_GROUP[i] == "Aircraft" | crime$OFFENSE_CODE_GROUP[i] == "License Violation" | crime$OFFENSE_CODE_GROUP[i] == "Explosives") {
    crime$Crime_Type[i] = "Other"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Missing Person Reported" | crime$OFFENSE_CODE_GROUP[i] == "Missing Person Located"){
    crime$Crime_Type[i] = "Missing Person"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Towed" | crime$OFFENSE_CODE_GROUP[i] == "Auto Theft Recovery" | crime$OFFENSE_CODE_GROUP[i] == "License Plate Related Incidents" | crime$OFFENSE_CODE_GROUP[i] == "Auto Theft"){
    crime$Crime_Type[i] = "Auto Vehicle"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Verbal Disputes" | crime$OFFENSE_CODE_GROUP[i] == "Harassment" | crime$OFFENSE_CODE_GROUP[i] == "Liquor Violation" | crime$OFFENSE_CODE_GROUP[i] ==  "Operating Under the Influence" | crime$OFFENSE_CODE_GROUP[i] == "Phone Call Complaints" | crime$OFFENSE_CODE_GROUP[i] == "Prostitution" | crime$OFFENSE_CODE_GROUP[i] == "Disorderly Conduct" ){
    crime$Crime_Type[i] = "Domestic"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Drug Violation"){
    crime$Crime_Type[i] = "Drugs"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Medical Assitance"){
    crime$Crime_Type[i] = "Medical"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Vandalism" | crime$OFFENSE_CODE_GROUP[i] == "Property Related Damage" | crime$OFFENSE_CODE_GROUP[i] == "Burglary- No Property Taken" | crime$OFFENSE_CODE_GROUP[i] == "Larceny" | crime$OFFENSE_CODE_GROUP[i] == "Larceny From Motor Vehicle" | crime$OFFENSE_CODE_GROUP[i] == "Residental Burglary" ){
    crime$Crime_Type[i] = "Nonviolent"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Aggravated Assault" | crime$OFFENSE_CODE_GROUP[i] == "Arson" | crime$OFFENSE_CODE_GROUP[i] == "Offenses Against Child/Family" | crime$OFFENSE_CODE_GROUP[i] == "Criminal Harassment" | crime$OFFENSE_CODE_GROUP[i] == "Biological Threat" | crime$OFFENSE_CODE_GROUP[i] == "HUMAN TRAFFICKING" | crime$OFFENSE_CODE_GROUP[i] == "Restraining Order Violation" | crime$OFFENSE_CODE_GROUP[i] == "Robbery" | crime$OFFENSE_CODE_GROUP[i] == "Home Invastion" | crime$OFFENSE_CODE_GROUP[i] == "Homicide" | crime$OFFENSE_CODE_GROUP[i] == "Manslaughter" | crime$OFFENSE_CODE_GROUP[i] == "HUMAN TRAFFICKING- INVOLUNRARY SERVITUDE") {
    crime$Crime_Type [i] = "Violent"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Confidence Games" | crime$OFFENSE_CODE_GROUP[i] == "Gambling" | crime$OFFENSE_CODE_GROUP[i] == "Fraud" | crime$OFFENSE_CODE_GROUP[i] == "Embezzlement" | crime$OFFENSE_CODE_GROUP[i] == "Counterfiting" ){
    crime$Crime_Type[i] = "Financial"
  }
  else if(crime$OFFENSE_CODE_GROUP[i] == "Other" & crime$OFFENSE_DESCRIPTION[i] == "THREATS TO DO BODILY HARM" | crime$OFFENSE_DESCRIPTION[i] == "INTIMIDATING WITNESS" | crime$OFFENSE_DESCRIPTION[i] == "KIDNAPPING/CUSTODIAL KIDNAPPING" | crime$OFFENSE_DESCRIPTION[i] == "ANIMAL ABUSE" | crime$OFFENSE_DESCRIPTION[i] == "ABDUCTION - INTICING" | crime$OFFENSE_DESCRIPTION[i] == "KIDNAPPING - ENTICING OR ATTEMPTED" | crime$OFFENSE_DESCRIPTION[i] == "CUSTODIAL KIDNAPPING"){
    crime$Crime_Type[i] = "Violent"
  }
  else if(crime$OFFENSE_CODE_GROUP[i] == "Other" & crime$OFFENSE_DESCRIPTION[i] == "TRESPASSING"){
    crime$Crime_Type[i] = "Nonviolent"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Other" & crime$OFFENSE_DESCRIPTION[i] == "VAL - VIOLATION OF AUTO LAW - OTHER" | crime$OFFENSE_DESCRIPTION[i] == "OTHER OFFENSE" | crime$OFFENSE_DESCRIPTION[i] == "DANGEROUS OR HAZARDOUS CONDITION" | crime$OFFENSE_DESCRIPTION[i] == "OBSCENE MATERIALS - PORNOGRAPHY" | crime$OFFENSE_DESCRIPTION[i] == "POSSESSION OF BURGLARIOUS TOOLS" | crime$OFFENSE_DESCRIPTION[i] == "VIOLATION - CITY ORDINANCE" | crime$OFFENSE_DESCRIPTION[i] == "WEAPON - OTHER - OTHER VIOLATION" | crime$OFFENSE_DESCRIPTION[i] == "PROPERTY - CONCEALING LEASED" | crime$OFFENSE_DESCRIPTION[i] == "VIOLATION - CITY ORDINANCE CONSTRUCTION PERMIT" | crime$OFFENSE_DESCRIPTION[i] =="VIOLATION - HAWKER AND PEDDLER"){
    crime$Crime_Type[i] = "Other"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Other" & crime$OFFENSE_DESCRIPTION[i] == "REPORT AFFECTING OTHER DEPTS." | crime$OFFENSE_DESCRIPTION[i] == "INVESTIGATION FOR ANOTHER AGENCY"){
    crime$Crime_Type[i] = "Police Work"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Other" & crime$OFFENSE_DESCRIPTION[i] == "EXTORTION OR BLACKMAIL"){
    crime$Crime_Type[i] = "Domestic"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Simple Assault" & crime$OFFENSE_DESCRIPTION[i] == "ASSAULT SIMPLE - BATTERY" | crime$OFFENSE_DESCRIPTION[i] == "A&B ON POLICE OFFICER" | crime$OFFENSE_DESCRIPTION[i] == "ASSAULT & BATTERY"){
    crime$Crime_Type[i] = "Violent"
  }
  else if (crime$OFFENSE_CODE_GROUP[i] == "Simple Assault" & crime$OFFENSE_DESCRIPTION[i] == "ASSAULT - SIMPLE"){
    crime$Crime_Type[i] = "Nonviolent"
  }
  else{
    crime$Crime_Type[i] = "Police Work"
  }
}

school_crime1 <- data.frame(t(rep(0, 9)))
crimetype1 <- unique(crime$Crime_Type)
crimetype1 <- crimetype1[order(crimetype1)]
colnames(school_crime1) <-crimetype1
for(i in 1:nsch){
  sch_lat <- school$Y[i]
  sch_long <- school$X[i]
  sch_crime <- crime[crime$lat < sch_lat + 0.005 & crime$lat > sch_lat - 0.005 &
                       +                      crime$long < sch_long + 0.005 & crime$long > sch_long - 0.005, ]
  sch_crime %>%
    group_by(sch_crime$Crime_Type) %>%
    summarise(count = n()) %>%   
    t() -> sch_crime_n
  school_crime1 <- rbind(school_crime1, as.numeric(sch_crime_n[2, ]))
}
school_crime1 <- school_crime1[-1, ]
rownames(school_crime1) <- school$SCH_LABEL

school4 <- read.csv("school_4.csv")
schoolinfo <- school4[, c(2,3,8,9,14,15,20,25,43,68,84,101)]
finaldata <- left_join(school_crime1[,c(2,3,5,9,10)], schoolinfo, by="SCH_LABEL")
colnames(finaldata)[c(13:16)] <- c("Dropout", "Science", "Math", "ELA") 
finaldata$Math <- round(finaldata$Math * 100/560, 1)
finaldata$ELA <- round(finaldata$ELA * 100/560, 1)

#crimedata
crime_4group <- crime[crime$Crime_Type %in% c("Domestic", "Drugs", "Missing Person", "Violent"), ]
crime_4group <- crime_4group[crime_4group$lat > 0,]
crime_4group <- crime_4group[ ,c(1:4,6,5,7)]
save(file = "BPS-Crime/crimedata.RData", crime_4group)