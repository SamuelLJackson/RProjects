mydata = read.csv("database.csv")
#remove all entries missing county data, and those disasters which are still active aka missing close date data
clean_data = mydata[!(mydata$County=="" | mydata$Close.Date==""),]

#some of the types only appeared less than 10 times, so I've added them to the 'Other' category
unique.types = as.data.frame(table(clean_data$Disaster.Type))
unique.types.less.10 = unique.types[(unique.types$Freq<10),]
clean_data$Disaster.Type[clean_data$Disaster.Type %in% unique.types.less.10$Var1] <- "Other"

#Find the number of days the state of disaster lasted
clean_data$date_diff <- as.Date(as.character(clean_data$Close.Date), format="%m/%d/%Y") - as.Date(as.character(clean_data$Start.Date), format="%m/%d/%Y")
#make sure it is a number, not a date object
clean_data$date_diff <- as.numeric(clean_data$date_diff)
#get the year the disaster was declared
clean_data$Start.Year <- factor(substr(as.character(clean_data$Start.Date),7,10))
#create the column to be predicted: a rounding to 100 of number of days of disaster

clean_data$Length.Round <- factor(100 * round(as.numeric(clean_data$date_diff)/100))
clean_data$Length.Round.200 <- factor(200 * round(as.numeric(clean_data$date_diff)/200))
clean_data$Length.Round.300 <- factor(300 * round(as.numeric(clean_data$date_diff)/300))
clean_data$Length.Round.400 <- factor(400 * round(as.numeric(clean_data$date_diff)/400))
clean_data$Length.Round.500 <- factor(500 * round(as.numeric(clean_data$date_diff)/500))
clean_data$Length.Round.600 <- factor(600 * round(as.numeric(clean_data$date_diff)/600))
clean_data$Length.Round.700 <- factor(700 * round(as.numeric(clean_data$date_diff)/700))
clean_data$Length.Round.800 <- factor(800 * round(as.numeric(clean_data$date_diff)/800))
clean_data$Length.Round.900 <- factor(900 * round(as.numeric(clean_data$date_diff)/900))
clean_data$Length.Round.1000 <- factor(1000 * round(as.numeric(clean_data$date_diff)/1000))



########################################################################################
# C R E A T E   R O U N D E D   V A L U E   F O R   D A Y   L E N G T H ################
########################################################################################

#some day lengths are too infrequent to be included ( those appearing less than 10 times)
unique.lengths = as.data.frame(table(clean_data$Length.Round))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round %in% unique.lengths.greater.10$Var1),]

#for 200 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.200))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.200 %in% unique.lengths.greater.10$Var1),]

#for 300 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.300))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.300 %in% unique.lengths.greater.10$Var1),]

#for 400 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.400))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.400 %in% unique.lengths.greater.10$Var1),]

#for 500 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.500))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.500 %in% unique.lengths.greater.10$Var1),]

#for 600 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.600))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.600 %in% unique.lengths.greater.10$Var1),]

#for 700 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.700))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.700 %in% unique.lengths.greater.10$Var1),]

#for 800 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.800))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.800 %in% unique.lengths.greater.10$Var1),]

#for 900 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.900))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.900 %in% unique.lengths.greater.10$Var1),]

#for 1000 round
unique.lengths = as.data.frame(table(clean_data$Length.Round.1000))
unique.lengths.greater.10 = unique.lengths[(unique.lengths$Freq>10),]
clean_data = clean_data[(clean_data$Length.Round.1000 %in% unique.lengths.greater.10$Var1),]

############################################################################################
# C R E A T E   C U S T O M   B U C K E T S   O F   E Q U A L   S I Z E ####################
############################################################################################

#the method of determining the approximately same size buckets is in the 'create_buckets.r'
f <- function(length) {
  length = as.character(length)
  length = as.numeric(length)

  if (length>4400) {
     "4400-8700";
  } else if (length>as.integer(3800)) {
    "3800-4400"
  } else if (length>as.integer(3300)) {
    "3300-3800"
  } else if (length>as.integer(3000)) {
    "3000-3300"
  } else if (length>as.integer(2700)) {
    "2700-3000"
  } else if (length>as.integer(2400)) {
    "2400-2700"
  } else if (length>as.integer(2100)) {
    "2100-2400"
  } else if (length>as.integer(1700)) {
    "1700-2100"
  } else if (length>as.integer(1300)) {
    "1300-1700"
  } else if (length>as.integer(1000)) {
    "1000-1300"
  } else if (length>as.integer(700)) {
    "700-1000"
  } else {
    "0-700"
  }
}
clean_data$Custom.Bucket <- 0
for (row in 1:nrow(clean_data)) {
    round_year <- clean_data[row,"Length.Round"]
    bucket <- f(round_year);
    clean_data[row,"Custom.Bucket"] <- bucket;
}
clean_data$Custom.Bucket <- factor(clean_data$Custom.Bucket)

#############################################################################################
# F I N A L   C L E A N U P #################################################################
#############################################################################################




#get rid of extraneous columns
clean_data = clean_data[ , !(names(clean_data) %in% c("Declaration.Number","date_diff","End.Date","Disaster.Title","Declaration.Date","Close.Date","Start.Date"))]


#some of the counties appear less than 10 times, remove those
unique.counties = as.data.frame(table(clean_data$County))
unique.counties.greater.10 = unique.counties[(unique.counties$Freq>10),]
clean_data = clean_data[(clean_data$County %in% unique.counties.greater.10$Var1),]

#some of the states appear less than 10 times, remove those
unique.states = as.data.frame(table(clean_data$State))
unique.states.greater.10 = unique.states[(unique.states$Freq>10),]
clean_data = clean_data[(clean_data$State %in% unique.states.greater.10$Var1),]