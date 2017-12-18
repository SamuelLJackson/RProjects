set.seed(666)
constant.column.error.rate <- function(column.name, value) {
  isolated.only.rows = clean_data[clean_data[,as.character(column.name)] == value,]
  train_rows = sample(nrow(isolated.only.rows),nrow(isolated.only.rows)/2)
  train = isolated.only.rows[train_rows,]
  test = isolated.only.rows[-train_rows,]
  
  model_1000 <- naiveBayes(Length.Round ~Declaration.Type+County+Hazard.Mitigation.Program+Disaster.Type+Start.Year+State+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
  pred_1000 <- predict(model_1000,test,type="class")
  tabl_pred_1000 = table(pred_1000,test$Length.Round)
  (1 - (sum(diag(tabl_pred_1000))/dim(test)[1]))
}

get.mean <- function(column.name,value) {
  isolated.rows = clean_data[clean_data[,as.character(column.name)] == value,]
  
  sum.value = 0;
  zero.count = 0;
  for (row in 1:nrow(isolated.rows)) {
    length.round <- as.integer(as.character(isolated.rows[row,"Length.Round"]));
    sum.value <- sum.value + length.round;
  }
  mean.value <- sum.value/(dim(isolated.rows)[1])
  mean.value
}

get.range <- function(column.name,value) {
  isolated.rows = clean_data[clean_data[,as.character(column.name)] == value,]
  range = range(as.integer(as.character(isolated.rows$Length.Round)))
  range
}
state.freq = as.data.frame(table(clean_data$State))
state.freq = state.freq[order(-state.freq$Freq),]
state.freq = state.freq[1:10,]
state.freq$mean.length <- 0
state.freq$range.from <-0
state.freq$range.to <- 0
state.freq$round.100.error <- 0
for (row in 1:20) {
  state.name = state.freq[row,1]
  state.freq[row,6] = constant.column.error.rate("State",state.name)
  state.freq[row,3] = get.mean("State",state.name)
  state.freq[row,4] = get.range("State",state.name)[1]
  state.freq[row,5] = get.range("State",state.name)[2]
}
disaster.freq = as.data.frame(table(clean_data$Disaster.Type))
disaster.freq = disaster.freq[order(-disaster.freq$Freq),]
disaster.freq = disaster.freq[1:10,]
disaster.freq$mean.length <- 0
disaster.freq$range.from <-0
disaster.freq$range.to <- 0
disaster.freq$round.100.error <- 0
for (row in 1:20) {
  disaster.name = disaster.freq[row,1]
  disaster.freq[row,6] = constant.column.error.rate("Disaster.Type",disaster.name)
  disaster.freq[row,3] = get.mean("Disaster.Type",disaster.name)
  disaster.freq[row,4] = get.range("Disaster.Type",disaster.name)[1]
  disaster.freq[row,5] = get.range("Disaster.Type",disaster.name)[2]
}

start.year.freq = as.data.frame(table(clean_data$Start.Year))
start.year.freq = start.year.freq[order(-start.year.freq$Freq),]
start.year.freq = start.year.freq[1:20,]
start.year.freq$mean.length <- 0
start.year.freq$range.from <-0
start.year.freq$range.to <- 0
start.year.freq$round.100.error <- 0
for (row in 1:20) {
  start.year.name = start.year.freq[row,1]
  start.year.freq[row,6] = constant.column.error.rate("Start.Year",start.year.name)
  start.year.freq[row,3] = get.mean("Start.Year",start.year.name)
  start.year.freq[row,4] = get.range("Start.Year",start.year.name)[1]
  start.year.freq[row,5] = get.range("Start.Year",start.year.name)[2]
}

declaration.freq = as.data.frame(table(clean_data$Declaration.Type))
declaration.freq = declaration.freq[order(-declaration.freq$Freq),]
declaration.freq = declaration.freq[1:2,]
declaration.freq$mean.length <- 0
declaration.freq$range.from <-0
declaration.freq$range.to <- 0
declaration.freq$round.100.error <- 0
for (row in 1:2) {
  declaration.name = declaration.freq[row,1]
  declaration.freq[row,6] = constant.column.error.rate("Declaration.Type",declaration.name)
  declaration.freq[row,3] = get.mean("Declaration.Type",declaration.name)
  declaration.freq[row,4] = get.range("Declaration.Type",declaration.name)[1]
  declaration.freq[row,5] = get.range("Declaration.Type",declaration.name)[2]
}