set.seed(666)
constant.column.error.rate <- function(column.name, value) {
  isolated.only.rows = clean_data[clean_data[,as.character(column.name)] == value,]
  train_rows = sample(nrow(isolated.only.rows),nrow(isolated.only.rows)/2)
  train = isolated.only.rows[train_rows,]
  test = isolated.only.rows[-train_rows,]
  
  
  model_1000 <- naiveBayes(Length.Round ~Declaration.Type+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
  pred_1000 <- predict(model_1000,test,type="class")
  tabl_pred_1000 = table(pred_1000,test$Length.Round)
  (1 - (sum(diag(tabl_pred_1000))/dim(test)[1]))
}


state.freq = as.data.frame(table(clean_data$State))
state.freq[order(state.freq$Freq),]
head(state.freq,n=5)

disasters = as.data.frame(table(clean_data$Disaster.Type))
disasters.sorted = disasters[order(-disasters$Freq),]
head(disasters.sorted,n=5)

