#create model
# package required: "e1071"
#install.packages("e1071")
library(e1071)

#set.seed(666): errors                = [1] 0.6258192 0.6216548 0.5988531 0.5789186 0.5448525 0.5387766 0.4926952 0.5164528 0.5023211 0.4842982 0.5708629
#               errors_100_alternates = [1] 0.6245221 0.6304615 0.6345576 0.6355134 0.8517204 0.9159612 0.8296696 0.9498225 0.8780038 0.9707810 0.9434052

#set.seed(555): errors                = [1] 0.6202895 0.6258192 0.5972829 0.5734571 0.5508602 0.5322228 0.4980202 0.5174085 0.5010240 0.4786319 0.5697706
#               errors_100_alternates = [1] 0.6224058 0.6268433 0.6355134 0.6359913 0.8472829 0.8300109 0.8213408 0.9478427 0.8765702 0.9702348 0.9432004

#set.seed(111): errors                = [1] 0.6151010 0.6101857 0.5900464 0.5647870 0.5430776 0.5278536 0.4869607 0.5155653 0.4952895 0.4824549 0.5593938
#               errors_100_alternates = [1] 0.6151010 0.6172856 0.6227471 0.6300519 0.8506281 0.8281677 0.8182004 0.9468187 0.8744539 0.9731704 0.9429274
set.seed(666)
train_rows = sample(nrow(clean_data),nrow(clean_data)/2)
train = clean_data[train_rows,]
test = clean_data[-train_rows,]


model_100 <- naiveBayes(Length.Round ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_100 <- predict(model_100,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors = (1 - (sum(diag(tabl_pred_100))/dim(test)[1]))  #0.615 



model_200 <- naiveBayes(Length.Round.200 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_200 <- predict(model_200,test,type="class")
tabl_pred_200 = table(pred_200,test$Length.Round.200)
errors[2] = (1 - (sum(diag(tabl_pred_200))/dim(test)[1])) #0.6134

model_300 <- naiveBayes(Length.Round.300 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_300 <- predict(model_300,test,type="class")
tabl_pred_300 = table(pred_300,test$Length.Round.300)
errors[3] = (1 - (sum(diag(tabl_pred_300))/dim(test)[1])) #0.595

model_400 <- naiveBayes(Length.Round.400 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_400 <- predict(model_400,test,type="class")
tabl_pred_400 = table(pred_400,test$Length.Round.400)
errors[4] = (1 - (sum(diag(tabl_pred_400))/dim(test)[1])) #0.5716

model_500 <- naiveBayes(Length.Round.500 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_500 <- predict(model_500,test,type="class")
tabl_pred_500 = table(pred_500,test$Length.Round.500)
errors[5] = (1 - (sum(diag(tabl_pred_500))/dim(test)[1])) #0.5467

model_600 <- naiveBayes(Length.Round.600 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_600 <- predict(model_600,test,type="class")
tabl_pred_600 = table(pred_600,test$Length.Round.600)
errors[6] = (1 - (sum(diag(tabl_pred_600))/dim(test)[1])) #0.538

model_700 <- naiveBayes(Length.Round.700 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_700 <- predict(model_700,test,type="class")
tabl_pred_700 = table(pred_700,test$Length.Round.700)
errors[7] = (1 - (sum(diag(tabl_pred_700))/dim(test)[1])) #0.498

model_800 <- naiveBayes(Length.Round.800 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_800 <- predict(model_800,test,type="class")
tabl_pred_800 = table(pred_800,test$Length.Round.800)
errors[8] = (1 - (sum(diag(tabl_pred_800))/dim(test)[1]))  #0.519

model_900 <- naiveBayes(Length.Round.900 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_900 <- predict(model_900,test,type="class")
tabl_pred_900 = table(pred_900,test$Length.Round.900)
errors[9] = (1 - (sum(diag(tabl_pred_900))/dim(test)[1])) #0.508

# error rate
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_1000 <- predict(model_1000,test,type="class")
tabl_pred_1000 = table(pred_1000,test$Length.Round.1000)
errors[10] = (1 - (sum(diag(tabl_pred_1000))/dim(test)[1]))     #0.489

# custom buckets model
model_custom_buckets <- naiveBayes(Custom.Bucket ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train)
pred_custom_buckets <- predict(model_custom_buckets,test,type="class")
tabl_pred_custom_buckets = table(pred_custom_buckets,test$Custom.Bucket)
errors[11] = (1 - (sum(diag(tabl_pred_custom_buckets))/dim(test)[1])) #0.563


# round to 100 buckets alternates

model_100_minus_public_assistance <- naiveBayes(Length.Round ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program, data=train)
pred_100 <- predict(model_100_minus_public_assistance,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates = (1 - (sum(diag(tabl_pred_100))/dim(test)[1]))   #0.616

model_100_minus_public_assistance_minus_household <- naiveBayes(Length.Round ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program, data=train)
pred_100 <- predict(model_100_minus_public_assistance_minus_household,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[2] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) #0.62

model_100_minus_public_assistance_minus_household_minus_individual <- naiveBayes(Length.Round ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type, data=train)
pred_100 <- predict(model_100_minus_public_assistance_minus_household_minus_individual,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[3] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) #0.63

model_100_minus_public_assistance_minus_household_minus_individual_minus_hazard <- naiveBayes(Length.Round ~Declaration.Type+State+County+Start.Year+Disaster.Type, data=train)
pred_100 <- predict(model_100_minus_public_assistance_minus_household_minus_individual_minus_hazard,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[4] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) #0.6249

model_100_declarationtype_state_county_disastertype <- naiveBayes(Length.Round ~Declaration.Type+State+County+Disaster.Type, data=train)
pred_100 <- predict(model_100_declarationtype_state_county_disastertype,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[5] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) 

model_100_declarationtype_state_county <- naiveBayes(Length.Round ~Declaration.Type+State+County, data=train)
pred_100 <- predict(model_100_minus_public_assistance_minus_household_minus_individual_minus_hazard_minus_start_year_minus_disaster_type,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[6] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) 

model_100_declarationtype_state <- naiveBayes(Length.Round ~Declaration.Type+State, data=train)
pred_100 <- predict(model_100_minus_public_assistance_minus_household_minus_individual_minus_hazard_minus_start_year_minus_disaster_type_minus_county,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[7] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1]))

model_100_state_county <- naiveBayes(Length.Round ~State+County, data=train)
pred_100 <- predict(model_100_state_county,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[8] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) 

model_100_state_only <- naiveBayes(Length.Round ~State, data=train)
pred_100 <- predict(model_100_state_only,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[9] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1]))

model_100_county_only <- naiveBayes(Length.Round ~County, data=train)
pred_100 <- predict(model_100_county_only,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[10] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) 

model_100_county_disaster_type <- naiveBayes(Length.Round ~Disaster.Type+County, data=train)
pred_100 <- predict(model_100_county_disaster_type,test,type="class")
tabl_pred_100 = table(pred_100,test$Length.Round)
errors_100_alternates[11] = (1 - (sum(diag(tabl_pred_100))/dim(test)[1])) 
