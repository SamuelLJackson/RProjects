###############################################################
# K   F O L D   C R O S S   V A L I D A T I O N ###############
###############################################################

#install.packages("e1071")
#library(e1071)
###############################################################
# C R E A T E   P A R T I T I O N S ###########################
###############################################################
set.seed(666)

partition.size = floor(dim(clean_data)[1]/10)
partition.one.rows = (sample(nrow(clean_data),partition.size))
partition.one = (clean_data[partition.one.rows,])
leftovers = clean_data[-partition.one.rows,]

partition.two.rows = (sample(nrow(leftovers),partition.size))
partition.two = leftovers[partition.two.rows,]
leftovers = leftovers[-partition.two.rows,]

partition.three.rows = (sample(nrow(leftovers),partition.size))
partition.three = leftovers[partition.three.rows,]
leftovers = leftovers[-partition.three.rows,]

partition.four.rows = (sample(nrow(leftovers),partition.size))
partition.four = leftovers[partition.four.rows,]
leftovers = leftovers[-partition.four.rows,]

partition.five.rows = (sample(nrow(leftovers),partition.size))
partition.five = leftovers[partition.five.rows,]
leftovers = leftovers[-partition.five.rows,]

partition.six.rows = (sample(nrow(leftovers),partition.size))
partition.six = leftovers[partition.six.rows,]
leftovers = leftovers[-partition.six.rows,]

partition.seven.rows = (sample(nrow(leftovers),partition.size))
partition.seven = leftovers[partition.seven.rows,]
leftovers = leftovers[-partition.seven.rows,]

partition.eight.rows = (sample(nrow(leftovers),partition.size))
partition.eight = leftovers[partition.eight.rows,]
leftovers = leftovers[-partition.eight.rows,]

partition.nine.rows = (sample(nrow(leftovers),partition.size))
partition.nine = leftovers[partition.nine.rows,]
leftovers = leftovers[-partition.nine.rows,]

partition.ten.rows = (sample(nrow(leftovers),partition.size))
partition.ten = leftovers[partition.ten.rows,]
leftovers = leftovers[-partition.ten.rows,]


##############################################
# A P P L Y   M O D E L ######################
##############################################


train.one = clean_data[-partition.one.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.one)
pred_100 <- predict(model_1000,partition.one,type="class")
tabl_pred_100 = table(pred_100,partition.one$Length.Round.1000)
cv.errors = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.two = clean_data[-partition.two.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.two)
pred_100 <- predict(model_1000,partition.two,type="class")
tabl_pred_100 = table(pred_100,partition.two$Length.Round.1000)
cv.errors[2] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.three = clean_data[-partition.three.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.three)
pred_100 <- predict(model_1000,partition.three,type="class")
tabl_pred_100 = table(pred_100,partition.three$Length.Round.1000)
cv.errors[3] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.four = clean_data[-partition.four.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.four)
pred_100 <- predict(model_1000,partition.four,type="class")
tabl_pred_100 = table(pred_100,partition.four$Length.Round.1000)
cv.errors[4] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.five = clean_data[-partition.five.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.five)
pred_100 <- predict(model_1000,partition.five,type="class")
tabl_pred_100 = table(pred_100,partition.five$Length.Round.1000)
cv.errors[5] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.six = clean_data[-partition.six.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.six)
pred_100 <- predict(model_1000,partition.six,type="class")
tabl_pred_100 = table(pred_100,partition.six$Length.Round.1000)
cv.errors[6] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.seven = clean_data[-partition.seven.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.seven)
pred_100 <- predict(model_1000,partition.seven,type="class")
tabl_pred_100 = table(pred_100,partition.seven$Length.Round.1000)
cv.errors[7] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.eight = clean_data[-partition.eight.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.eight)
pred_100 <- predict(model_1000,partition.eight,type="class")
tabl_pred_100 = table(pred_100,partition.eight$Length.Round.1000)
cv.errors[8] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.nine = clean_data[-partition.nine.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.nine)
pred_100 <- predict(model_1000,partition.nine,type="class")
tabl_pred_100 = table(pred_100,partition.nine$Length.Round.1000)
cv.errors[9] = (1 - (sum(diag(tabl_pred_100))/partition.size))

train.ten = clean_data[-partition.ten.rows,]
model_1000 <- naiveBayes(Length.Round.1000 ~Declaration.Type+State+County+Hazard.Mitigation.Program+Start.Year+Disaster.Type+Individual.Assistance.Program+Individuals...Households.Program+Public.Assistance.Program, data=train.ten)
pred_100 <- predict(model_1000,partition.ten,type="class")
tabl_pred_100 = table(pred_100,partition.ten$Length.Round.1000)
cv.errors[10] = (1 - (sum(diag(tabl_pred_100))/partition.size))

#m[m[, "three"] == 11,]