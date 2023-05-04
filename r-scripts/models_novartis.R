
rm(list = ls())

setwd("~/novartis")

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(broom)
library(tidyr)

library(caret)
library(rpart.plot)

data <- read_xlsx("wip_1519_mod_R.xlsx", sheet = 3, trim_ws = TRUE)

data$P_NumNights <- data$P_NumNights %>%
  str_replace_all("\\D", "0") %>% # cambio tutto ciò che non è numero con 0
  str_replace_na("0") %>% # cambio NA con 0
  as.numeric() # converto in numeric

data <- data %>% 
  filter(anno != 2015, # tolgo 2015
         P_Totale > 0, # tolgo totale preventivo 0 o inferiore
         C_Totale > 0) %>% # tolgo totale consuntivo 0 o inferiore
  select(P_NumNights:P_SC, C_Totale, -P_Totale, -P_NumRoomnigths) # tengo solo quantitative

data %>% map_lgl(is.numeric) %>% table() # check che siano tutte variabili numeriche
str(data) 


## Forecasting

# Train e Test Set
# Resample index
set.seed(1234)
trainIndex <- createDataPartition(1:nrow(data), p = .8, list = FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

# Setting up cross-validation
cvcontrol <- trainControl(method = "repeatedcv", number = 10, allowParallel = TRUE)


## Models
# Decision Tree
modelLookup("rpart")
tree <- train(C_Totale ~ ., 
              data = train,
              method = "rpart", 
              trControl = cvcontrol, 
              tuneLength = 10)
tree
plot(tree)
plot(tree$finalModel, main = "Regression Tree for Carseat High Sales")
rpart.plot(tree$finalModel, main = "Decision Tree Model")

tree_pred <- predict(tree, newdata = test, type = "raw")

# Bagging
modelLookup("treebag")
bagg <- train(C_Totale ~ ., 
              data = train, 
              method = "treebag", 
              trControl = cvcontrol, 
              importance = TRUE)
bagg
plot(varImp(bagg))

bagg_pred <- predict(bagg, newdata = test, type = "raw")

# Random Forest
modelLookup("rf")
rf <- train(C_Totale ~ ., 
            data = train, 
            method = "rf", 
            trControl = cvcontrol, 
            importance = TRUE)
rf
plot(varImp(rf), main = "Random Forest Model")

rf_pred <- predict(rf, newdata = test, type = "raw")

# Random Forest (Ranger)
modelLookup("ranger")
rfr <- train(C_Totale ~ ., 
             data = train, 
             method = "ranger", 
             trControl = cvcontrol, 
             importance = "impurity")
rfr
plot(varImp(rfr))

rfr_pred <- predict(rfr, newdata = test, type = "raw")

# Gradient Boosting
modelLookup("gbm")
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = c(0.01, 0.1),
                        n.minobsinnode = c(10, 20))

gbm <- train(C_Totale ~ ., 
             data = train, 
             method = "gbm", 
             trControl = cvcontrol,
             tuneGrid = gbmGrid,
             verbose = FALSE)
gbm
plot(gbm, main = "Gradient Boosting Model")

gbm_pred <- predict(gbm, newdata = test, type = "raw")

# XGBoost
modelLookup("xgbTree")
xgbGrid <-  expand.grid(nrounds = c(100, 500, 1000), 
                        max_depth = c(4, 5, 6, 7, 8, 9, 10), 
                        eta = c(0.1, 0.2, 0.3, 0.4, 0.5),
                        gamma = c(10, 20), 
                        colsample_bytree = c(1),
                        min_child_weight = c(1),
                        subsample = c(1))

xgb <- train(C_Totale ~ ., 
             data = train, 
             method = "xgbTree", 
             trControl = cvcontrol,
             tuneGrid = xgbGrid,
             verbose = FALSE)
xgb
plot(xgb)

xgb_pred <- predict(xgb, newdata = test, type = "raw")

# Generalized Linear Model Stepwise AIC
modelLookup("glmStepAIC")
glmStep <- train(C_Totale ~ ., 
                 data = train, 
                 method = "glmStepAIC", 
                 trControl = cvcontrol,
                 verbose = FALSE)
glmStep

glmStep_pred <- predict(glmStep, newdata = test, type = "raw")

# Boosted Generalized Linear Model
modelLookup("glmboost")
glmBoost <- train(C_Totale ~ ., 
                  data = train, 
                  method = "glmboost", 
                  trControl = cvcontrol)
glmBoost
plot(glmBoost)

glmBoost_pred <- predict(glmBoost, newdata = test, type = "raw")

# Neural Network
modelLookup("nnet")
nnetGrid <-  expand.grid(size = c(0, 1, 2, 3),
                         decay = c(0, 0.0005, 0.005))

nnet <- train(C_Totale ~ ., 
              data = train, 
              method = "nnet", 
              trControl = cvcontrol,
              tuneGrid = nnetGrid)
nnet
plot(nnet)

nnet_pred <- predict(nnet, newdata = test, type = "raw")

# Bayesian Neural Network
modelLookup("brnn")
brnn <- train(C_Totale ~ ., 
              data = train, 
              method = "brnn", 
              trControl = cvcontrol,
              verbose = FALSE)
brnn
plot(brnn)

brnn_pred <- predict(brnn, newdata = test, type = "raw")



## Results
df_res <- data.frame(C_Totale = test$C_Totale, 
                     Regression_Tree = tree_pred, 
                     Bagging = bagg_pred,
                     Random_Forest = rf_pred,
                     Ranger = rfr_pred,
                     Gradient_Boosting = gbm_pred,
                     Xtreme_Gradient_Boosting = xgb_pred, 
                     Generalized_Linear_Model = glmStep_pred, 
                     Boosted_GLM = glmBoost_pred,
                     #NeuralNet_pred = nnet_pred,
                     Bayesian_Neural_Network = brnn_pred)

# Out-of-sample Performance Evaluation
evaluation <- function(pred, obs) {
  
  abs <- abs(pred - obs)
  sd <- sd(abs)
  mae <- MAE(pred, obs)
  upp <- mae + sd*0.25
  low <- mae - sd*0.25
  
  res <- c(MAE = mae, Upper = upp, Lower = low)
  
  return(res)
  
}

eval <- df_res[-93,] %>%
  map(~evaluation(pred = ., obs = df_res[-93,]$C_Totale)) %>%
  transpose() %>%
  map_df(as_tibble) %>%
  t() %>%
  as.data.frame()
names(eval) <- c("MAE", "Upper", "Lower")
eval <- eval[-1,]
eval$Model <- rownames(eval) %>% as.factor()
rownames(eval) <- NULL

library(ggplot2)
library(ggrepel)
ggplot(eval, aes(x = Model, y = MAE, col = Model)) +
  geom_point(size = 3) +
  geom_text(aes(label = trunc(MAE)), hjust = 1.1, vjust = 0) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, col = Model), width = .2, show.legend = FALSE) +
  geom_label_repel(data = subset(eval, MAE == min(MAE)), aes(label = "Best Model"), 
                   box.padding = 0.35, point.padding = 0.5, segment.color = "grey50",
                   show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Out-of-Sample Performance Evaluation")

df_res[-93,] %>%
  select(C_Totale, Ranger) %>%
  mutate(ID = 1:nrow(.)) %>%
  gather(key = "Type", value = "Value", -ID) %>%
ggplot(aes(x = ID, y = Value, col = Type)) +
  geom_line(alpha = .6) +
  geom_point(alpha = .8, size = 1) + 
  labs(title = "Best Model's Performance by Events", subtitle = "Actual Values vs Forecasted Values") +
  xlab("Events") + ylab("Total Costs") +
  scale_color_discrete(name = "Events", labels = c("Actual", "Forecast"))



# ##########------------------------------------------------------############-
# #h2o - AutoML
# load("train.rda"); load("test.rda")
# library(h2o)
# localH2O <- h2o.init()
# train_h2o <- as.h2o(train)
# test_h2o <- as.h2o(test)
# 
# automl <- h2o.automl(x = setdiff(names(train_h2o), "C_Totale"),
#                      y = "C_Totale",
#                      training_frame = train_h2o,
#                      nfolds = 10)
# 
# str(automl)
# automl@leader@model
# plot(automl@leader)
# 
# h2o.partialPlot(automl@leader@model, data = train_h2o, cols = c("P_Hotel", "P_FoodBeverage"))
# 
# 
# automl_pred <- h2o.predict(automl, test_h2o)
# 
# 
# save(automl, file = "automl.rda")
# ##########------------------------------------------------------############-



## Inference
# Linear Regression Model
lm <- lm(C_Totale ~ ., data)
summary(lm)

xreg <- c("P_NumNights", "P_NumPartecipanti", "P_AssLogistica", "P_AvAssistenzaTecnica",
          "P_Allestimenti", "P_Iscrizioni", "P_Hotel", "P_SaleMeeting", "P_Viaggi",
          "P_FoodBeverage", "P_SC")
formula <- as.formula(paste0("C_Totale ~ -1 + ", paste(xreg, collapse = " + ")))
lmOpt <- lm(formula, data)
summary(lmOpt)

# Problems?!
plot(1:length(lmOpt$residuals), lmOpt$residuals)
qqnorm(lmOpt$residuals)
shapiro.test(lmOpt$residuals)
# attenzione perchè le osservazioni solo solo positive


library(effects)
effects::Effect.lm(lmOpt)

e <- effects::predictorEffects(lmOpt)
plot(e)
plot(predictorEffects(lmOpt, partial.residuals = TRUE),
     partial.residual = list(pch = ".", col = "#FF00FF80"),
     axes = list(x = list(rotate = 45)),
     rows = 4, cols = 3)

plot(predictorEffects(lmOpt, partial.residuals = TRUE, 
                      predictors = c("P_Hotel", "P_FoodBeverage", "P_SaleMeeting")),
     partial.residual = list(pch = ".", col = "#FF00FF80"),
     axes = list(x = list(rotate = 45)),
     rows = 1, cols = 3, 
     ylim = c(0, 200000), ylab = "Total Costs")

lm_pred <- predict(lmOpt, newdata = test)
evaluation(lm_pred, test$C_Totale)





##########------------------------------------------------------############-
#h2o - AutoML
load("train.rda"); load("test.rda")
library(h2o)
localH2O <- h2o.init()
train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)

automl <- h2o.automl(x = setdiff(names(train_h2o), "C_Totale"),
                     y = "C_Totale",
                     training_frame = train_h2o,
                     nfolds = 10)

str(automl)
automl@leader@model
plot(automl@leader)

h2o.partialPlot(automl@leader@model, data = train_h2o, cols = c("P_Hotel", "P_FoodBeverage"))


automl_pred <- h2o.predict(automl, test_h2o)


save(automl, file = "automl.rda")
##########------------------------------------------------------############-

