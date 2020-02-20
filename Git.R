# Library Import ----------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(xlsx)
library(readxl)
library(ggpubr)
library(openxlsx)
library(ROCR)
library(pROC)
library(caret)
library(corrplot)			
library(tictoc)
library(janitor)
library(DMwR)


# Data Transformations Benign vs. Malignant -----------------------------------------------------


exp1 <- read_excel("data.xlsx") 


exp1 <- exp1 %>%
  select(Tumor:Kep)%>%
  glimpse()


col_vector_factor <- c(1)
col_vector_numeric <- c(2:6)

exp1[, col_vector_factor] <- lapply(exp1[, col_vector_factor], as.factor)
exp1[, col_vector_numeric] <- lapply(exp1[, col_vector_factor], numeric)
glimpse(exp1)



exp1$Tumor <- ifelse(exp1$Tumor == 1, "Yes", "No")
exp1$Tumor <- as.factor(exp1$Tumor)



# Data Transformation Low_vs_High -----------------------------------------


exp2 <- read_excel("data.xlsx") 

exp2 <- exp2 %>%
  filter(Gleason_sum == 6 | Gleason_sum == 8 | Gleason_sum == 9 | Gleason_sum == 10)

col_vector_factor <- c(1, 7:10)
col_vector_numeric <- c(2:6)

exp2[, col_vector_factor] <- lapply(exp2[, col_vector_factor], as.factor)
exp2[, col_vector_numeric] <- lapply(exp2[, col_vector_factor], numeric)




exp2 <- exp2 %>%
  mutate(
    tumor = case_when(
      Gleason_sum == 6 ~ "0",
      TRUE ~ "1"
    )
  )


exp2$tumor <- ifelse(exp2$tumor == 1, "Yes", "No")
exp2$tumor <- as.factor(exp2$tumor)


exp2 <- exp2 %>%
  select(T2:Kep, tumor)%>%
  glimpse()



# Data Transformation Low_vs_Intermediate ---------------------------------

exp3 <- read_excel("data.xlsx") 
exp3 <- exp3 %>%
  filter(Gleason_sum == 6 | Gleason_sum == 7)

exp3[, col_vector_factor] <- lapply(exp3[, col_vector_factor], as.factor)
exp3[, col_vector_numeric] <- lapply(exp3[, col_vector_factor], numeric)
glimpse(exp3)

exp3 <- exp3 %>%
  mutate(
    tumor = case_when(
      Gleason_sum == 6 ~ "0",
      TRUE ~ "1"
    )
  )

exp3$tumor <- ifelse(exp3$tumor == 1, "Yes", "No")
exp3$tumor <- as.factor(exp3$tumor)


exp3 <- exp3 %>%
  select(T2:Kep, tumor)%>%
  glimpse()



# Data Transformation Intermediate_vs_High --------------------------------



exp4 <- read_excel("data.xlsx") 
exp4 <- exp4 %>%
  filter(Gleason_sum >= 7)

exp4[, col_vector_factor] <- lapply(exp4[, col_vector_factor], as.factor)
exp4[, col_vector_numeric] <- lapply(exp4[, col_vector_factor], numeric)
glimpse(exp4)

exp4 <- exp4 %>%
  mutate(
    tumor = case_when(
      Gleason_sum == 7 ~ "0",
      TRUE ~ "1"
    )
  )


exp4$tumor <- ifelse(exp4$tumor == 1, "Yes", "No")
exp4$tumor <- as.factor(exp4$tumor)


exp4 <- exp4 %>%
  select(T2:Kep, tumor)%>%
  glimpse()


# Data Splittings ----------------------------------------------------------


set.seed(42)


#EXP1


trainIndex_exp1 <- createDataPartition(exp1$Tumor, p = 0.75, list = FALSE)

head(trainIndex_exp1)

tumortrain_exp1 <- exp1[trainIndex_exp1,]
tumortest_exp1 <- exp1[-trainIndex_exp1,]


tumortrain_noperf_exp1 <- tumortrain_exp1 %>%
  select(ADC, T2, Tumor)


tumortest_noperf_exp1 <- tumortest_exp1 %>%
  select(ADC, T2, Tumor)


#EXP2


trainIndex_exp2 <- createDataPartition(exp2$tumor, p = 0.75, list = FALSE)


head(trainIndex_exp2)

tumortrain_exp2 <- exp2[trainIndex_exp2,]

tumortrain_exp2 <- SMOTE(tumor ~ ., as.data.frame(tumortrain_exp2), perc.over = 100)%>%
  glimpse()

tumortest_exp2 <- exp2[-trainIndex_exp2,]%>%
  glimpse()

tumortrain_noperf_exp2 <- tumortrain_exp2 %>%
  select(ADC, T2, tumor)

tumortest_noperf_exp2 <- tumortest_exp2 %>%
  select(ADC, T2, tumor)


#EXP3


trainIndex_exp3 <- createDataPartition(exp3$tumor, p = 0.75, list = FALSE)

head(trainIndex_exp3)

tumortrain_exp3 <- exp3[trainIndex_exp3,]

tumortrain_exp3 <- SMOTE(tumor ~ ., as.data.frame(tumortrain_exp3), perc.over = 100)%>%
  glimpse()

tumortest_exp3 <- exp3[-trainIndex_exp3,]%>%
  glimpse()


tumortrain_noperf_exp3 <- tumortrain_exp3 %>%
  select(ADC, T2, tumor)

tumortest_noperf_exp3 <- tumortest_exp3 %>%
  select(ADC, T2, tumor)


#EXP4


trainIndex_exp4 <- createDataPartition(exp4$tumor, p = 0.75, list = FALSE)

head(trainIndex_exp4)

tumortrain_exp4 <- exp4[trainIndex_exp4,]%>%
  glimpse()

tumortrain_exp4 <- SMOTE(tumor ~ ., as.data.frame(tumortrain_exp4), perc.over = 100)%>%
  glimpse()

tumortest_exp4 <- exp4[-trainIndex_exp4,]%>%
  glimpse()


tumortrain_noperf_exp4 <- tumortrain_exp4 %>%
  select(ADC, T2, tumor)%>%
  glimpse()

tumortest_noperf_exp4 <- tumortest_exp4 %>%
  select(ADC, T2, tumor)%>%
  glimpse()


# GBM with EXP1 -----------------------------------------------------------------

set.seed(42)
model_gbm_exp1 <- train(Tumor~., data = tumortrain_exp1, method = "gbm", metric="ROC", trControl = myControl)
d <- summary(model_gbm_exp1)
plot(model_gbm_exp1)
model_gbm_exp1

pred_gbm_exp1 <- predict(model_gbm_exp1, tumortest_exp1)
(cm_gbm_exp1 <- confusionMatrix(pred_gbm_exp1, tumortest_exp1$Tumor, positive = "Yes"))
probs_gbm_exp1 <- predict(model_gbm_exp1, tumortest_exp1, type = "prob")
ROC_gbm_exp1 <- roc(predictor=probs_gbm_exp1$Yes, response = tumortest_exp1$Tumor)
ROC_gbm_exp1$auc

plot(ROC_gbm_exp1)
roc1 <- smooth(ROC_gbm_exp1)



# GBM without EXP1 --------------------------------------

set.seed(42)
model_gbm_noperf_exp1 <- train(Tumor~., data = tumortrain_noperf_exp1, method = "gbm", 
                               metric="ROC", trControl = myControl)
summary(model_gbm_noperf_exp1)
plot(model_gbm_noperf_exp1)
model_gbm_noperf_exp1

pred_gbm_noperf_exp1 <- predict(model_gbm_noperf_exp1, tumortest_noperf_exp1)
confusionMatrix(pred_gbm_noperf_exp1, tumortest_noperf_exp1$Tumor, positive = "Yes")
probs_gbm_noperf_exp1 <- predict(model_gbm_noperf_exp1, tumortest_noperf_exp1, type = "prob")
ROC_gbm_noperf_exp1 <- roc(predictor=probs_gbm_noperf_exp1$Yes, response = tumortest_noperf_exp1$Tumor)
ROC_gbm_noperf_exp1$auc

plot(ROC_gbm_noperf_exp1)
roc2 <- smooth(ROC_gbm_noperf_exp1)



# GBM with EXP2 -----------------------------------------------------------


set.seed(42)
model_gbm_exp2 <- train(tumor ~ ., data=tumortrain_exp2, method = "gbm", metric="ROC", trControl = myControl)
model_gbm_exp2
a <- summary(model_gbm_exp2)
plot(model_gbm_exp2)


pred_gbm_exp2 <- predict(model_gbm_exp2, tumortest_exp2)
(cm_gbm_exp2 <- confusionMatrix(pred_gbm_exp2, tumortest_exp2$tumor, positive = "Yes"))
pred_gbm_exp2
probs_gbm_exp2 <- predict(model_gbm_exp2, tumortest_exp2, type = "prob")
probs_gbm_exp2
ROC_gbm_exp2 <- roc(predictor=probs_gbm_exp2$Yes, response = tumortest_exp2$tumor)
ROC_gbm_exp2$auc

plot(ROC_gbm_exp2)
roc3 <- smooth(ROC_gbm_exp2, method = "density")


# GBM without EXP2 ----------------------


set.seed(42)
model_gbm_noperf_exp2 <- train(tumor ~ ., data=tumortrain_noperf_exp2, method = "gbm", metric="ROC", 
                               trControl = myControl)
model_gbm_noperf_exp2
summary(model_gbm_noperf_exp2)
plot(model_gbm_noperf_exp2)

pred_gbm_noperf_exp2 <- predict(model_gbm_noperf_exp2, tumortest_noperf_exp2)
(cm_gbm_noperf_exp2 <- confusionMatrix(pred_gbm_noperf_exp2, tumortest_noperf_exp2$tumor, positive = "Yes"))
pred_gbm_noperf_exp2
probs_gbm_noperf_exp2 <- predict(model_gbm_noperf_exp2, tumortest_noperf_exp2, type = "prob")
probs_gbm_noperf_exp2
ROC_gbm_noperf_exp2 <- roc(predictor=probs_gbm_noperf_exp2$Yes, response = tumortest_noperf_exp2$tumor)
ROC_gbm_noperf_exp2$auc

plot(ROC_gbm_noperf_exp2)
roc4 <- smooth(ROC_gbm_noperf_exp2)


# GBM with EXP3 ----------------------


set.seed(42)
model_gbm_exp3 <- train(tumor ~ ., data=tumortrain_exp3, method = "gbm", metric="ROC", trControl = myControl)
model_gbm_exp3
b <- summary(model_gbm_exp3)
plot(model_gbm_exp3)


pred_gbm_exp3 <- predict(model_gbm_exp3, tumortest_exp3)
(cm_gbm_exp3 <- confusionMatrix(pred_gbm_exp3, tumortest_exp3$tumor, positive = "Yes"))
pred_gbm_exp3
probs_gbm_exp3 <- predict(model_gbm_exp3, tumortest_exp3, type = "prob")
probs_gbm_exp3
ROC_gbm_exp3 <- roc(predictor=probs_gbm_exp3$Yes, response = tumortest_exp3$tumor)
ROC_gbm_exp3$auc

plot(ROC_gbm_exp3)
roc5 <- smooth(ROC_gbm_exp3)

# GBM without EXP3 ----------------------


set.seed(42)
model_gbm_noperf_exp3 <- train(tumor ~ ., data=tumortrain_noperf_exp3, method = "gbm", metric="ROC", 
                               trControl = myControl)
model_gbm_noperf_exp3
summary(model_gbm_noperf_exp3)
plot(model_gbm_noperf_exp3)

pred_gbm_noperf_exp3 <- predict(model_gbm_noperf_exp3, tumortest_noperf_exp3)
(cm_gbm_noperf_exp3 <- confusionMatrix(pred_gbm_noperf_exp3, tumortest_noperf_exp3$tumor, positive = "Yes"))
pred_gbm_noperf_exp3
probs_gbm_noperf_exp3 <- predict(model_gbm_noperf_exp3, tumortest_noperf_exp3, type = "prob")
probs_gbm_noperf_exp3
ROC_gbm_noperf_exp3 <- roc(predictor=probs_gbm_noperf_exp3$Yes, response = tumortest_noperf_exp3$tumor)
ROC_gbm_noperf_exp3$auc

plot(ROC_gbm_noperf_exp3)
roc6 <- smooth(ROC_gbm_noperf_exp3)

# GBM with EXP4 ----------------------


set.seed(42)
model_gbm_exp4 <- train(tumor ~ ., data=tumortrain_exp4, method = "gbm", metric="ROC", trControl = myControl)
model_gbm_exp4
c <- summary(model_gbm_exp4)
plot(model_gbm_exp4)


pred_gbm_exp4 <- predict(model_gbm_exp4, tumortest_exp4)
(cm_gbm_exp4 <- confusionMatrix(pred_gbm_exp4, tumortest_exp4$tumor, positive = "Yes"))
pred_gbm_exp4
probs_gbm_exp4 <- predict(model_gbm_exp4, tumortest_exp4, type = "prob")
probs_gbm_exp4
ROC_gbm_exp4 <- roc(predictor=probs_gbm_exp4$Yes, response = tumortest_exp4$tumor)
ROC_gbm_exp4$auc

plot(ROC_gbm_exp4)
roc7 <- smooth(ROC_gbm_exp4)


# GBM without EXP4 ----------------------


set.seed(42)
model_gbm_noperf_exp4 <- train(tumor ~ ., data=tumortrain_noperf_exp4, method = "gbm", metric="ROC", trControl = myControl)
model_gbm_noperf_exp4
summary(model_gbm_noperf_exp4)
plot(model_gbm_noperf_exp4)

pred_gbm_noperf_exp4 <- predict(model_gbm_noperf_exp4, tumortest_noperf_exp4)
(cm_gbm_noperf_exp4 <- confusionMatrix(pred_gbm_noperf_exp4, tumortest_noperf_exp4$tumor, positive = "Yes"))
pred_gbm_noperf_exp4
probs_gbm_noperf_exp4 <- predict(model_gbm_noperf_exp4, tumortest_noperf_exp4, type = "prob")
probs_gbm_noperf_exp4
ROC_gbm_noperf_exp4 <- roc(predictor=probs_gbm_noperf_exp4$Yes, response = tumortest_noperf_exp4$tumor)
ROC_gbm_noperf_exp4$auc

plot(ROC_gbm_noperf_exp4)
roc8 <- smooth(ROC_gbm_noperf_exp4)


# Stasticial Testing ------------------------------------------------------


set.seed(42)
(roctest13 <- roc.test(ROC_gbm_exp1, ROC_gbm_noperf_exp1, method="venkatraman", boot.n = 2000, paired = TRUE))
set.seed(42)
(roctest4 <- roc.test(ROC_gbm_exp2, ROC_gbm_noperf_exp2, method="venkatraman", boot.n = 2000, paired = TRUE))
set.seed(42)
(roctest7 <- roc.test(ROC_gbm_exp3, ROC_gbm_noperf_exp3, method="venkatraman", boot.n = 2000, paired = TRUE))
set.seed(42)
(roctest10 <- roc.test(ROC_gbm_exp4, ROC_gbm_noperf_exp4, method="venkatraman", boot.n = 2000, paired = TRUE))


# Data Summary ------------------------------------------------------------


sprintf("Benign vs. Malignant, GBM with: %.3f", ROC_gbm_exp1$auc)
sprintf("Benign vs. Malignant, GBM without: %.3f", ROC_gbm_noperf_exp1$auc)
sprintf("p-Value: %.3f", roctest13$p.value)

sprintf("Low-Grade vs. High-Grade PCA, GBM with: %.3f", ROC_gbm_exp2$auc)
sprintf("Low-Grade vs. High-Grade PCA, GBM without: %.3f", ROC_gbm_noperf_exp2$auc)
sprintf("p-Value: %.3f", roctest4$p.value)


sprintf("Low-Grade vs Intermediate-Grade PCA, GBM with: %.3f", ROC_gbm_exp3$auc)
sprintf("Low-Grade vs Intermediate-Grade PCA, GBM without: %.3f", ROC_gbm_noperf_exp3$auc)
sprintf("p-Value: %.3f", roctest7$p.value)


sprintf("Intermediate vs. High-Grade PCA, GBM with: %.3f", ROC_gbm_exp4$auc)
sprintf("Intermediate vs. High-Grade PCA, GBM without: %.3f", ROC_gbm_noperf_exp4$auc)
sprintf("p-Value: %.3f", roctest10$p.value)


# ROC Plots -----------------------------------------------------------------


par(mfrow=c(1,1))
plot.window(xlim = c(0, 1), ylim = c(-0.2, 1),
            xaxs = "i", yaxs="i")
plot(roc1, col=1, lty=1, lwd=3, cex.axis=1.5, cex.lab=1.5)
plot(roc2, col=4, lty=3, lwd=3, add=TRUE, cex.axis=1.5, cex.lab=1.5)

plot(roc3, col=1, lty=1, lwd=3, cex.axis=1.5, cex.lab=1.5)
plot(roc4, col=4, lty=3, lwd=3, add=TRUE, cex.axis=1.5, cex.lab=1.5)

plot(roc5, col=1, lty=1,lwd=3, cex.axis=1.5, cex.lab=1.5)
plot(roc6, col=4, lty=3,lwd=3,  add=TRUE, cex.axis=1.5, cex.lab=1.5)

plot(roc7, col=1, lty=1, lwd=3, cex.axis=1.5, cex.lab=1.5)
plot(roc8, col=4, lty=3, lwd=3, add=TRUE, cex.axis=1.5, cex.lab=1.5)
