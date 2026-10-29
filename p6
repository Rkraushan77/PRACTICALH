# 6A. Multiple linear regression (mtcars)
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
summary(model)

# 6B. Multicollinearity
cor(mtcars[, c("disp", "hp", "wt", "drat")])
library(car)
vif(model)


# 6C. Ridge regression
install.packages(c("data.table", "dplyr", "glmnet", "caret"))
library(data.table)
library(dplyr)
library(glmnet)
library(caret)

# 1. Load and combine the train and test data
train <- fread("Train.csv")
test <- fread("Test.csv")
test[, Item_Outlet_Sales := NA]
combi <- rbind(train, test)

# 2. Treat missing and zero values
missing_index <- which(is.na(combi$Item_Weight))
for (i in missing_index) {
  item <- combi$Item_Identifier[i]
  combi$Item_Weight[i] <- mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}
zero_index <- which(combi$Item_Visibility == 0)
for (i in zero_index) {
  item <- combi$Item_Identifier[i]
  combi$Item_Visibility[i] <- mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
}

# 3. Encode the categorical features
combi[, Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))]
combi[, Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]
ohe_1 <- dummyVars("~.", data = combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df <- data.table(predict(ohe_1, combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi <- cbind(combi[, c("Item_Identifier")], ohe_df)

# 4. Transform and scale the numeric features
combi[, Item_Visibility := log(Item_Visibility + 1)]
num_vars <- which(sapply(combi, is.numeric))
num_vars_names <- names(num_vars)
combi_numeric <- combi[, setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num <- preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm <- predict(prep_num, combi_numeric)
combi[, setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]
combi <- cbind(combi, combi_numeric_norm)

# 5. Split back into train and test
train <- combi[1:nrow(train)]
test <- combi[(nrow(train) + 1):nrow(combi)]
test[, Item_Outlet_Sales := NULL]

# 6. Ridge regression with cross-validation
set.seed(123)
control <- trainControl(method = "cv", number = 5)
Grid_ri_reg <- expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0002))
Ridge_model <- train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                     y = train$Item_Outlet_Sales, method = "glmnet",
                     trControl = control, tuneGrid = Grid_ri_reg)
cat("Mean validation test RMSE = ", mean(Ridge_model$resample$RMSE))
plot(Ridge_model, main = "Ridge Regression")

