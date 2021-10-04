## Thesis code

# 1. Load necessary packages -------------------------------------------------

pacman::p_load("readxl",       # read excel
               "tidyverse",    # data manipulation
               "lubridate",    # date formatting
               "caret",        # model training
               "recipes",      # pre-processing
               "rsample",      # data splitting
               "DataExplorer", # EDA
               "corrplot",     # correlation plot
               "glmnet",       # elastic net modeling
               "MLmetrics",    # model metrics
               "vip",          # variable importance
               "pdp",          # partial dependence plot
               "gridExtra",    # plot tool
               "caTools",      # plot ROC curve
               "ggmosaic",     # Categorical Variables Exploration
               "randomForest", # Modeling Random forest
               "xgboost",      # Modeling Boosting
               "e1071",        # Modeling Support vector machine
               "modelplotr",   # To evaluate the business value of predictive models
               "caTools"
)


# 2. Load the data -----------------------------------------------------------

rawdata <- read_excel("data.xlsx")
glimpse(rawdata)
summary(rawdata)
# translate variables
data <- rawdata %>%
  rename( CustomerID = Ügyfél, TaxNumber = Adószám, ZipCode = Irányítószám, Municipality = Település, Address = Cím, Country = Ország,
          County = Megye, PaymentMethod = `Fizetési mód`, CustomerType = `Ügyfél típus`, Gender = Nem, Married = Házas,
          FirstPurchase = `Elso vásárlás`, LastPurchase = `Utolsó vásárlás`, No.Purchases = `Vásárlások száma`,
          TotalPayment = `Teljes fizetés(HUF)`, ProductCategory = `Árucikk kategória`, Shipping = Szállítás, Discount = Kedvezmény,
          TitleDenomination = `Cím megnevezés` )
glimpse(data)

data %>%
  group_by(ProductCategory) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

data %>% 
  dplyr::select(CustomerID, ProductCategory) %>% 
  group_by(ProductCategory) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(ProductCategory, -n), y = n)) +
  geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

# 3. RFM model ---------------------------------------------------------------

data$FirstPurchase <- as.Date(data$FirstPurchase, format="%d/%m/%y")
data$LastPurchase <- as.Date(data$LastPurchase, format="%d/%m/%y")

analysis_date <- as.Date("2021-02-01", tz = "UTC")
df_RFM <- data %>% 
  summarise(recency = as.numeric(analysis_date - data$LastPurchase),
            frequency = data$No.Purchases, 
            monetary = data$TotalPayment)
summary(df_RFM)

# Scoring
# R_score
df_RFM$R_Score[df_RFM$recency>907] <- 1
df_RFM$R_Score[df_RFM$recency>479 & df_RFM$recency<=907 ] <- 2
df_RFM$R_Score[df_RFM$recency>226 & df_RFM$recency<=479 ] <- 3
df_RFM$R_Score[df_RFM$recency<=226] <- 4

# F_score
df_RFM$F_Score[df_RFM$frequency<8] <- 1
df_RFM$F_Score[df_RFM$frequency>=8 & df_RFM$frequency<13] <- 2
df_RFM$F_Score[df_RFM$frequency>=13 & df_RFM$frequency<27 ] <- 3
df_RFM$F_Score[df_RFM$frequency>=27] <- 4

# M_score
df_RFM$M_Score[df_RFM$monetary<= 190250] <- 1
df_RFM$M_Score[df_RFM$monetary>=190250 & df_RFM$monetary<361446] <- 2
df_RFM$M_Score[df_RFM$monetary>=361446 & df_RFM$monetary<868308 ] <- 3
df_RFM$M_Score[df_RFM$monetary>=868308] <- 4

# RFM_score
df_RFM <- df_RFM %>% mutate(RFM_Score = 100 * R_Score + 10 * F_Score + M_Score)

# Customer Segmentation
glimpse(df_RFM)

champions<- c(444)
loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
potential_loyalist <-c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
recent_customers <- c(411)
promising <- c(311, 312, 313, 331)
needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
about_to_sleep <- c(211)
at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
cant_lose <- c(134,143,144,234,242,243,244)
lost <- c(111)

segments <- df_RFM
segments <- as.vector(segments$RFM_Score)
segments[which(segments %in% champions)]="Champions"
segments[which(segments %in% potential_loyalist)] = "Potential Loyalist"
segments[which(segments %in% loyal_customers)] = "Loyal Customers"
segments[which(segments %in% recent_customers)] = "Recent Customers"
segments[which(segments %in% promising)] = "Promising"
segments[which(segments %in% needing_attention)] = "Customer Needing Attention"
segments[which(segments %in% about_to_sleep)] = "About to Sleep"
segments[which(segments %in% at_risk)] = "At Risk"
segments[which(segments %in% cant_lose)] = "Can't Lose Them"
segments[which(segments %in% lost)] = "Lost"
customer_segment <- data.frame(cus_seg=segments)
customer_segment %>% 
  count(cus_seg) %>% 
  arrange(desc(n)) %>% 
  rename(cus_seg = cus_seg, Count = n)

# Visualization
ggplot(data = customer_segment) + 
  aes(x = cus_seg, fill = cus_seg) + 
  geom_bar() + 
  labs(title = "Customer Segmentation", 
       x = "Segment", 
       y = "Total Customer") + coord_flip()+ theme_minimal()

# Classification into the data frame we use
data <- data %>% mutate(SegmentName = customer_segment$cus_seg)
data$SegmentName <- as.factor(data$SegmentName)
glimpse(data)

# Add Churn variable
data <- data %>% 
  mutate(Churn = ifelse(SegmentName == "Can't Lose Them" |
                        SegmentName == "Lost",
                              "Yes", "No"))

data$Churn <- as.factor(data$Churn)
summary(data$Churn)

data <- data %>% mutate(Recency = df_RFM$recency, Frequency = df_RFM$frequency, MonetaryValue = df_RFM$monetary)


# 4. Exploratory Data Analysis after customer segmentation -------------------

# Plot missing values
plot_missing(data, missing_only = TRUE)

# Plot variables
# Continuous Variable Distribution
data %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')
# Categorical Variable Distribution
data %>%
  keep(is.factor) %>%
  gather() %>%
  group_by(key, value) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
  coord_flip() +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')
# Continuous Variables Exploration
F_hist <- ggplot(data, aes(x = Recency, fill = Churn)) +
  geom_histogram(binwidth = 5) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,100,by=10))
F_hist
R_boxplot <- ggplot(data, aes(x = Churn, y = Recency, fill = Churn)) +
  geom_boxplot() + 
  theme_minimal() +
  theme(legend.position = 'none')
R_boxplot
# Categorical Variables Exploration
PC_graph <- trim_data %>%
  dplyr::select(ProductCategory, Churn) %>% 
  table(.) %>% 
  as.data.frame() %>% 
  ggplot(.) +
  ggmosaic::geom_mosaic(aes(weight = Freq, x = product(ProductCategory), fill = Churn)) +
  ggthemes::theme_tufte() +
  scale_fill_brewer(type = "qual") +
  labs(x = 'ProductCategory')
PC_graph 

## Histogram and bar chart
plot_histogram(churn_train)
plot_bar(churn_train[, c(6, 7, 8)])


# 5. Data pre-processing -----------------------------------------------------

data$Gender <- as.character(data$Gender)
data$Gender[is.na(data$Gender)] <- "none"
data$Married[is.na(data$Married)] <- "none"

# re-encode variables to handle predictors with more than 53 categories
data <- data %>% 
  mutate(PaymentCash = ifelse(PaymentMethod == "Készpénz",
                              "Yes", "No"))
data <- data %>% 
  mutate(InBudapest = ifelse(Municipality == "Budapest",
                             "Yes", "No"))
# change variables into factors
data$Country <- as.factor(data$Country)
data$Municipality <- as.factor(data$Municipality)
data$CustomerType <- as.factor(data$CustomerType)
data$Gender <- as.factor(data$Gender)
data$Married <- as.factor(data$Married)
data$ProductCategory <- as.factor(data$ProductCategory)
data$Shipping <- as.factor(data$Shipping)
data$Discount <- as.factor(data$Discount)
data$PaymentMethod <- as.factor(data$PaymentMethod)
data$PaymentCash <- as.factor(data$PaymentCash)
data$InBudapest <- as.factor(data$InBudapest)
data$TitleDenomination <- as.factor(data$TitleDenomination)

# new variables related to RFM
data <- data %>% 
  mutate(LengthofLoyalty = LastPurchase - FirstPurchase)
data$LengthofLoyalty <- as.double(data$LengthofLoyalty)
data <- data %>% 
  mutate(AVGSpendingperTime = MonetaryValue / Frequency)

# Feature selection
trim_data <- data %>% select(-CustomerID, -TaxNumber, -ZipCode, -Address, -Municipality, -Country, -County ,-PaymentMethod, -FirstPurchase,
                             -LastPurchase, -No.Purchases, -TotalPayment, -SegmentName )
glimpse(trim_data)


# Create an importance based on mean decreasing gini
fit_rf = randomForest(Churn~., data=trim_data)
round(importance(fit_rf), 3)
round(varImp(fit_rf), 3)

# One of the popular ways of doing feature selection is using chi-square test.
chi.square <- vector()
p.value <- vector()
cateVar <- trim_data %>% 
  dplyr::select(-Churn) %>% 
  keep(is.factor)

for (i in 1:length(cateVar)) {
  p.value[i] <- chisq.test(trim_data$Churn, unname(unlist(cateVar[i])), correct = FALSE)[3]$p.value
  chi.square[i] <- unname(chisq.test(trim_data$Churn, unname(unlist(cateVar[i])), correct = FALSE)[1]$statistic)
}

chi_sqaure_test <- tibble(variable = names(cateVar)) %>% 
  add_column(chi.square = chi.square) %>% 
  add_column(p.value = p.value)
knitr::kable(chi_sqaure_test)


# 6. Validation --------------------------------------------------------------


# Stratified sampling with the rsample package
set.seed(123)
split_strat  <- initial_split(trim_data, prop = 0.7, 
                              strata = "Churn")
churn_train  <- training(split_strat)
churn_test   <- testing(split_strat)

# baseline accuracy
round(prop.table(table(churn_train$Churn)),3)
round(prop.table(table(churn_test$Churn)),3)

# Define a cross validation method
tr_control <- trainControl(method = "cv", # k-fold cross validation
                           number = 10, # number of folds
                           classProbs = TRUE, #class probabilities rather than response
                           summaryFunction = prSummary) # prSummary for precision and recall

# Check for near-zero variances
nearZeroVar(trim_data, saveMetrics = TRUE) %>% 
  tibble::rownames_to_column() %>% 
  filter(nzv)

# Create recipe for pre-processing
blueprint <- recipe(Churn ~ ., data = churn_train) %>%
  step_nzv(all_predictors()) %>%     # remove variables that have (or almost have) the same value for every data point
  step_BoxCox(all_numeric(), -all_outcomes()) %>% # creates a specification of a recipe step that will transform data using a simple Box-Cox transformation
  step_center(all_numeric(), -all_outcomes()) %>%  # center to have a mean of 0
  step_scale(all_numeric(), -all_outcomes()) %>% # normalize to have a standard deviation of 1
  step_dummy(all_nominal(), -all_outcomes())  # The predictors are categorical in nature (i.e. nominal), it would make sense to convert 
# these factor predictors into numeric dummy variables (aka indicator variables) using step_dummy() because most Machine learning algorithms
# work only on numeric data as they are based on mathematical equations


# 7. Modeling ----------------------------------------------------------------

set.seed(123)
# To implement a logistic regression model, GLM function was used (argument family = binomial)
LR_model <- train(
  blueprint,
  data = churn_train,
  method = "glm",
  family = "binomial",
  trControl = tr_control,
  metric = "AUC")
# Regularized Generalized Linear Model / Glmnet is a package that fits generalized linear and similar models via penalized maximum likelihood
GLM_model <- train(
  blueprint,
  data = churn_train,
  method = "glmnet",
  family = "binomial",
  trControl = tr_control,
  tuneLength = 10,
  metric = "AUC")
# Recursive Partitioning / Simple Decision Tree using CARET
DT_model <- train(
  Churn ~ ., churn_train,
  method = "rpart",         
  trControl = tr_control,
  metric = "AUC")
# Simple Random forest
RF_model <- train(
  Churn ~ ., churn_train,
  method = "rf",
  ntree = 100,
  trControl = tr_control,
  metric = "AUC")
# eXtreme Gradient Boosting / A more regularized form of Gradient Boosting
GB_model <- train(
  blueprint,
  data = churn_train,
  method = "xgbTree",
  tuneGrid = data.frame(.nrounds=300, .max_depth=3,
     .eta=0.03,.gamma=0,
     .subsample=0.5, .colsample_bytree=0.1,
     .min_child_weight = 1),
  trControl = tr_control,
  metric = "AUC")
# Support Vector Machine with Linear Kernel
SVM_model <- train(
  blueprint,
  data = churn_train,
  method = "svmLinear",
  trControl = tr_control,
  tuneLength = 10,
  metric = "AUC")
# Simple Neural network
ANN_model <- train(
  blueprint,
  data = churn_train,
  method = "nnet",
  trControl = tr_control,
  metric = "AUC")


# 8. Evaluation --------------------------------------------------------------

# Extract out of sample performance measures
results <- resamples(list(
  LR = LR_model,
  GLM = GLM_model,
  DT = DT_model,
  RF = RF_model,
  GB = GB_model,
  SVM = SVM_model,
  ANN = ANN_model
))

# summary
summary(results)
bwplot(results)
dotplot(results)
dotplot(results, metric = "F")

# model differences
difValues <- diff(results)
bwplot(difValues, metric = "F")


## Assessing predictive power using test data (unseen data)

# 1) LR
# confusion matrix 
class.real.lr <- churn_test$Churn # actual class
class.pred.lr <- predict(LR_model, churn_test, type = "raw") # predicted class
scoring.lr    <- predict(LR_model, churn_test, type = "prob") [, "Yes"] # predicted ["yes"] class probability
confusionMatrix(data = class.pred.lr, reference = class.real.lr, positive = "Yes", mode = "everything")
# ROC and AUC
colAUC(scoring.lr , class.real.lr, plotROC = TRUE) 

# 2) GLM
# confusion matrix 
class.real.glm <- churn_test$Churn
class.pred.glm <- predict(GLM_model, churn_test, type = "raw")
scoring.glm    <- predict(GLM_model, churn_test, type = "prob") [, "Yes"]
confusionMatrix(data = class.pred.glm, reference = class.real.glm, positive = "Yes", mode = "everything")
# ROC and AUC
colAUC(scoring.glm , class.real.glm, plotROC = TRUE) 

# 3) DT
# confusion matrix 
class.real.tree <- churn_test$Churn
class.pred.tree <- predict(DT_model, churn_test, type = "raw")
scoring.tree    <- predict(DT_model, churn_test, type = "prob") [, "Yes"]
confusionMatrix(data = class.pred.tree, reference = class.real.tree, positive = "Yes", mode = "everything")
# ROC and AUC
colAUC(scoring.tree , class.real.tree, plotROC = TRUE) 

# 4) RF
# confusion matrix 
class.real.rf <- churn_test$Churn 
class.pred.rf <- predict(RF_model, churn_test, type = "raw") 
scoring.rf    <- predict(RF_model, churn_test, type = "prob") [, "Yes"] 
confusionMatrix(data = class.pred.rf, reference = class.real.rf, positive = "Yes", mode = "everything") 
# ROC and AUC
colAUC(scoring.rf , class.real.rf, plotROC = TRUE) 

# 5) xgboost
# confusion matrix 
class.real.xgb <- churn_test$Churn 
class.pred.xgb <- predict(GB_model, churn_test, type = "raw")
scoring.xgb    <- predict(GB_model, churn_test, type = "prob") [, "Yes"] 
confusionMatrix(data = class.pred.xgb, reference = class.real.xgb, positive = "Yes", mode = "everything")
# ROC and AUC
colAUC(scoring.xgb , class.real.xgb, plotROC = TRUE)

# 6) SVM
# confusion matrix 
class.real.svm <- churn_test$Churn 
class.pred.svm <- predict(SVM_model, churn_test, type = "raw")
scoring.svm    <- predict(SVM_model, churn_test, type = "prob") [, "Yes"] 
confusionMatrix(data = class.pred.svm, reference = class.real.svm, positive = "Yes", mode = "everything")
# ROC and AUC
colAUC(scoring.svm , class.real.svm, plotROC = TRUE)

# 7) ANN
# confusion matrix 
class.real.ann <- churn_test$Churn 
class.pred.ann <- predict(ANN_model, churn_test, type = "raw")
scoring.ann    <- predict(ANN_model, churn_test, type = "prob") [, "Yes"] 
confusionMatrix(data = class.pred.ann, reference = class.real.ann, positive = "Yes", mode = "everything")
# ROC and AUC
colAUC(scoring.ann , class.real.ann, plotROC = TRUE)


# 9. Feature interpretation --------------------------------------------------

# Variable importance plot
vip(LR_model, geom = "point", num_features = 10)
vip(GLM_model, geom = "point", num_features = 10)
vip(DT_model, geom = "point", num_features = 10)
vip(RF_model, geom = "point", num_features = 10)
vip(GB_model, geom = "point", num_features = 10)
#vip(SVM_model, geom = "point", num_features = 10)
# caret package has no implementation of varImp for SVM. Caret can give variable importance only for algorithms that can do feature
# selection and the standard 2-norm SVM is not one of them. It is fine since svm is not one of the best model.
vip(ANN_model, geom = "point", num_features = 10)

varImp(LR_model)
varImp(GLM_model)
varImp(DT_model)
varImp(RF_model)
varImp(GB_model)
varImp(ANN_model)


## Partial dependence plot

# Recency
y1 <- pdp::partial(RF_model, "Recency", grid.resolution = 20, plot = TRUE)
y2 <- ggplot(churn_train, aes(Churn, Recency)) +
  geom_boxplot() +
  coord_flip()
grid.arrange(y1, y2, ncol = 2)
pdp::partial(RF_model, "Recency", grid.resolution = 20, plot = TRUE)

# ProductCategory
p1 <- pdp::partial(RF_model, "ProductCategory") %>% as_tibble()
p1$yhat <- round(p1$yhat, 7)
p1 <- p1 %>% 
  filter(yhat != 6.246658)
p2 <- tibble(ProductCategory = "Other", yhat = 6.246658)
p1 <- rbind(p1, p2)
p1 %>% ggplot(aes(ProductCategory, yhat)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 6.246658, color = "red")

# Frequency
pdp::partial(RF_model, "Frequency", grid.resolution = 20, plot = TRUE)

# MonetaryValue
pdp::partial(RF_model, "MonetaryValue", grid.resolution = 20, plot = TRUE)

# LengthofLoyalty
pdp::partial(RF_model, "LengthofLoyalty", grid.resolution = 20, plot = TRUE)

# AVGSpendingperTime
pdp::partial(RF_model, "AVGSpendingperTime", grid.resolution = 20, plot = TRUE)


# 10. Business Impact---------------------------------------------------------

# transform datasets and model objects into scored data and calculate deciles 
scores_and_ntiles <- prepare_scores_and_ntiles(datasets=list("churn_train","churn_test"),
                                               dataset_labels = list("train data","test data"),
                                               models = list("LR_model","GLM_model","DT_model","RF_model","GB_model","SVM_model","ANN_model"),
                                               model_labels = list("Logistic regression","Generalized linear model","Decision tree",
                                                    "Random forest","Gradient boosting","Support vector machine","Artificial neural network"),
                                               target_column="Churn",
                                               ntiles = 100)

#generate input data frame for all plots in modelplotr
plot_input <- plotting_scope(prepared_input = scores_and_ntiles) # if not specified the first alphabetic model is selected, ANN in this case
plot_input <- plotting_scope(prepared_input = scores_and_ntiles, select_model_label = 'Random forest')

#Cumulative gain
plot_cumgains(data = plot_input)
#Cumulative lift
plot_cumlift(data = plot_input)
#Response plot
plot_response(data = plot_input)
#Cumulative response plot
plot_cumresponse(data = plot_input)
#Combined in one plot
plot_multiplot(data = plot_input)

## financial plots
# Return on investment plot
plot_roi(data = plot_input,fixed_costs = 1000, variable_costs_per_unit = 10, profit_per_unit = 50)
# By default, in the ROI plot the ntile is highlighted where return on investment is highest. In the profit plot and costs & revenues 
# plot, the ntile where the profit is highest is highlighted by default but that can be changed.
# Costs & Revenues plot, highlighted at max roi instead of max profit
plot_costsrevs(data = plot_input,fixed_costs = 1000,variable_costs_per_unit = 10,profit_per_unit = 50)
plot_costsrevs(data = plot_input,fixed_costs = 1000,variable_costs_per_unit = 10,profit_per_unit = 50,highlight_ntile = "max_roi")
#Profit plot , highlighted at custom ntile instead of at max profit
plot_profit(data = plot_input,fixed_costs = 1000,variable_costs_per_unit = 10,profit_per_unit = 50)
plot_profit(data = plot_input,fixed_costs = 1000,variable_costs_per_unit = 10,profit_per_unit = 50,highlight_ntile = 5)

# To highlight a specific decile (or ntile), this can be done with the parameter highlight_ntile =.
plot_cumgains(data = plot_input,highlight_ntile = 20)
plot_cumlift(data = plot_input,highlight_ntile = 20)
plot_cumresponse(data = plot_input,highlight_ntile = 20, highlight_how = 'plot')

# Customizing textual elements
my_text <- customize_plot_text(plot_input=plot_input)
# explore default values for the cumulative response plot:
my_text$cumresponse
# translate to Hungarian
my_text$cumresponse$plottitle <- 'Kumulatív reagálási diagram'
my_text$cumresponse$x_axis_label <- 'percentilis'
my_text$cumresponse$y_axis_label <- '% reagál (kumulatív)'
my_text$cumresponse$response_refline_label <- 'reagálás a teljes adatbázisban'
my_text$cumresponse$annotationtext <- "szövegmagyarázat: &NTL percentilisben reagálnak legtöbben. Gépi tanulási model az &MDL. Az adatbázisnál a &DS-ban a %% &YVAL-ek száma &VALUE"
plot_cumresponse(data = plot_input,highlight_ntile = 20, custom_plot_text = my_text)

# set scope to compare models, to have several lines in the plots
plot_input <- plotting_scope(prepared_input = scores_and_ntiles,scope = 'compare_models')
#customize plot line colors with RColorbrewer 
plot_cumgains(data = plot_input,custom_line_colors = RColorBrewer::brewer.pal(2,'Accent'))
#customize plot line colors with color names / hexadecimal codes 
plot_cumlift(data = plot_input,custom_line_colors = c('deepskyblue2','#FF0000'))

# save plot with custom location and filename
#plot_cumresponse(data = plot_input, save_fig_filename = 'D:\\plot1.png')

