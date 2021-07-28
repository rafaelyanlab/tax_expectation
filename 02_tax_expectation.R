# i. INTRODUCTION ---------------------------------------------------------

# The objective of this paper is to predict the tax scenario for employees in the payroll before the starting of a new fiscal year or the tax
# scenario for new hirings during the current year in order to reduce differences between the real amount of taxes and the cumulative total of
# taxes payed at the end of the year.

# ii. LOAD LIBRARIES ----------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(stringr)
library(lubridate)
library(caret)

# I. Data Exploration --------------------------------------------------------
# ...I.I. Variables in the Dataset --------------------------------------------------------

# Read dataset (from github):
final_set <- readRDS("rda/final_set.rda")

# Let's explore our data set:
str(final_set)
summary(final_set)

# total = value of compensation which is being paid.
# month = month of payroll.
# year = year of payroll.
# uma = "Unidad de Medida y Actualización" is an economic reference in mexican pesos which determines the amount of payment of obligations and
# assumptions established in federal laws (in this case is the reference to determine taxes). This value is updated every year and applies the
# whole year.
# years_service = years of service of an employee.
# tax_scenario = the OUTCOME we want to predict.
# id_employee = identifier of employees.
# id_payroll = identifier of the payroll (there are five different payrolls, one per each employer. This variable is not id_employer, because
# an employer could have multiple types of payrolls. In this particular case the relationship is one to one).
# id_job_title = identifier of the job title of an employee.
# id_compensation = identifier of the concept of compensation that is being paid.

# In column years_service we can see there are 4 NA's. So, we will eliminate all records of those employees:
discard_observations <- final_set %>%
  filter(is.na(years_service)) %>%
  distinct(id_employee)

final_set <- anti_join(final_set,discard_observations,by="id_employee")

# Notice that there are records of negative years_service which is inconsistent.
# Therefore, we will eliminate all records of an employee in a fiscal year where years_service is negative:
discard_observations <- final_set %>%
  group_by(id_employee,year,years_service) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  filter(years_service<0)

final_set <- anti_join(final_set,discard_observations,by="id_employee")

# Now the observations of our final set are:
str(final_set)
summary(final_set)

# ...I.II. Feature Transformation --------------------------------------------------------

# ......I.II.I. Insights --------------------------------------------------------

# So far, we ended up with 9 variables and 1 outcome. In theory, we could train algorithms with all these variables.
# Nontheless, we must remember that one of the goals of this project is to come with a practical model than simply payroll projection.
# Hence, we will focus on training a model with just a few information of the employee which we "know a priori" and we can "obtain easely".
# This is the "employee profile".

# From this perspective, in our dataset, there are unknown features at the beginning of a given year like:
# 1). Compensation (id_compensation).
# 2). Total amount of payment (total).
# 3). Month of payment (month).

# There could be many combinations of these three variables. Hence, to simplify these features, we will just consider the average base salary paid
# in a year as part of the employee profile. Note that we are trading-off accuracy for simplicity.

# There is another feature that has an special treatment: Year of payroll (year). Despite we know this value "a priori", in this particular case 
# "year" is not relevant to the calculations of tax scenarios "per se". It is only necessary to determine the value of the "uma" feature. UMA value
# is updated and published every year. The reason I kept the variable "year" in the set is because in practice it is very useful as a referral
# since every stakeholder in the project uses it intuitively, as opposed to the "UMA":

final_set %>% distinct(year,uma)

# In order to train our model we will summarize the first 3 features we previously described (id_compensation, total, month) into two values per
# employee:

# 1). The average base salary: which will account for most of the variability due to other compensations. On the other hand, the variability due to
# those compensations that depend on the lenght of service will be accounted by our feature "years_service".
# 2). The months effectively paid: note that I used the word "paid" and not "worked" since there are employees who have more than one job position
# in the same period. So this variable would account the effect of accumulative salary.

# We know in fact that base salary is recorded as id_compensation = 79 (in 2020) and id_compensation = 3 (in 2017 and 2018):
base_salary <- final_set %>%
  filter(id_compensation == 79 | id_compensation == 3)

head(base_salary,10)

# So the average and the total months effectively paid are:
base_salary <- base_salary %>%
  group_by(id_employee,id_job_title,year) %>%
  summarize(paid_months = n(), average_salary = mean(total)) %>%
  ungroup()

head(base_salary,10)

# As we mentioned before, there are employees with more than 1 job title:
base_salary %>% group_by(id_employee,year) %>%
  summarize(n = n()) %>% filter(n>1)

# All the employees with more than one job title will be summarize per id_employee per year and job_title would be set as 0 (non available).
# But, since job title is relevant to our model, we would apply a "job title + average salary" approach to employees with 1 job title and a
# "just average salary" approach to employees with more than one job position.

# Therefore, we will add a variable to our dataset which tells us which approach should be considered for each employee. An easy way to accomplish
# this is to create our variable in terms of job positions:
# 1 = one job position.
# 2 = more than one job position.


# ......I.II.II. Adequating our final set --------------------------------------------------------

# First, we create two datasets, one for employees with one job position and other with the rest of employees.
# The first dataset would calculate average salary and months effectively paid groupping by both employee and job position, while the second one
# would calculate these features groupping just by employee:

type_1_set <- base_salary

head(type_1_set)

type_2_set <- final_set %>%
  filter(id_compensation == 79 | id_compensation == 3) %>%
  group_by(id_employee,year) %>%
  summarize(paid_months = n(), average_salary = mean(total))

head(type_2_set)

# Then we create a data set with job titles per employee:
job_titles_per_employee <- base_salary %>%
  group_by(id_employee,year) %>%
  summarize(n = n()) %>%
  mutate (num_job_titles = ifelse(n==1,1,2)) %>%
  ungroup()

# Note that id_employee "4" in 2020 has 2 job titles:
head(job_titles_per_employee, n =10)

# In the first set we want to keep only the records of 2017 and 2018 where id_employee "4" had just one job position. On the other hand, in the
# second set, we want to keep just 2020 grouped in 1 record (no matter if the employee had 2 job positions). Note, that the last observation
# computes 10 months effectively paid: 8 with id_job_position "4" and 2 with id_job_position "156":
type_1_set %>% filter(id_employee == 4) # In 2020 this employee has two id_job_title so we should treat it differently than 2017 and 2018.
type_2_set %>% filter(id_employee == 4) # this is a more appropriate treatment for 2020: summarize by year.

# So, we will leave only the employees which match the "job title criteria" in each set:
type_1_set <- type_1_set %>%
  left_join(
    job_titles_per_employee %>%
      filter(num_job_titles == 1) %>%
      select(id_employee,year,num_job_titles),
    by=c("id_employee","year")) %>%
  filter(num_job_titles==1)

head(type_1_set,n=10)

type_2_set <- type_2_set %>%
  left_join(
    job_titles_per_employee %>%
      filter(num_job_titles == 2) %>%
      select(id_employee,year,num_job_titles),
    by=c("id_employee","year")) %>%
  filter(num_job_titles==2) %>%
  mutate(id_job_title = 0) # we add id_job_title to facilitate append with rbind()

head(type_2_set,n=10)

# Finally, we append both sets:

both_types_set <- rbind(type_1_set,type_2_set) %>%
  arrange(id_employee,id_job_title,year)

# Note that employees with num_job_titles = 2 have id_job_title = 0. So technically we just need one of these columns.
head(both_types_set,n=10)

str(both_types_set)

# Now our final set has the following structure:
final_set <- left_join(
  both_types_set %>% select(-num_job_titles),
  final_set %>% distinct(id_employee,year,uma,years_service,id_payroll,tax_scenario),
  by = c("id_employee","year")
)

head(final_set,n=10)

# One last step we will do is convert tax scenario 2 into tax scenario 1 (both result in a zero taxable base of social security compensations):
final_set <- final_set %>% mutate(tax_scenario = ifelse(tax_scenario == 2, 1, tax_scenario))

saveRDS(final_set,"rda/final_set_transformed.rda")

# The features we will use for fitting models in this project are:
# 1. id_job_title
# 2. paid_months
# 3. average_salary
# 4. uma
# 5. years_service
# 6. id_payroll

# The variables that we will leave out of the models are:
# 1. id_employee: even though this variable could be relevant it is not useful to our model since we don't want to predict tax scenarios based
# on who the employee is (as a subject) but based on his attributes, in this case job_title, paid_months, average_salary and years_service.
# 2. year: as we said before, year is important as a universal referral.

# Our outcome will be:
# 1. tax_scenario


# ...I.III. Relationship between Features and Output --------------------------------------------------------

# We have transformed our data into six features. We know that some of these features are numerical and some others are categorical. We also know
# that our outcome is categorical. Hence, we have two combinations according to the type of data. We will perform a specific statistical test for
# each case to confirm that our predictors has a significant relationship with tax scenarios:

# 1. Categorical outcome and categorical feature: In this case we will perform a chi-squared test. The features included in this group are
# id_job_title and id_payroll. This test will tell us if tax scenarios differ across the categorical features and if these differences are
# significant. Greater differences between expected and actual data produce a larger chi-squared value. The larger the chi-squared value, the
# greater the probability that there really exists a significant difference, and thus that our outcome and features are dependent.

# 2. Categorigal outcome and numerical feature. In this case we will perform an ANOVA test. The features included in this group are paid_months,
# average_salary, uma and years_service. ANOVA tells us whether any of the tax scenario means are different from the overall mean of the data by
# checking the variance of each tax scenario against the overall variance of the data. If one or more tax scenarios falls outside the range of
# variation predicted by the null hypothesis (all group means are equal), then the test is statistically significant, and thus there is a
# difference in the values of the features between each tax scenario.

# ......I.III.I. Chi-squared Test --------------------------------------------------------

# id_payroll.

# Contingency table:
table(final_set$id_payroll, final_set$tax_scenario)

# Look how small are the counts of id_payroll 2 and id_payroll 5 (specially on scenarios 3 and 4).
# We can test chi-squared in two in ways:
# 1. Consider only id_payroll 1, 2 and 3 (cases >= 20).
x <- final_set %>% filter(id_payroll != 2, id_payroll != 5)
table(x$id_payroll, x$tax_scenario)

chi_square <- chisq.test(x$id_payroll, x$tax_scenario, correct=TRUE)
chi_square

# 2. Use simulations to find the p-value:
x <- table(final_set$id_payroll, final_set$tax_scenario)

chi_square <- chisq.test(x, simulate.p.value = TRUE)
chi_square

# Both results shows p-values <= 0.05, hence, there is a significant difference between id_payroll and tax scenarios.

# Plot:
p1 <- final_set %>%
  mutate(id_payroll = as.factor(id_payroll)) %>%
  group_by(id_payroll,tax_scenario) %>%
  summarize(n=n()) %>%
  ggplot(aes(fill=id_payroll, y= n, x= tax_scenario)) +
  geom_bar(stat="identity") +
  annotate("text", x = 2.5, y = 15000, label = str_c("p-value= ",signif(chi_square$p.value,digits=3),sep=""), size=3.5)
p1

# id_job_title.

# Contingency table:
head(table(final_set$id_job_title, final_set$tax_scenario),10)

# We have the same issue with id_job_title: there are several values with just a few counts (<= 20). So, we will use simulations to find the
# p-value:
x <- table(final_set$id_job_title, final_set$tax_scenario)

chi_square <- chisq.test(x, simulate.p.value = TRUE)
chi_square

# The p-value based on 2000 replicates is <= 0.05, hence, there is a significant difference between id_job_title and tax scenarios.

p2 <- final_set %>%
  group_by(id_job_title,tax_scenario) %>%
  summarize(n=n()) %>%
  ggplot(aes(fill=id_job_title, y= n, x= tax_scenario)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  annotate("text", x = 2.5, y = 15000, label = str_c("p-value= ",signif(chi_square$p.value,digits=3),sep=""), size=3.5)
p2

# Plot in 1 figure:
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)

fig <- grid.arrange(p1,p2, ncol = 2)
fig


# ......I.III.II. ANOVA Test --------------------------------------------------------

# average_salary

# We can test ANOVA like this:
one_way_anova <- aov(average_salary ~ factor(tax_scenario), data = final_set)
summary(one_way_anova)

# The p-value is <= 0.05, hence, there is a significant difference between the average_salary mean of each individual tax scenario and the
# average_salary mean of the whole data.

# Plot
p1 <- final_set %>%
  ggplot(aes(factor(tax_scenario),average_salary, col=factor(tax_scenario))) +
  geom_boxplot() +
  #geom_point(alpha = .2, size = .2) +
  #geom_jitter(width = .3) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  geom_hline(aes(yintercept = mean(average_salary)),colour = "blue", linetype="dashed", size = 1) +
  annotate("text", x = 2, y = 30000, label = str_c("p-value= ",signif(summary(one_way_anova)[[1]][1, 5],digits=3),sep=""), size=3.5)
p1

# uma

# ANOVA:
one_way_anova <- aov(uma ~ factor(tax_scenario), data = final_set)
summary(one_way_anova)

# The p-value is <= 0.05, hence, there is a significant difference between the uma mean of each individual tax scenario and the uma mean of the
# whole data.

# Plot
p2 <- final_set %>%
  ggplot(aes(factor(tax_scenario),uma, col=factor(tax_scenario))) +
  geom_boxplot() +
  #geom_point(alpha = .2, size = .2) +
  #geom_jitter(width = .3) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  geom_hline(aes(yintercept = mean(uma)),colour = "blue", linetype="dashed", size = 1) +
  annotate("text", x = 2, y = 82.5, label = str_c("p-value= ",signif(summary(one_way_anova)[[1]][1, 5],digits=3),sep=""), size=3.5)
p2

# paid_months

# ANOVA:
one_way_anova <- aov(paid_months ~ factor(tax_scenario), data = final_set)
summary(one_way_anova)

# The p-value is <= 0.05, hence, there is a significant difference between the paid_months mean of each individual tax scenario and the
# paid_months mean of the whole data.

# Plot
p3 <- final_set %>%
  ggplot(aes(factor(tax_scenario),paid_months, col=factor(tax_scenario))) +
  geom_boxplot() +
  #geom_point(alpha = .2, size = .2) +
  #geom_jitter(width = .3) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  geom_hline(aes(yintercept = mean(paid_months)),colour = "blue", linetype="dashed", size = 1) +
  annotate("text", x = 2, y = 9.5, label = str_c("p-value= ",signif(summary(one_way_anova)[[1]][1, 5],digits=3),sep=""), size=3.5)
p3

# years_service

# ANOVA:
one_way_anova <- aov(years_service ~ factor(tax_scenario), data = final_set)
summary(one_way_anova)

# The p-value is <= 0.05, hence, there is a significant difference between the years of service mean of each individual tax scenario and the
# years of service mean of the whole data.

# Plot
p4 <- final_set %>%
  ggplot(aes(factor(tax_scenario),years_service, col=factor(tax_scenario))) +
  geom_boxplot() +
  #geom_point(alpha = .2, size = .2) +
  #geom_jitter(width = .3) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  geom_hline(aes(yintercept = mean(years_service)),colour = "blue", linetype="dashed", size = 1) +
  annotate("text", x = 2, y = 75, label = str_c("p-value= ",signif(summary(one_way_anova)[[1]][1, 5],digits=3),sep=""), size=3.5)
p4

# Plot in 1 figure:
fig <- grid.arrange(p1,p2,p3,p4, ncol = 2)
fig


# ...I.IV. Create Train and Test Sets --------------------------------------------------------

# We have an imbalanced dataSet:
table(final_set$tax_scenario)
prop.table(table(final_set$tax_scenario))

# We will try over-sampling to balance our dataset:
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
library(ROSE)

# First we balance our minority class with our majority class (tax scenario 1 and 4):
final_subset_1 <- final_set %>% filter(tax_scenario==1 | tax_scenario==4)

oversampling <- ovun.sample(tax_scenario ~ ., data = final_subset_1, method = "over", seed = 1)$data
table(oversampling$tax_scenario)

balanced_final_set <- oversampling
table(balanced_final_set$tax_scenario)

# Now we balance the remaining class (tax scenario 3) with tax scenario 1 of our previous balanced dataset:
final_subset_2 <- rbind(
  final_set %>% filter(tax_scenario==3),
  balanced_final_set %>% filter(tax_scenario==1))
table(final_subset_2$tax_scenario)

oversampling <- ovun.sample(tax_scenario ~ ., data = final_subset_2, method = "over", seed = 1)$data
table(oversampling$tax_scenario)

final_subset_2 <- oversampling %>% filter(tax_scenario==3)

# We join the data in one dataset:
balanced_final_set <- rbind(balanced_final_set,final_subset_2)

table(balanced_final_set$tax_scenario)
prop.table(table(balanced_final_set$tax_scenario))

str(balanced_final_set)

# We export the dataset:
saveRDS(balanced_final_set,"rda/balanced_final_set.rda")

# Divide into train and test set:
balanced_final_set <- readRDS("rda/balanced_final_set.rda")

set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(balanced_final_set$tax_scenario, times = 1, p = 0.2, list = FALSE)
test_set <- balanced_final_set[test_index,]
train_set <- balanced_final_set[-test_index,]

# As we can see, our train set includes observations for all four possible tax scenarios by year:
train_set %>%
  group_by(year, tax_scenario) %>%
  summarize(n= n())

test_set %>%
  group_by(year, tax_scenario) %>%
  summarize(n= n())

# We check for values in id_job_title in our test set which aren't in our train set, so we can remove them:
discard_observations <- anti_join(test_set %>% distinct(id_job_title,year),
                                  train_set %>% distinct(id_job_title,year),
                                  by=c("id_job_title","year"))

discard_observations <- test_set %>%
  semi_join(discard_observations,by=c("id_job_title","year"))

test_set <- anti_join(test_set,discard_observations,by=c("id_job_title","year"))

## We will trace index of discarded observations which will be useful later on with PCA:
#
id_row_reference <- rownames(test_set)
id_row_reference <- cbind(test_set,id_row_reference)

id_discarded_observations <- semi_join(id_row_reference,discard_observations,by=c("id_job_title","year"))
id_discarded_observations <- id_discarded_observations %>% pull(.) %>% as.integer()
#
##

# And add them back to train set:
train_set <- rbind(train_set,discard_observations)

str(train_set)
str(test_set)

# Select features for algorithm:
test_set_x <- test_set %>% select(-id_employee,-year,-tax_scenario)
train_set_x <- train_set %>% select(-id_employee,-year,-tax_scenario)

# Select outcome for algorithm:
train_set_y <- factor(train_set$tax_scenario)
test_set_y <- factor(test_set$tax_scenario)

head(train_set_x,10)

# Save sets in rdas:
saveRDS(test_set,"rda/test_set.rda")
saveRDS(train_set,"rda/train_set.rda")


# II. Models --------------------------------------------------------

# Read sets (if necessary):
test_set <- readRDS("rda/test_set.rda")
train_set <- readRDS("rda/train_set.rda")

# ...II.I. K-Nearest Neighbors --------------------------------------------------------

# The method we will use is "knn" from the caret library.

# ......II.I.I. Model 1: Considering Job Title --------------------------------------------------------

method_number <- 1
method <- "K-Nearest Neighbors"
method_abv <- "knn_job_title"
method_desc <- "KNN considering feature id_job_title"

# Model description:
modelLookup("knn")

# Model training:
# Find best tuned parameter using tuneLength argument. We can also tune using the argument tuneLength (i.e. tuneLength = 10):
# The resulting model is stored in:
# train <- readRDS("rda/model_1_train_knn.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x, train_set_y,
               method = "knn",
               trControl = train_control,
               tuneGrid = data.frame(k=c(3,5,7,10)))

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Best value for tuning parameters:
parameters <- modelLookup("knn")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x)

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Exploring confusion matrix:
str(cm)
cm$overall
cm$table
cm$byClass

# Results:
# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(test_set_y,prediction_df)
names(model_predictions)[1] <- "true"

# b). Summary.
model_results <- data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,Macro_F1_weighted = F1_beta_half,Parameters = method_parameters, Description = method_desc)

# c). Train model.
model_1_train_knn <- train
saveRDS(model_1_train_knn,"rda/model_1_train_knn.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ......II.I.II. Model 2: Without Considering Job Title --------------------------------------------------------

method_number <- 2
method <- "K-Nearest Neighbors"
method_abv <- "knn_no_job_title"
method_desc <- "KNN without considering feature id_job_title" 

# Model description:
modelLookup("knn")

# Model training:
# Find best tuned parameter using tuneLength argument. We can also tune using the argument tuneLength (i.e. tuneLength = 10):
# The resulting model is stored in:
# train <- readRDS("rda/model_2_train_knn.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x %>% select(-id_job_title), train_set_y,
               method = "knn",
               trControl = train_control,
               tuneGrid = data.frame(k=c(3,5,7,10)))

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Best value for tuning parameters:
parameters <- modelLookup("knn")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x %>% select(-id_job_title))

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_2_train_knn <- train
saveRDS(model_2_train_knn,"rda/model_2_train_knn.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ......II.I.III. Model 3: Ensemble --------------------------------------------------------

method_number <- 3
method <- "K-Nearest Neighbors"
method_abv <- "knn_ensemble"
method_desc <- "KNN ensemble according to id_job_title"

# For id_job_title = 0 (multiple job titles) predict knn_no_job_title, else predict knn_job_title:
model_predictions <- cbind(test_set_x$id_job_title,model_predictions)
names(model_predictions)[1] <- "id_job_title"
head(model_predictions)

model_predictions <- model_predictions %>% mutate(knn_ensemble = if_else(id_job_title==0,knn_no_job_title,knn_job_title))

table(model_predictions$knn_job_title)
table(model_predictions$knn_no_job_title)
table(model_predictions$knn_ensemble)
table(model_predictions$true)

cm <- confusionMatrix(model_predictions$knn_ensemble,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ...II.II. Classification and Regression Trees (CART) --------------------------------------------------------

# In R, CART algorithm is called RPART (Recursive Partitioning And Regression Trees).
# Classification Trees are used for categorical data, while Regression Trees are used for numerical data.
# The methods we will use are "rpart" and "rpart2" from the rpart library.
# The difference between these methods is basically the tuning parameter. While "rpart" tunes the complexity parameter (cp), "rpart2" tunes
# the max tree depth (maxdepth).

# ......II.II.I. Model 4 (rpart): Considering Job Title --------------------------------------------------------

method_number <- 4
method <- "Classification Tree"
method_abv <- "rpart_job_title"
method_desc <- "CART considering feature id_job_title"

# Model description:
modelLookup("rpart")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_4_train_rpart.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x, train_set_y,
               method = "rpart",
               trControl = train_control,
               tuneLength = 10)

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Variable Importance:
vi <- varImp(train)
vi <- list(vi)
names(vi)[1] <- method_abv
vi

cart_importance <- vi
names(cart_importance)

# Best value for tuning parameters:
parameters <- modelLookup("rpart")$parameter
best_tune <- round(train$bestTune,5)
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x)

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_4_train_rpart <- train
saveRDS(model_4_train_rpart,"rda/model_4_train_rpart.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass
cart_importance[[method_abv]]

# Plot
plot(model_4_train_rpart$finalModel,margin=0.1)
text(model_4_train_rpart$finalModel,cex=0.75, pos=3, offset=-0.8, font=4)

# ......II.II.II. Model 5 (rpart): Without Considering Job Title --------------------------------------------------------

method_number <- 5
method <- "Classification Tree"
method_abv <- "rpart_no_job_title"
method_desc <- "CART without considering feature id_job_title" 

# Model description:
modelLookup("rpart")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_5_train_rpart.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x %>% select(-id_job_title), train_set_y,
               method = "rpart",
               trControl = train_control,
               tuneLength = 10)

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Variable Importance:
vi <- varImp(train)
vi <- list(vi)
names(vi)[1] <- method_abv
vi

cart_importance <- c(cart_importance,vi)
names(cart_importance)

# Best value for tuning parameters:
parameters <- modelLookup("rpart")$parameter
best_tune <- round(train$bestTune,5)
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x %>% select(-id_job_title))

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_5_train_rpart <- train
saveRDS(model_5_train_rpart,"rda/model_5_train_rpart.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass
cart_importance[[method_abv]]

# Plot
plot(model_5_train_rpart$finalModel,margin=0.1)
text(model_5_train_rpart$finalModel,cex=0.75, pos=3, offset=-0.8, font=4)


# ......II.II.III. Model 6 (rpart): Ensemble --------------------------------------------------------

method_number <- 6
method <- "Classification Tree"
method_abv <- "rpart_ensemble"
method_desc <- "CART ensemble according to id_job_title"

# For id_job_title = 0 (multiple job titles) predict knn_no_job_title, else predict knn_job_title:
model_predictions <- model_predictions %>% mutate(rpart_ensemble = if_else(id_job_title==0,rpart_no_job_title,rpart_job_title))

cm <- confusionMatrix(model_predictions$rpart_ensemble,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ......II.II.IV. Model 7 (rpart2): Considering Job Title --------------------------------------------------------

method_number <- 7
method <- "Classification Tree"
method_abv <- "rpart2_job_title"
method_desc <- "CART considering feature id_job_title" 

# Model description:
modelLookup("rpart2")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_7_train_rpart2.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x, train_set_y,
               method = "rpart2",
               trControl = train_control,
               tuneLength = 10)

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Variable Importance:
vi <- varImp(train)
vi <- list(vi)
names(vi)[1] <- method_abv
vi

cart_importance <- c(cart_importance,vi)
names(cart_importance)

# Best value for tuning parameters:
parameters <- modelLookup("rpart2")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x)

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_7_train_rpart2 <- train
saveRDS(model_7_train_rpart2,"rda/model_7_train_rpart2.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass
cart_importance[[method_abv]]

# Plot
plot(model_7_train_rpart2$finalModel,margin=0.1)
text(model_7_train_rpart2$finalModel,cex=0.75, pos=3, offset=-0.8, font=4)


# ......II.II.V. Model 8 (rpart2): Without Considering Job Title --------------------------------------------------------

method_number <- 8
method <- "Classification Tree"
method_abv <- "rpart2_no_job_title"
method_desc <- "CART without considering feature id_job_title" 

# Model description:
modelLookup("rpart2")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_8_train_rpart2.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x %>% select(-id_job_title), train_set_y,
               method = "rpart2",
               trControl = train_control,
               tuneLength = 10)

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Variable Importance:
vi <- varImp(train)
vi <- list(vi)
names(vi)[1] <- method_abv
vi

cart_importance <- c(cart_importance,vi)
names(cart_importance)

# Best value for tuning parameters:
parameters <- modelLookup("rpart2")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x %>% select(-id_job_title))

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_8_train_rpart2 <- train
saveRDS(model_8_train_rpart2,"rda/model_8_train_rpart2.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass
cart_importance[[method_abv]]

# Plot
plot(model_8_train_rpart2$finalModel,margin=0.1)
text(model_8_train_rpart2$finalModel,cex=0.75, pos=3, offset=-0.8, font=4)


# ......II.II.VI. Model 9 (rpart2): Ensemble --------------------------------------------------------

method_number <- 9
method <- "Classification Tree"
method_abv <- "rpart2_ensemble"
method_desc <- "CART (rpart2) ensemble according to id_job_title"

# For id_job_title = 0 (multiple job titles) predict knn_no_job_title, else predict knn_job_title:
model_predictions <- model_predictions %>% mutate(rpart2_ensemble = if_else(id_job_title==0,rpart2_no_job_title,rpart2_job_title))

cm <- confusionMatrix(model_predictions$rpart2_ensemble,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ...II.III. Principal Component Analysis (PCA) --------------------------------------------------------

# First, we will use PCA for dimensional reduction hoping that the first 3 PC's account for most of the variability in the dataset.
# Once, we have a three dimensional (or two dimensional) dataset, we could visualize the data in a plot.
# Then we can perform KNN to make predictios based on this transformed dataset.

# The main purpose of running PCA and then KNN is to have a model with high interpretability as in Classification Trees.

# ......II.III.I. PCA with prcomp --------------------------------------------------------

# In order to train this model we need to perform PCA on the final set BEFORE partitioning.
# This is because when predicting, both train and test set must have the same features which are, in this case, the Principal Components we decide
# to choose (i.e. PC1, PC2 and PC3).

features <- balanced_final_set %>% select(-id_employee,-year,-tax_scenario)
outcome <- balanced_final_set %>% select(tax_scenario) %>% pull(tax_scenario) %>% as.factor()

# Always center the data for PCA (the argument center by dafault is TRUE) and scale only if ranges of values are different for each variable.
# In this case we MUST scale the data since values are in different units, i.e. average salary is a currency variable and paid_months is a time
# variable.
pca <- prcomp(features, center = TRUE, scale = TRUE)


# ......II.III.II. PCA Elements --------------------------------------------------------

# First we will define all the elements of PCA and explain how to interpret PCA results. The main elements of the prcomp are:

# .........a). sdev --------------------------------------------------------

# sdev = standard deviation of each axis of our orthogonal linear transformation, ordered by importance (decreasing order).
# sdev^2 = eigenvalues of each axis or component, 
#        = sum of the squares distances between the projected points (to the axis) and the origin = ss(distances).
# The PC or axis is the line that best fit the points: the line with the largest ss(distances).

# The axes are the reference from where you take a "picture" of your data.
# The axes are either the number of variables or the number of observations, whichever is smaller.
pca$sdev^2

# So, our diagonal matrix of the variances is:
diag(pca$sdev^2)

# Which is the same as the diagonal of the covariance matrix of pca$x:
cov(pca$x)

# We can plot a "scree plot" which is a graph of the variance accounted for each PC.
# Variance in %:
pca_var_per <- round(pca$sdev^2/sum(pca$sdev^2)*100, 2) 
pca_var_per

# This is the variability accounted for the first 3 PCs:
sum(pca_var_per[1:3]) # Note that total variability of PC1, PC2 and PC 3 is less than 80%.

# Let's see all PCs vs Variance on a scree plot:
variance_df <- data.frame(pc = 1:length(pca_var_per), variation_perc = pca_var_per) %>%
  mutate(variation_accum = cumsum(variation_perc))

# Scree plot:
variance_df %>%
  ggplot(aes(x=pc)) +
  geom_bar(aes(y=variation_perc), stat="identity", alpha=0.5, fill="#0072B2", colour="#0072B2") +
  xlab("Principal Component") +
  ylab("Variance (%)") +
  theme_bw()

# .........b). x --------------------------------------------------------

# x = The orthogonal linear transformation of our original data into the axes or Principal Components.
# These are the coordinates of the observations on each PC.
# PCs are ordered by importance (as shown before: pca$sdev^2).
dim(pca$x)
top_n <- 10
pca$x[1:top_n,]

# Here is a graph of our observations from PC1 and PC2 point of view (the most important components).
# "Picture" of our observations viewed from PC1 and PC2.
pca_x_df <- data.frame(tax_scenario = outcome,
                       PC1 = pca$x[,1],
                       PC2 = pca$x[,2])

# Colorblind-friendly palette:
# "#999999" #gray           "#F0E442" #yellow
# "#000000" #black          "#0072B2" #dark blue
# "#E69F00" #light orange   "#D55E00" #dark orange
# "#56B4E9" #light blue     "#CC79A7" #pink
# "#009E73" #green

shapes <- c(3, 15, 4, 2) 
shapes <- shapes[as.numeric(pca_x_df$tax_scenario)]

pca_x_df %>% ggplot(aes(x=PC1, y=PC2, colour = tax_scenario, size=tax_scenario)) +
  scale_color_manual(values=c("#56B4E9","#E69F00","#009E73")) +
  xlim(-5,10) + #plot interval with most of the points
  ylim(-5,5) + #plot interval with most of the points
  scale_size_manual(values=c(.7,2,.7,1.2)) +
  geom_point(shape = shapes) +
  xlab(str_c("PC1 (", pca_var_per[1], "%)", sep="")) +
  ylab(str_c("PC2 (", pca_var_per[2], "%)", sep="")) +
  theme_bw()


# .........c). rotation --------------------------------------------------------

# rotation = matrix of eigenvectors, matrix of loading scores ("recipe" with the mix of the variables to prepare "PCs").
dim(pca$rotation)
pca$rotation

# So the variables that contribute the most to PC "n" in decreasing order are:
n <- 1
sort(abs(pca$rotation[,n]),decreasing = TRUE)

n <- 2
sort(abs(pca$rotation[,n]),decreasing = TRUE)

n <- 3
sort(abs(pca$rotation[,n]),decreasing = TRUE)

# Here is a graph of our variables from PC1 and PC2 point of view (the most important components).
# "Picture" of our variables view from PC1 and PC2.
pca_rotation_df <- data.frame(Variable=rownames(pca$rotation),
                              PC1 = pca$rotation[,1],
                              PC2 = pca$rotation[,2])

# Note that average_salary and id_job_title are the most important variables for PC1. On the other hand, years_service and id_payroll are the most
# important variables for PC2:
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
library(ggrepel)

pca_rotation_df %>% ggplot(aes(x=PC1, y=PC2, label=Variable)) +
  geom_point(alpha=0.6,fill="#0072B2",colour="#0072B2") +
  geom_text_repel(box.padding = .05,size=3,min.segment.length = 0) +
  xlab(str_c("PC1 (", pca_var_per[1], "%)", sep="")) +
  ylab(str_c("PC2 (", pca_var_per[2], "%)", sep="")) +
  theme_bw()


# ......II.III.III. Model 10: PCA + KNN Considering Job Title --------------------------------------------------------

method_number <- 10
method <- "PCA + KNN"
method_abv <- "pca_knn_job_title"
method_desc <- "KNN on PCA considering feature id_job_title" 

# Now, we will perform knn on the transformed set, using PC1, PC2 and PC3. Note that these three PCs account the following variance:
# (We will sacrifice accuracy for interpretability, since we can plot 3 dimensional data)
sum(pca_var_per[1:3])

head(pca$x[,1:3],10)

# Create train and test sets using test_index from section "I.IV Create Train and Test Sets":
pca_set <- pca$x[,1:3] %>% as.data.frame()
id_row_reference <- rownames(pca_set)
pca_set <- cbind(pca_set,id_row_reference) 

test_set_x_pca <- pca_set[test_index,] %>% as.data.frame()
train_set_x_pca <- pca_set[-test_index,] %>% as.data.frame()

# Remember than after splitting the data we deleted some observations from the test_set and added them back to the train set.
# We need to arrange the observations in the same exact order so we can compare predictions with the rest of the models:
discard_observations <- test_set_x_pca %>% filter (id_row_reference %in% id_discarded_observations)

test_set_x_pca <- test_set_x_pca %>% anti_join(discard_observations,by="id_row_reference") %>% select(-id_row_reference)
train_set_x_pca <- rbind(train_set_x_pca %>% mutate(id_row_reference=0),discard_observations) %>% select(-id_row_reference)

str(test_set_x_pca,20)
str(train_set_x_pca,20)

# test_set_y_pca is equal to test_set_y:
test_set_y_pca <- test_set_y
train_set_y_pca <- train_set_y

# Model description:
modelLookup("knn")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_10_train_pca_knn.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x_pca, train_set_y_pca,
               method = "knn",
               trControl = train_control,
               tuneGrid = data.frame(k=c(3,5,7,9,11)))

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Best value for tuning parameters:
parameters <- modelLookup("knn")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x_pca)

# Model metrics:
accuracy <- mean(prediction == test_set_y_pca)
cm <- confusionMatrix(prediction,test_set_y_pca)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

str(model_predictions)
str(prediction_df)

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_10_train_pca_knn <- train
saveRDS(model_10_train_pca_knn,"rda/model_10_train_pca_knn.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# .........a). Model Interpretation --------------------------------------------------------

# Variable Importance:
# PC1 = average_salary and id_job_title
# PC2 = years_service and id_payroll
# PC3 = paid_months and id_payroll
pca_rotation_3d_df <- data.frame(Variable=rownames(pca$rotation),
                                 PC1 = pca$rotation[,1],
                                 PC2 = pca$rotation[,2],
                                 PC3 = pca$rotation[,3])

if(!require(scatterplot3d)) install.packages("scatterplot3d", repos = "http://cran.us.r-project.org")
library(scatterplot3d)

# Variable Importance plot:
scatterplot3d(x= pca_rotation_3d_df$PC1, xlab="PC1",
              y= pca_rotation_3d_df$PC2, ylab="PC2",
              z= pca_rotation_3d_df$PC3, zlab="PC3",
              pch = 16,
              color=c("#56B4E9","#999999","#E69F00","#009E73","#CC79A7","#000000"),
              type = "h")
legend("top", legend = rownames(pca_rotation_3d_df),
       col =  c("#56B4E9","#999999","#E69F00","#009E73","#CC79A7","#000000"), pch = 16, 
       inset = -0.1, cex= 0.6, xpd = TRUE, horiz = TRUE)


# Observations:
pca_x_3d_df <- data.frame(tax_scenario = outcome,
                          PC1 = pca$x[,1],
                          PC2 = pca$x[,2],
                          PC3 = pca$x[,3])

shapes <- c(3, 4, 2)
shapes <- shapes[as.numeric(outcome)]

colors <- c("#56B4E9","#E69F00","#009E73")
colors <- colors[as.numeric(outcome)]

scatterplot3d(x= pca_x_3d_df$PC1, xlab="PC1",
              y= pca_x_3d_df$PC2, ylab="PC2",
              z= pca_x_3d_df$PC3, zlab="PC3",
              pch = shapes, angle=70,
              color=colors,
              box=TRUE)
legend("right", legend = levels(train_set_y),
       col =  c("#56B4E9","#E69F00","#009E73"), pch = 16)

scatterplot3d(x= pca_x_3d_df$PC1, xlab="PC1",
              y= pca_x_3d_df$PC2, ylab="PC2",
              z= pca_x_3d_df$PC3, zlab="PC3",
              pch = shapes, angle=70,
              color=colors,
              box=TRUE,
              type="h")
legend("right", legend = levels(train_set_y),
       col =  c("#56B4E9","#E69F00","#009E73"), pch = 16)

scatterplot3d(x= pca_x_3d_df$PC1, xlab="PC1",
              y= pca_x_3d_df$PC2, ylab="PC2",
              z= pca_x_3d_df$PC3, zlab="PC3",
              pch = shapes, angle=-70,
              color=colors,
              box=TRUE)
legend("right", legend = levels(train_set_y),
       col =  c("#56B4E9","#E69F00","#009E73"), pch = 16)

scatterplot3d(x= pca_x_3d_df$PC1, xlab="PC1",
              y= pca_x_3d_df$PC2, ylab="PC2",
              z= pca_x_3d_df$PC3, zlab="PC3",
              pch = shapes, angle=-70,
              color=colors,
              box=TRUE,
              type="h")
legend("right", legend = levels(train_set_y),
       col =  c("#56B4E9","#E69F00","#009E73"), pch = 16)


# ......II.III.IV. Model 11: PCA + KNN Without Considering Job Title --------------------------------------------------------

method_number <- 11
method <- "PCA + KNN"
method_abv <- "pca_knn_no_job_title"
method_desc <- "KNN on PCA without considering feature id_job_title" 

# Discard feature id_job_title:
features <- balanced_final_set %>% select(-id_employee,-year,-tax_scenario,-id_job_title)
outcome <- balanced_final_set %>% select(tax_scenario) %>% pull(tax_scenario) %>% as.factor()

# PCA (center and scale data):
pca <- prcomp(features, center = TRUE, scale = TRUE)

# Variance in %:
pca_var_per <- round(pca$sdev^2/sum(pca$sdev^2)*100, 2) 
pca_var_per

# Now, we will perform knn on the transformed set, using PC1, PC2 and PC3. Note that these three PCs account the following variance:
# (We will sacrifice accuracy for interpretability, since we can plot 3 dimensional data)
sum(pca_var_per[1:3])

head(pca$x[,1:3],10)

# Create train and test sets using test_index from section "I.IV Create Train and Test Sets":
pca_set <- pca$x[,1:3] %>% as.data.frame()
id_row_reference <- rownames(pca_set)
pca_set <- cbind(pca_set,id_row_reference) 

test_set_x_pca <- pca_set[test_index,] %>% as.data.frame()
train_set_x_pca <- pca_set[-test_index,] %>% as.data.frame()

# Remember than after splitting the data we deleted some observations from the test_set and added them back to the train set.
# We need to arrange the observations in the same exact order so we can compare predictions with the rest of the models:
discard_observations <- test_set_x_pca %>% filter (id_row_reference %in% id_discarded_observations)

test_set_x_pca <- test_set_x_pca %>% anti_join(discard_observations,by="id_row_reference") %>% select(-id_row_reference)
train_set_x_pca <- rbind(train_set_x_pca %>% mutate(id_row_reference=0),discard_observations) %>% select(-id_row_reference)

str(test_set_x_pca,20)
str(train_set_x_pca,20)

# test_set_y_pca is equal to test_set_y:
test_set_y_pca <- test_set_y
train_set_y_pca <- train_set_y

# Model description:
modelLookup("knn")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_11_train_pca_knn.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x_pca, train_set_y_pca,
               method = "knn",
               trControl = train_control,
               tuneGrid = data.frame(k=c(3,5,7,9,11)))

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Best value for tuning parameters:
parameters <- modelLookup("knn")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x_pca)

# Model metrics:
accuracy <- mean(prediction == test_set_y_pca)
cm <- confusionMatrix(prediction,test_set_y_pca)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

str(model_predictions)
str(prediction_df)

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_11_train_pca_knn <- train
saveRDS(model_11_train_pca_knn,"rda/model_11_train_pca_knn.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ......II.III.V. Model 12: Ensemble --------------------------------------------------------

method_number <- 12
method <- "PCA + KNN"
method_abv <- "pca_knn_ensemble"
method_desc <- "KNN on PCA ensemble according to id_job_title"

# For id_job_title = 0 (multiple job titles) predict knn_no_job_title, else predict knn_job_title:
model_predictions <- model_predictions %>% mutate(pca_knn_ensemble = if_else(id_job_title==0,pca_knn_no_job_title,pca_knn_job_title))

cm <- confusionMatrix(model_predictions$pca_knn_ensemble,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ...II.IV. Random Forest --------------------------------------------------------

# The method we will use is "rf" from the randomForest library.

# ......II.IV.I. Model 13: Considering Job Title --------------------------------------------------------

method_number <- 13
method <- "Random Forest"
method_abv <- "rf_job_title"
method_desc <- "Random Forest considering feature id_job_title" 

# Model description:
modelLookup("rf")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_13_train_rf.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x, train_set_y,
               method = "rf",
               trControl = train_control,
               tuneLength = 6, #mtry = Number of variables randomly sampled as candidates at each split.
               importance = TRUE) #importance = TRUE makes varImp(train) works.

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Variable Importance:
vi <- varImp(train)
vi <- list(vi)
names(vi)[1] <- method_abv
vi

cart_importance <- c(cart_importance,vi)
names(cart_importance)

# Best value for tuning parameters:
parameters <- modelLookup("rf")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x)

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_13_train_rf <- train
saveRDS(model_13_train_rf,"rda/model_13_train_rf.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass
cart_importance[[method_abv]]


# ......II.IV.II. Model 14: Without Considering Job Title --------------------------------------------------------

method_number <- 14
method <- "Random Forest"
method_abv <- "rf_no_job_title"
method_desc <- "Random Forest without considering feature id_job_title" 

# Model description:
modelLookup("rf")

# Model training:
# The resulting model is stored in:
# train <- readRDS("rda/model_14_train_rf.rda")
set.seed(1,sample.kind="Rounding")
train_control <- trainControl(method = "cv", number = 10) # k-fold cross-validation with 10 folds
train <- train(train_set_x %>% select(-id_job_title), train_set_y,
               method = "rf",
               trControl = train_control,
               tuneGrid = data.frame(mtry= seq(2,5)), # mtry = Number of variables randomly sampled as candidates at each split.
               importance = TRUE) # importance = TRUE makes varImp(train) works.

warnings()

# Best value for tuning parameters:
train$bestTune
ggplot(train)

# Variable Importance:
vi <- varImp(train)
vi <- list(vi)
names(vi)[1] <- method_abv
vi

cart_importance <- c(cart_importance,vi)
names(cart_importance)

# Best value for tuning parameters:
parameters <- modelLookup("rf")$parameter
best_tune <- train$bestTune
equal <- replicate(ncol(best_tune),"= ")

method_parameters <- paste0(parameters,equal,best_tune)
method_parameters

# Model prediction:
prediction <- predict(train, test_set_x %>% select(-id_job_title))

# Model metrics:
accuracy <- mean(prediction == test_set_y)
cm <- confusionMatrix(prediction,test_set_y)
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# a). Predictions.
prediction_df <- prediction %>%
  as.data.frame()

names(prediction_df)[1] <- method_abv

model_predictions <- cbind(model_predictions,prediction_df)

# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = method_parameters, Description = method_desc))

# c). Train model.
model_14_train_rpart <- train
saveRDS(model_14_train_rf,"rda/model_14_train_rf.rda")

# Print results:
head(model_predictions,10)
model_results
cm$byClass
cart_importance[[method_abv]]


# ......II.IV.III. Model 15: Ensemble --------------------------------------------------------

method_number <- 15
method <- "Random Forest"
method_abv <- "rf_ensemble"
method_desc <- "Random Forest ensemble according to id_job_title"

# For id_job_title = 0 (multiple job titles) predict knn_no_job_title, else predict knn_job_title:
model_predictions <- model_predictions %>% mutate(rf_ensemble = if_else(id_job_title==0,rf_no_job_title,rf_job_title))

cm <- confusionMatrix(model_predictions$rf_ensemble,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ...II.V. Ensembles --------------------------------------------------------

# ......II.V.I. Model 16: Ensemble All Models --------------------------------------------------------

# Finally, we will ensemble all the models we have trained.

method_number <- 16
method <- "Ensemble"
method_abv <- "ensemble_all"
method_desc <- "Ensemble of all models (1 to 15)"

str(model_predictions)

predict_class <- model_predictions=="1"
predict_class <- as.data.frame(predict_class)
ensemble <- rowMeans(predict_class[3:17]) %>% as.data.frame() %>% rename("class_1"=".")

predict_class <- model_predictions=="3"
predict_class <- as.data.frame(predict_class)
ensemble <- cbind(ensemble,rowMeans(predict_class[3:17]) %>% as.data.frame() %>% rename("class_3"="."))

predict_class <- model_predictions=="4"
predict_class <- as.data.frame(predict_class)
ensemble <- cbind(ensemble,rowMeans(predict_class[3:17]) %>% as.data.frame() %>% rename("class_4"="."))

ensemble <- colnames(ensemble)[apply(ensemble,1,which.max)]

ensemble <- str_sub(ensemble, start= -1)

ensemble <- factor(ensemble, levels = c(1,3,4)) %>% as.data.frame() %>% rename("ensemble_all"=".")

# Predictions
model_predictions <- cbind(model_predictions,ensemble)

head(model_predictions,10)

cm <- confusionMatrix(model_predictions$ensemble_all,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# ......II.V.II. Model 17 Ensemble Best Models --------------------------------------------------------

# Now we will ensemble the models we have trained.

method_number <- 17
method <- "Ensemble"
method_abv <- "ensemble_ensembles"
method_desc <- "Ensemble of ensembles (models 3, 6, 9, 12 and 15)"

# Best models:
# model 1 "knn_position_true"
# model 6 "rf_position_true"
# model 7 "rf_position_false"
# model 12 "rpart2_position_true"
# model 15 "pca_knn_position_true"

selected_models <- model_predictions %>%
  select(knn_ensemble,rpart_ensemble,rpart2_ensemble,pca_knn_ensemble,rf_ensemble)

predict_class <- selected_models=="1"
predict_class <- as.data.frame(predict_class)
ensemble <- rowMeans(predict_class) %>% as.data.frame() %>% rename("class_1"=".")

predict_class <- selected_models=="3"
predict_class <- as.data.frame(predict_class)
ensemble <- cbind(ensemble,rowMeans(predict_class) %>% as.data.frame() %>% rename("class_3"="."))

predict_class <- selected_models=="4"
predict_class <- as.data.frame(predict_class)
ensemble <- cbind(ensemble,rowMeans(predict_class) %>% as.data.frame() %>% rename("class_4"="."))

ensemble <- colnames(ensemble)[apply(ensemble,1,which.max)]

ensemble <- str_sub(ensemble, start= -1)

ensemble <- factor(ensemble, levels = c(1,3,4)) %>% as.data.frame() %>% rename("ensemble_ensembles"=".")

model_predictions <- cbind(model_predictions,ensemble)

head(model_predictions,10)

cm <- confusionMatrix(model_predictions$ensemble_ensembles,model_predictions$true)

# Model metrics:
accuracy <- cm$overall[["Accuracy"]]
recall <- cm$byClass[,1] %>% as.numeric()
precision <- cm$byClass[,3] %>% as.numeric()
beta <- 1
F1_beta_one <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))
beta <- .5
F1_beta_half <- mean(1/(beta^2/(1+beta^2)*1/recall+(1/(1+beta^2)*1/precision)))

cm
accuracy
F1_beta_one
F1_beta_half

# Results:
# b). Summary.
model_results <- rbind(model_results,
                       data.frame(ID = method_number, Method = method, Accuracy = accuracy, Macro_F1 = F1_beta_one,
                                  Macro_F1_weighted = F1_beta_half, Parameters = "N.A.", Description = method_desc))

# Print results:
head(model_predictions,10)
model_results
cm$byClass


# III. Save Results --------------------------------------------------------

saveRDS(model_results,"rda/model_results.rda")
saveRDS(model_predictions,"rda/model_predictions.rda")

str(model_predictions)

model_predictions <- model_predictions %>%
  mutate(error_rf_job_title = if_else(true == rf_job_title, 0,1),
         error_ensemble_ensembles = if_else(true == ensemble_ensembles,0,1))

errors <- model_predictions %>%
  group_by (id_job_title) %>%
  summarize(errors = sum(error_rf_job_title),n=n()) %>%
  arrange(-errors)

# Note that more that 80% of the errors corresponds to id_job_title 4:
head(errors,5)
sum(x[1:2,]$errors) / sum(x$errors)

errors_id_4 <- model_predictions %>%
  filter(id_job_title ==4) %>%
  group_by (id_job_title,true,rf_job_title) %>%
  summarize(n=n())

head(errors,10) %>%
  ggplot(aes(x=reorder(id_job_title,-errors),y=errors)) +
  geom_bar(stat = "identity",fill="#0072B2") +
  xlab("Job Title ID") +
  ylab("Errors in Prediction (Model 13 Random Forest)") +
  geom_text(aes(label=errors), vjust=-0.5, size=2.5) +
  theme_bw()

str(train_set_x)
