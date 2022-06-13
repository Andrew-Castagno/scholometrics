SMOTE with GLM
================
Andrew Castagno
5/30/2022

``` r
library(tidyverse)
library(tidymodels)
library(themis)
library(skimr)

ch.df <- read_csv("churn_clean.csv") %>%
  mutate(Churn_bin = as.integer(ifelse(Churn == "Yes", 1, 0)), Zip = factor(Zip))
```

\#\#Modeling

Continuing from our data visualization example, next it would be nice to
move on to some modeling. For two element categorical data such as
customer churn, a logistic regression model is a simple place to start.

\#Data prep

First we should examine our variables to determine which may be valuable
in generating a predictive model of customer churn. We will also want to
check if any feature analysis is necessary. Conveniently, this dataset
does not have any NA values, but this would also be the time where we
would determine how to best handle those.

``` r
skim(ch.df)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | ch.df |
| Number of rows                                   | 10000 |
| Number of columns                                | 51    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 27    |
| factor                                           | 1     |
| numeric                                          | 23    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim\_variable   | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:-----------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| Customer\_id     |          0 |              1 |   6 |   7 |     0 |     10000 |          0 |
| Interaction      |          0 |              1 |  36 |  36 |     0 |     10000 |          0 |
| UID              |          0 |              1 |  32 |  32 |     0 |     10000 |          0 |
| City             |          0 |              1 |   3 |  26 |     0 |      6058 |          0 |
| State            |          0 |              1 |   2 |   2 |     0 |        52 |          0 |
| County           |          0 |              1 |   3 |  21 |     0 |      1620 |          0 |
| Area             |          0 |              1 |   5 |   8 |     0 |         3 |          0 |
| TimeZone         |          0 |              1 |  12 |  30 |     0 |        25 |          0 |
| Job              |          0 |              1 |   3 |  59 |     0 |       639 |          0 |
| Marital          |          0 |              1 |   7 |  13 |     0 |         5 |          0 |
| Gender           |          0 |              1 |   4 |   9 |     0 |         3 |          0 |
| Churn            |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| Techie           |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| Contract         |          0 |              1 |   8 |  14 |     0 |         3 |          0 |
| Port\_modem      |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| Tablet           |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| InternetService  |          0 |              1 |   3 |  11 |     0 |         3 |          0 |
| Phone            |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| Multiple         |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| OnlineSecurity   |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| OnlineBackup     |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| DeviceProtection |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| TechSupport      |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| StreamingTV      |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| StreamingMovies  |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| PaperlessBilling |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| PaymentMethod    |          0 |              1 |  12 |  24 |     0 |         4 |          0 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                    |
|:---------------|-----------:|---------------:|:--------|----------:|:-------------------------------|
| Zip            |          0 |              1 | FALSE   |      8583 | 161: 4, 323: 4, 443: 4, 617: 4 |

**Variable type: numeric**

| skim\_variable         | n\_missing | complete\_rate |     mean |       sd |      p0 |      p25 |      p50 |      p75 |      p100 | hist  |
|:-----------------------|-----------:|---------------:|---------:|---------:|--------:|---------:|---------:|---------:|----------:|:------|
| CaseOrder              |          0 |              1 |  5000.50 |  2886.90 |    1.00 |  2500.75 |  5000.50 |  7500.25 |  10000.00 | ▇▇▇▇▇ |
| Lat                    |          0 |              1 |    38.76 |     5.44 |   17.97 |    35.34 |    39.40 |    42.11 |     70.64 | ▁▇▇▁▁ |
| Lng                    |          0 |              1 |   -90.78 |    15.16 | -171.69 |   -97.08 |   -87.92 |   -80.09 |    -65.67 | ▁▁▂▆▇ |
| Population             |          0 |              1 |  9756.56 | 14432.70 |    0.00 |   738.00 |  2910.50 | 13168.00 | 111850.00 | ▇▁▁▁▁ |
| Children               |          0 |              1 |     2.09 |     2.15 |    0.00 |     0.00 |     1.00 |     3.00 |     10.00 | ▇▃▁▁▁ |
| Age                    |          0 |              1 |    53.08 |    20.70 |   18.00 |    35.00 |    53.00 |    71.00 |     89.00 | ▇▇▇▇▇ |
| Income                 |          0 |              1 | 39806.93 | 28199.92 |  348.67 | 19224.72 | 33170.60 | 53246.17 | 258900.70 | ▇▂▁▁▁ |
| Outage\_sec\_perweek   |          0 |              1 |    10.00 |     2.98 |    0.10 |     8.02 |    10.02 |    11.97 |     21.21 | ▁▅▇▂▁ |
| Email                  |          0 |              1 |    12.02 |     3.03 |    1.00 |    10.00 |    12.00 |    14.00 |     23.00 | ▁▂▇▂▁ |
| Contacts               |          0 |              1 |     0.99 |     0.99 |    0.00 |     0.00 |     1.00 |     2.00 |      7.00 | ▇▂▁▁▁ |
| Yearly\_equip\_failure |          0 |              1 |     0.40 |     0.64 |    0.00 |     0.00 |     0.00 |     1.00 |      6.00 | ▇▁▁▁▁ |
| Tenure                 |          0 |              1 |    34.53 |    26.44 |    1.00 |     7.92 |    35.43 |    61.48 |     72.00 | ▇▂▁▃▆ |
| MonthlyCharge          |          0 |              1 |   172.62 |    42.94 |   79.98 |   139.98 |   167.48 |   200.73 |    290.16 | ▂▇▆▃▁ |
| Bandwidth\_GB\_Year    |          0 |              1 |  3392.34 |  2185.29 |  155.51 |  1236.47 |  3279.54 |  5586.14 |   7158.98 | ▇▃▁▆▅ |
| Item1                  |          0 |              1 |     3.49 |     1.04 |    1.00 |     3.00 |     3.00 |     4.00 |      7.00 | ▃▇▇▃▁ |
| Item2                  |          0 |              1 |     3.51 |     1.03 |    1.00 |     3.00 |     4.00 |     4.00 |      7.00 | ▃▇▇▃▁ |
| Item3                  |          0 |              1 |     3.49 |     1.03 |    1.00 |     3.00 |     3.00 |     4.00 |      8.00 | ▃▆▇▁▁ |
| Item4                  |          0 |              1 |     3.50 |     1.03 |    1.00 |     3.00 |     3.00 |     4.00 |      7.00 | ▃▇▇▃▁ |
| Item5                  |          0 |              1 |     3.49 |     1.02 |    1.00 |     3.00 |     3.00 |     4.00 |      7.00 | ▃▇▇▃▁ |
| Item6                  |          0 |              1 |     3.50 |     1.03 |    1.00 |     3.00 |     3.00 |     4.00 |      8.00 | ▃▆▇▁▁ |
| Item7                  |          0 |              1 |     3.51 |     1.03 |    1.00 |     3.00 |     4.00 |     4.00 |      7.00 | ▃▇▇▃▁ |
| Item8                  |          0 |              1 |     3.50 |     1.03 |    1.00 |     3.00 |     3.00 |     4.00 |      8.00 | ▃▆▇▁▁ |
| Churn\_bin             |          0 |              1 |     0.26 |     0.44 |    0.00 |     0.00 |     0.00 |     1.00 |      1.00 | ▇▁▁▁▃ |

Customer id, CaseOrder, interaction, and UID are all unique to each
individual and therefore would not be useful in generating a model and
should be dropped. County, Zip, and city both also have a very high
number of unique values relative to the size of the dataset and
therefore should be dropped or engineered in some way. An example would
be classifying those cities or counties by something like relative size
within their state, or overall population size, and engineering a
feature such as city\_size or city\_population. However, that would
require data on the sizes of those cities and for now it should be fine
to drop them, as a lot of that same information will be contained at the
state level.

The final variable which is questionable is job, which has over 600
unique values. In the earlier data visualizations, it became clear that
some jobs are associated with higher rates of customer churn. For now we
will leave this variable in, but if there is overfitting then we can
revisit it.

``` r
model.df <- ch.df %>%
  select(-c("Zip", "City", "County", "Customer_id", "CaseOrder", "Interaction", "UID", "Job", "Churn_bin"))

prop.table(table(model.df$Churn))
```

    ## 
    ##    No   Yes 
    ## 0.735 0.265

There is a minor class imbalance in our response variable, with customer
churn occurring in 26.5% of cases vs the 73.5% of cases in which there
is customer retention. There are a few ways to deal with class imbalance
like this, and here we will attempt a few different options and select
the one that results in the best accuracy, specificity, and sensitivity
in a logistic regression model.

This will all be handled with tidymodels.

First we will split our data into training and testing sets.

``` r
set.seed(111)

#by specifying a strata, we will ensure that churn retains the same proportion in training and testing data. The proportion defaults to 3/4 training to 1/4 testing, which is plenty.

c_split <- initial_split(model.df, strata = Churn)
c_train <- training(c_split)
c_test <- testing(c_split)
#setting up cross validation folds
c_folds <- vfold_cv(c_train)
```

Next we will set up our tidymodels recipe, in which we will specify that
SMOTE is to be done to alleviate our class imbalance problem.

``` r
c_rec <- recipe(Churn~., data = c_train) %>%
  step_dummy(all_nominal(), -all_outcomes())%>%
  step_smote(Churn)

#If you wish to extract the SMOTE-ed data for analysis outside of the tidymodels framework, the following line is helpful
#transformed_data <- c_rec %>% prep %>% juice

#then creating a workflow
c_wf <- workflow() %>%
  add_recipe(c_rec)
```

\#Logistic Regression Modeling

Now we will perform some logistic regression modeling, this model seeks
to determine the log-odds of a customer ceasing to utilize the company’s
products.

``` r
glm_spec <- logistic_reg() %>% 
  set_engine('glm')
```

``` r
doParallel::registerDoParallel()

glm_rs <- c_wf %>% 
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = c_folds,
    metrics = metric_set(roc_auc, accuracy,kap, sensitivity, specificity),
    control = control_resamples(save_pred = TRUE))

collect_metrics(glm_rs)
```

    ## # A tibble: 5 × 6
    ##   .metric     .estimator  mean     n std_err .config             
    ##   <chr>       <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1 accuracy    binary     0.886    10 0.00112 Preprocessor1_Model1
    ## 2 kap         binary     0.724    10 0.00368 Preprocessor1_Model1
    ## 3 roc_auc     binary     0.956    10 0.00148 Preprocessor1_Model1
    ## 4 sensitivity binary     0.886    10 0.00401 Preprocessor1_Model1
    ## 5 specificity binary     0.885    10 0.00986 Preprocessor1_Model1

This model apparently does a great job of predicting customer churn,
with accuracy (how accurately it assigns yes/no), specificity (the
proportion of successfully identified positives), and sensitivity (the
proportion of successfully identified negatives) all at around the .89
range. An ROC area under the curve of .957 is outstanding, further
suggesting that our model far outperforms chance. Furthermore, kappa -
which is a measurement of how well the model does compared to chance -
is at .73, which according to Cohen’s interpretation is a “substantial”
level of agreement.

To finalize our model, we can make use of the “last\_fit” command, which
will train on our training set and test on our testing data.

``` r
GLM_final <- c_wf %>%
 add_model(glm_spec) %>%
 last_fit(c_split,
          metrics = metric_set(roc_auc, accuracy,kap, sensitivity, specificity))
```

    ## ! train/test split: preprocessor 1/1, model 1/1 (predictions): There are new levels in a fac...

``` r
collect_metrics(GLM_final)
```

    ## # A tibble: 5 × 4
    ##   .metric     .estimator .estimate .config             
    ##   <chr>       <chr>          <dbl> <chr>               
    ## 1 accuracy    binary         0.887 Preprocessor1_Model1
    ## 2 kap         binary         0.728 Preprocessor1_Model1
    ## 3 sensitivity binary         0.885 Preprocessor1_Model1
    ## 4 specificity binary         0.893 Preprocessor1_Model1
    ## 5 roc_auc     binary         0.959 Preprocessor1_Model1

And again we see high accuracy, specificity, and sensitivity, all near
.88, along with a high kappa and very high ROC AUC.

Now let’s compare to a model in which we do not perform SMOTE on our
data.

``` r
doParallel::registerDoParallel()
#It's not necessary to convert all nominal variables into dummies because we are not smote-ing. These no smote variables will have "ns" in their names ns = no smote. 
c_ns_rec <- recipe(Churn~., data = c_train)

#then creating a workflow
c_ns_wf <- workflow() %>%
  add_recipe(c_ns_rec)

#generating the logistic regression model
glm_ns_rs <- c_ns_wf %>% 
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = c_folds,
    metrics = metric_set(roc_auc, accuracy,kap, sensitivity, specificity),
    control = control_resamples(save_pred = TRUE))

collect_metrics(glm_ns_rs)
```

    ## # A tibble: 5 × 6
    ##   .metric     .estimator  mean     n std_err .config             
    ##   <chr>       <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1 accuracy    binary     0.898    10 0.00413 Preprocessor1_Model1
    ## 2 kap         binary     0.735    10 0.00978 Preprocessor1_Model1
    ## 3 roc_auc     binary     0.956    10 0.00141 Preprocessor1_Model1
    ## 4 sensitivity binary     0.938    10 0.00447 Preprocessor1_Model1
    ## 5 specificity binary     0.788    10 0.0135  Preprocessor1_Model1

This resulted in a higher sensitivity by about .06, and a lower
specificity by about .08. ROC auc is very slightly higher, but likely
not in a statistically significant way. That being said, modeling
without SMOTE may be better in this case, as it does a better job of
capturing cases where customer’s leave (positive cases of customer
churn). Whether you choose to use the model in which the Churn variable
has undergone SMOTE or not depends on whether you care more about
positive predictive value (correctly identifying when a person will
leave the company), or negative predictive value (correctly identifying
when a customer will stay). Both models are performing very well, so it
is a matter of priorities.

Finalizing the no smote model:

``` r
GLM_ns_final <- c_ns_wf %>%
 add_model(glm_spec) %>%
 last_fit(c_split,
          metrics = metric_set(roc_auc, accuracy,kap, sensitivity, specificity))
```

    ## ! train/test split: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...

``` r
collect_metrics(GLM_ns_final)
```

    ## # A tibble: 5 × 4
    ##   .metric     .estimator .estimate .config             
    ##   <chr>       <chr>          <dbl> <chr>               
    ## 1 accuracy    binary         0.905 Preprocessor1_Model1
    ## 2 kap         binary         0.756 Preprocessor1_Model1
    ## 3 sensitivity binary         0.935 Preprocessor1_Model1
    ## 4 specificity binary         0.822 Preprocessor1_Model1
    ## 5 roc_auc     binary         0.959 Preprocessor1_Model1

Let’s try one more method, this time reducing the size of our dataset
and selecting an even number of samples from the two Churn classes.

``` r
set.seed(111)
#counting the number of samples with a "Yes" value for churn
num_yes <- model.df %>% 
  filter(Churn == "Yes") %>%
  nrow()

#Selecting an even number of samples with "No" values for churn from all of the data randomly, and then binding together with all of the samples with a "Yes" value for churn
balanced.df <- model.df%>%
  filter(Churn == "No") %>%
  slice_sample(n = num_yes)%>%
  rbind(filter(model.df, Churn == "Yes"))

#Setting up splits again in this balanced data, adding "_b_" to the naming convention for "balanced"
c_b_split <- initial_split(balanced.df, strata = Churn)
c_b_train <- training(c_b_split)
c_b_test <- testing(c_b_split)
c_b_folds <- vfold_cv(c_b_train)
```

And now performing modeling on the balanced data.

``` r
#initializing parallel processing
doParallel::registerDoParallel()
#It's not necessary to convert all nominal variables into dummies because we are not smote-ing. These no smote variables will have "ns" in their names ns = no smote. 
c_b_rec <- recipe(Churn~., data = c_b_train)

#then creating a workflow
c_b_wf <- workflow() %>%
  add_recipe(c_b_rec)

#generating the logistic regression model
glm_b_rs <- c_b_wf %>% 
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = c_b_folds,
    metrics = metric_set(roc_auc, accuracy,kap, sensitivity, specificity),
    control = control_resamples(save_pred = TRUE))

collect_metrics(glm_b_rs)
```

    ## # A tibble: 5 × 6
    ##   .metric     .estimator  mean     n std_err .config             
    ##   <chr>       <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1 accuracy    binary     0.886    10 0.00225 Preprocessor1_Model1
    ## 2 kap         binary     0.772    10 0.00448 Preprocessor1_Model1
    ## 3 roc_auc     binary     0.955    10 0.00115 Preprocessor1_Model1
    ## 4 sensitivity binary     0.872    10 0.00334 Preprocessor1_Model1
    ## 5 specificity binary     0.900    10 0.00412 Preprocessor1_Model1

This model results in the highest specificity of all, along with a
fairly high sensitivity. The kappa is also the highest of all our
models, and the ROC auc is similar.

Finalizing this model.

``` r
GLM_b_final <- c_b_wf %>%
 add_model(glm_spec) %>%
 last_fit(c_b_split,
          metrics = metric_set(roc_auc, accuracy,kap, sensitivity, specificity))
```

    ## ! train/test split: preprocessor 1/1, model 1/1 (predictions): prediction from a rank-defici...

``` r
collect_metrics(GLM_b_final)
```

    ## # A tibble: 5 × 4
    ##   .metric     .estimator .estimate .config             
    ##   <chr>       <chr>          <dbl> <chr>               
    ## 1 accuracy    binary         0.873 Preprocessor1_Model1
    ## 2 kap         binary         0.747 Preprocessor1_Model1
    ## 3 sensitivity binary         0.843 Preprocessor1_Model1
    ## 4 specificity binary         0.903 Preprocessor1_Model1
    ## 5 roc_auc     binary         0.944 Preprocessor1_Model1
