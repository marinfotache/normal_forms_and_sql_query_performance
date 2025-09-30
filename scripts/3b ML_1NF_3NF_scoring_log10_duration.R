####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###             3b. ML Models for Scoring/Regression - log10_duration            ###
###                 (Models building, tuning, and interpretation)                ###
####################################################################################
###   Some predictors are converted from numeric into factors, and predictors    ###
###   related to the normal form are removed                                     ###
####################################################################################
# last update: 2025-09-30

options(scipen = 999)
library(tidyverse)
library(ranger)   # for Random Forest models
library(xgboost)  # for XGBoost models
library(tidyverse)
library(tidymodels)
library(vip)
library(patchwork)
# install.packages('DALEXtra')
#library(DALEX)
library(DALEXtra)
library(scales)
library(ingredients)
library(viridis)
library(ggsci)
library(svglite)
library(corrr)



# uncomment and execute the next line to set the default/current directory according to your local confoguration
# setwd('here is the path to your current directory')

base_path <- getwd()


####################################################################################
###                           Load data & prepare df_scoring
####################################################################################

setwd(paste(base_path, 'data', sep = '/'))
load(file = 'NF_final_dataset_2025-09.RData')
glimpse(main_df)

main_df |>
     count(completed)


### Keep only the completed queries for all three db servers and all three normal forms
df_scoring_log10_duration_init <- main_df |>
    filter(completed == 'successful') |>
    semi_join(
        main_df |>
            filter(completed == 'successful') |>
            group_by(query_id) |>
            tally() |>
            filter (n == 9) |>   ### 
            distinct(query_id) |>
            ungroup()) |>
    set_names( str_remove_all(names(main_df), 'n_of_|operators_|_all|umn')) |>
    rename(
        SELECT_non_aggr_func = SELECT_all_non_aggr_func,
        SELECT_aggr_func = SELECT_all_aggr_func,
        WHERE_non_aggr_func = WHERE_all_non_aggr_func,
        WHERE_func__date = WHERE_non_aggr_func__date,
        HAVING_non_scalar_subq = HAVING_main_predicates__non_scalar_subquery,
        HAVING_scalar_subq = HAVING_main_predicates__scalar_subquery,
        query_completion = completed) |>
    rename(FROM_table_size = FROM_size_of_processed_tables) |>
    mutate(FROM_joins = FROM_INNER_joins + FROM_OUTER_joins) |>
    mutate(scale_factor = factor(scale_factor)) |>
    select(dbserver, query_id, scale_factor, normal_form, duration_sec, 
        SELECT_cols:SELECT_aggr_func, FROM_join_paths, FROM_joins, FROM_table_size, 
        WHERE_predicates:offset) |>
    #     select(-scale_factor) |>
    mutate(normal_form = as.integer(str_extract(normal_form, '[0-9]'))) |>
    mutate(duration_sec = if_else(duration_sec == 0, 0.0001, duration_sec))  |>
    mutate(log10_duration = log10(duration_sec)) |>
    # select(-duration_sec, -SELECT_n_of_all_aggr_func)
    select(-duration_sec)
    
## correlation plot
corrplot::corrplot(cor(
    df_scoring_log10_duration_init |>
        select(-query_id) |>
        select_if(is.numeric ) , 
            method = "spearman"), method = "number", 
    type = "upper",
    tl.cex = .6, number.cex = 0.7)

glimpse(df_scoring_log10_duration_init)

df_scoring_log10_duration <- df_scoring_log10_duration_init |>
     mutate (
          scale_factor = factor(scale_factor),
          SELECT_aggr_func = if_else(SELECT_aggr_func > 10, 10, SELECT_aggr_func),
          SELECT_date_func = if_else(SELECT_date_func > 0, 'yes', 'no'),
          SELECT_SUBSTR = if_else(SELECT_SUBSTR > 0, 'yes', 'no'),
          WHERE__between = if_else(WHERE__between > 0, 'yes', 'no'),
          WHERE__in = if_else(WHERE__in > 0, 'yes', 'no'),
          WHERE__like = if_else(WHERE__like > 0, 'yes', 'no'),
          WHERE_func__date = if_else(WHERE_func__date > 0, 'yes', 'no'),
          WHERE_non_aggr_func = if_else(WHERE_non_aggr_func > 0, 'yes', 'no'),
          WHERE_pkey_attribs = if_else(WHERE_pkey_attribs > 2, 2, WHERE_pkey_attribs)
          ) |>
     select(dbserver, log10_duration, scale_factor:FROM_join_paths, WHERE_predicates:offset)
     
glimpse(df_scoring_log10_duration)     

## correlation plot
corrplot::corrplot(cor(
  df_scoring_log10_duration %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .8, number.cex = 0.8)


setwd(paste(base_path, 'figures', sep = '/'))


df_scoring_log10_duration_mssqlserver <- df_scoring_log10_duration |>
    ungroup() |>
    filter(dbserver == 'mssqlserver') |>
    select(-dbserver)
# 3102

df_scoring_log10_duration_mysql <- df_scoring_log10_duration |>
    ungroup() |>
    filter(dbserver == 'mysql') |>
    select(-dbserver)
# 3102

df_scoring_log10_duration_postgresql <- df_scoring_log10_duration |>
    ungroup() |>
    filter(dbserver == 'postgresql') |>
    select(-dbserver)
# 3102

glimpse(df_scoring_log10_duration)
table(df_scoring_log10_duration$normal_form)



##########################################################################
###                             Main split of the data           
###
set.seed(1234)
splits   <- initial_split(df_scoring_log10_duration, prop = 0.75)
splits_mssqlserver <- initial_split(df_scoring_log10_duration_mssqlserver, prop = 0.75)
splits_mysql <- initial_split(df_scoring_log10_duration_mysql, prop = 0.75)
splits_postgresql <- initial_split(df_scoring_log10_duration_postgresql, prop = 0.75)

train_tbl <- training(splits)
test_tbl  <- testing(splits)
train_tbl_mssqlserver <- training(splits_mssqlserver)
test_tbl_mssqlserver  <- testing(splits_mssqlserver)
train_tbl_mysql <- training(splits_mysql)
test_tbl_mysql  <- testing(splits_mysql)
train_tbl_postgresql <- training(splits_postgresql)
test_tbl_postgresql  <- testing(splits_postgresql)


## cross-validation folds
set.seed(1234)
#cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1)
cv_train_mssqlserver <- vfold_cv(train_tbl_mssqlserver, v = 5, repeats = 1)
cv_train_mysql <- vfold_cv(train_tbl_mysql, v = 5, repeats = 1)
cv_train_postgresql <- vfold_cv(train_tbl_postgresql, v = 5, repeats = 1)



##########################################################################
###                        The recipe for data preparation             ###
# the_recipe <- recipe(log10_duration ~ ., data = train_tbl) %>%
#     step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
#     step_impute_knn(all_predictors(), neighbors = 3) %>%   # ... when having missing values
#     step_zv(all_predictors()) # this removes predictors with zero variance

the_recipe_mssqlserver <- recipe(log10_duration ~ ., data = train_tbl_mssqlserver) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_impute_knn(all_predictors(), neighbors = 3) %>%
    step_zv(all_predictors())

the_recipe_mysql <- recipe(log10_duration ~ ., data = train_tbl_mysql) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_zv(all_predictors())

the_recipe_postgresql <- recipe(log10_duration ~ ., data = train_tbl_postgresql) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_impute_knn(all_predictors(), neighbors = 3) %>%
    step_zv(all_predictors())



#########################################################################
###                           Model Specification

## Random Forest models
rf_spec <- rand_forest(
     mtry = tune(),    # first hyper-parameter to be tuned
     trees = 700,
     min_n = tune()     # second hyper-parameter to be tuned
          ) %>%
     set_engine("ranger") |>
     set_mode("regression")

rf_spec


### XGBoost models
xgb_spec <- boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                          ## step size
    ) |>
    set_engine("xgboost") %>% 
    set_mode("regression")

xgb_spec



#########################################################################
###                           Assemble the workflows

#wf_rf <- workflow() |> add_model(rf_spec) |> add_recipe(the_recipe)
wf_rf_mssqlserver <- workflow() |> add_model(rf_spec) |> add_recipe(the_recipe_mssqlserver)
wf_rf_mysql <- workflow() |> add_model(rf_spec) |> add_recipe(the_recipe_mysql)
wf_rf_postgresql <- workflow() |> add_model(rf_spec) |> add_recipe(the_recipe_postgresql)

#wf_xgb <- workflow() |> add_model(xgb_spec) |> add_recipe(the_recipe)
wf_xgb_mssqlserver <- workflow() |> add_model(xgb_spec) |> add_recipe(the_recipe_mssqlserver)
wf_xgb_mysql <- workflow() |> add_model(xgb_spec) |> add_recipe(the_recipe_mysql)
wf_xgb_postgresql <- workflow() |> add_model(xgb_spec) |> add_recipe(the_recipe_postgresql)



#########################################################################
###                      Grids for hyper-parameter tuning

set.seed(1234)
rf_grid <- dials::grid_random(
    finalize(mtry(), train_tbl |> select (-log10_duration)),
    min_n(),
    size = 100)


set.seed(1234)
xgb_grid <- dials::grid_random(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_tbl |> select (-log10_duration)),
    learn_rate(),
    size = 300   # the number should be larger, but it would take longer
    )


#########################################################################
###   Fit the models for all k-fold folders and hyper-parameters grid

set.seed(1234)
rf_resamples_mssqlserver <- wf_rf_mssqlserver |>
    tune_grid(resamples = cv_train_mssqlserver, grid = rf_grid)

set.seed(1234)
rf_resamples_mysql <- wf_rf_mysql |>
    tune_grid(resamples = cv_train_mysql, grid = rf_grid)

set.seed(1234)
rf_resamples_postgresql <- wf_rf_postgresql |>
    tune_grid(resamples = cv_train_postgresql, grid = rf_grid)


set.seed(1234)
xgb_resamples_mssqlserver <- wf_xgb_mssqlserver |>
    tune_grid(resamples = cv_train_mssqlserver, grid = xgb_grid)

set.seed(1234)
xgb_resamples_mysql <- wf_xgb_mysql |>
    tune_grid(resamples = cv_train_mysql, grid = xgb_grid)

set.seed(1234)
xgb_resamples_postgresql <- wf_xgb_postgresql |>
    tune_grid(resamples = cv_train_postgresql, grid = xgb_grid)


# In case of errors, one can extract additional information as follows ...
#temp <- xgb_resamples$.notes[[1]][1]
#temp$.notes[1]

# show_notes(.Last.tune.result)



#########################################################################
### Explore the results and choose the best hyper-parameter combination

# performance metrics (mean) across folds for each grid line

rf_resamples_mssqlserver |> collect_metrics()
autoplot(rf_resamples_mssqlserver) 
rf_resamples_mssqlserver |> collect_metrics() |> 
    filter(`.metric` == 'rsq') |> summarise(avg_rsq = mean(mean, na.rm = TRUE))

rf_resamples_mysql |> collect_metrics()
autoplot(rf_resamples_mysql) 
rf_resamples_mysql |> collect_metrics() |> 
    filter(`.metric` == 'rsq') |> summarise(avg_rsq = mean(mean, na.rm = TRUE))

rf_resamples_postgresql |> collect_metrics()
autoplot(rf_resamples_postgresql) 
rf_resamples_postgresql |> collect_metrics() |> 
    filter(`.metric` == 'rsq') |> summarise(avg_rsq = mean(mean, na.rm = TRUE))


xgb_resamples_mssqlserver |> collect_metrics()
autoplot(xgb_resamples_mssqlserver)
xgb_resamples_mssqlserver |> collect_metrics() |> 
    filter(`.metric` == 'rsq') |> summarise(avg_rsq = mean(mean, na.rm = TRUE))

xgb_resamples_mysql |> collect_metrics()
autoplot(xgb_resamples_mysql)
xgb_resamples_mysql |> collect_metrics() |> 
    filter(`.metric` == 'rsq') |> summarise(avg_rsq = mean(mean, na.rm = TRUE))

xgb_resamples_postgresql |> collect_metrics()
autoplot(xgb_resamples_postgresql)
xgb_resamples_postgresql |> collect_metrics() |> 
    filter(`.metric` == 'rsq') |> summarise(avg_rsq = mean(mean, na.rm = TRUE))



# choose the best hyper-parameter combination

best_rf_mssqlserver <- rf_resamples_mssqlserver |> select_best(metric = "rmse")
best_rf_mssqlserver

best_rf_mysql <- rf_resamples_mysql |> select_best(metric = "rmse")
best_rf_mysql

best_rf_postgresql <- rf_resamples_postgresql |> select_best(metric = "rmse")
best_rf_postgresql


best_xgb_mssqlserver <- xgb_resamples_mssqlserver |> select_best(metric = "rmse")
best_xgb_mssqlserver

best_xgb_mysql <- xgb_resamples_mysql |> select_best(metric = "rmse")
best_xgb_mysql

best_xgb_postgresql <- xgb_resamples_postgresql |> select_best(metric = "rmse")
best_xgb_postgresql


#########################################################################
###        Finalize the workflows with the best performing parameters


final_wf_rf_mssqlserver <- wf_rf_mssqlserver |>
    finalize_workflow(best_rf_mssqlserver)

final_wf_rf_mysql <- wf_rf_mysql |>
  finalize_workflow(best_rf_mysql)

final_wf_rf_postgresql <- wf_rf_postgresql |>
    finalize_workflow(best_rf_postgresql)


final_wf_xgb_mssqlserver <- wf_xgb_mssqlserver |>
    finalize_workflow(best_xgb_mssqlserver)

final_wf_xgb_mysql <- wf_xgb_mysql |>
  finalize_workflow(best_xgb_mysql)

final_wf_xgb_postgresql <- wf_xgb_postgresql |>
    finalize_workflow(best_xgb_postgresql)



## fit the final models on the entire train data set

set.seed(1234)
final_rf_train_mssqlserver <- final_wf_rf_mssqlserver |>
    fit(data = train_tbl_mssqlserver)

set.seed(1234)
final_rf_train_mysql <- final_wf_rf_mysql |>
    fit(data = train_tbl_mysql)

set.seed(1234)
final_rf_train_postgresql <- final_wf_rf_postgresql |>
    fit(data = train_tbl_postgresql)


set.seed(1234)
final_xgb_train_mssqlserver <- final_wf_xgb_mssqlserver |>
    fit(data = train_tbl_mssqlserver)

set.seed(1234)
final_xgb_train_mysql <- final_wf_xgb_mysql |>
  fit(data = train_tbl_mysql)

set.seed(1234)
final_xgb_train_postgresql <- final_wf_xgb_postgresql |>
    fit(data = train_tbl_postgresql)



#########################################################################
###             Model performance on the test data set

### Function last_fit() fits the finalized workflow one last time 
### to the training data and evaluates one last time on the testing data.

custom_metrics <- metric_set(rmse, mae, rsq, ccc)


set.seed(1234)
test__rf_mssqlserver <- final_wf_rf_mssqlserver |> last_fit(splits_mssqlserver)
test__rf_mssqlserver |> collect_metrics()
test__rf_mssqlserver |> collect_metrics(type = "wide") 
compute_metrics(test__rf_mssqlserver, metric_set(rmse, mae, rsq, ccc))

set.seed(1234)
test__rf_mysql <- final_wf_rf_mysql |> last_fit(splits_mysql)
test__rf_mysql |> collect_metrics()
test__rf_mysql |> collect_metrics(type = "wide") 
compute_metrics(test__rf_mysql, metric_set(rmse, mae, rsq, ccc))

set.seed(1234)
test__rf_postgresql <- final_wf_rf_postgresql |> last_fit(splits_postgresql)
test__rf_postgresql |> collect_metrics()
test__rf_postgresql |> collect_metrics(type = "wide") 
compute_metrics(test__rf_postgresql, metric_set(rmse, mae, rsq, ccc))


set.seed(1234)
test__xgb_mssqlserver <- final_wf_xgb_mssqlserver |> last_fit(splits_mssqlserver)
test__xgb_mssqlserver |> collect_metrics()
test__xgb_mssqlserver |> collect_metrics(type = "wide") 
compute_metrics(test__xgb_mssqlserver, metric_set(rmse, mae, rsq, ccc))

set.seed(1234)
test__xgb_mysql <- final_wf_xgb_mysql |> last_fit(splits_mysql)
test__xgb_mysql |> collect_metrics()
test__xgb_mysql |> collect_metrics(type = "wide") 
compute_metrics(test__xgb_mysql, metric_set(rmse, mae, rsq, ccc))

set.seed(1234)
test__xgb_postgresql <- final_wf_xgb_postgresql |> last_fit(splits_postgresql)
test__xgb_postgresql |> collect_metrics()
test__xgb_postgresql |> collect_metrics(type = "wide") 
compute_metrics(test__xgb_postgresql, metric_set(rmse, mae, rsq, ccc))


#########################################################################
###              Model-Level Interpretation
#########################################################################

#########################################################################
# Create a pre-processed dataframe of the train datasets
set.seed(1234)
imp_data_mssqlserver <- the_recipe_mssqlserver |> prep() |> bake(new_data = NULL)
imp_data_mysql <- the_recipe_mysql |> prep() |> bake(new_data = NULL)
imp_data_postgresql <- the_recipe_postgresql |> prep() |> bake(new_data = NULL)


#########################################################################
# Final model with the best parameters
set.seed(1234)
df_spec_final_xgb_mssqlserver <- xgb_spec |> finalize_model(best_xgb_mssqlserver) |> 
    set_engine("xgboost", importance = "permutation")
set.seed(1234)
df_spec_final_xgb_mysql <- xgb_spec |> finalize_model(best_xgb_mysql) |> 
    set_engine("xgboost", importance = "permutation")
set.seed(1234)
df_spec_final_xgb_postgresql <- xgb_spec |> finalize_model(best_xgb_postgresql) |> 
    set_engine("xgboost", importance = "permutation")


#########################################################################
# Build the explainer-objects 

set.seed(1234)
explainer_xgb_mssqlserver <- DALEXtra::explain_tidymodels(
    df_spec_final_xgb_mssqlserver |> fit(log10_duration ~ ., data = imp_data_mssqlserver),
    data = imp_data_mssqlserver |> select(-log10_duration), 
    y = train_tbl_mssqlserver$log10_duration,
    verbose = FALSE)

set.seed(1234)
explainer_xgb_mysql <- DALEXtra::explain_tidymodels(
    df_spec_final_xgb_mysql |> fit(log10_duration ~ ., data = imp_data_mysql),
    data = imp_data_mysql |> select(-log10_duration), 
    y = train_tbl_mysql$log10_duration,
    verbose = FALSE)

set.seed(1234)
explainer_xgb_postgresql <- DALEXtra::explain_tidymodels(
  df_spec_final_xgb_postgresql |> fit(log10_duration ~ ., data = imp_data_postgresql),
  data = imp_data_postgresql |> select(-log10_duration), 
  y = train_tbl_postgresql$log10_duration,
  verbose = FALSE)


DALEX::model_performance(explainer_xgb_mssqlserver)
DALEX::model_performance(explainer_xgb_mysql)
DALEX::model_performance(explainer_xgb_postgresql)


#########################################################################
# Variable importance plot

set.seed(1234)
vi_xgb_fi_mssqlserver  <- ingredients::feature_importance(explainer_xgb_mssqlserver)

set.seed(1234)
vi_xgb_fi_mysql  <- ingredients::feature_importance(explainer_xgb_mysql)

set.seed(1234)
vi_xgb_fi_postgresql  <- ingredients::feature_importance(explainer_xgb_postgresql)

g1 <- plot(vi_xgb_fi_mssqlserver, max_vars = 10) + 
     theme_bw() + 
     theme(legend.position = 'none') +      
     ggtitle("Permutation-Based Variable Importance", 
             "XGBoost - MS SQL Server") + 
     theme_bw() +
     theme(legend.position = 'none') +
     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
     theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
     theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
     theme(axis.text = element_text(hjust = 0.5, size = 11, angle = 0)) +
#     theme(strip.text = element_text(size = 11)) 
     theme(strip.text = element_blank())

g2 <- plot(vi_xgb_fi_mysql, max_vars = 10) + 
    theme_bw() + 
    theme(legend.position = 'none') +      
    ggtitle("Permutation-Based Variable Importance", 
          "XGBoost - MySQL") + 
    theme_bw() +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(axis.text = element_text(hjust = 0.5, size = 11, angle = 0)) +
  #     theme(strip.text = element_text(size = 11)) 
    theme(strip.text = element_blank())

g3 <- plot(vi_xgb_fi_postgresql, max_vars = 10) + 
     theme_bw() + 
     theme(legend.position = 'none') +      
     ggtitle("Permutation-Based Variable Importance", 
             "XGBoost - PostgreSQL") + 
     theme_bw() +
     theme(legend.position = 'none') +
     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
     theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
     theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
     theme(axis.text = element_text(hjust = 0.5, size = 11, angle = 0)) +
#     theme(strip.text = element_text(size = 11)) 
     theme(strip.text = element_blank())

x <- g1 + g2 + g3 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("71 scoring - top 10 variable importance - rf.pdf",  plot = x,  
       width = 45, height = 16, units = "cm") 



###########################################################################
##       Partial Depencence Plots and Accumulated Dependence Plots

##  SQL Server
set.seed(1234)
pd_model_mssqlserver <- partial_dependence(explainer_xgb_mssqlserver, 
    variables = c("normal_form", "scale_factor_X1"))
pd_model_mssqlserver$`_label_` = "PDP"

set.seed(1234)
ad_model_mssqlserver <- accumulated_dependence(explainer_xgb_mssqlserver, 
    variables = c("normal_form", "scale_factor_X1"))
ad_model_mssqlserver$`_label_` = "ALE"

g1 <- plot(pd_model_mssqlserver, ad_model_mssqlserver) +
     ggtitle("Feature Effects (XGBoost) - MS SQL Server", "") +
     theme_bw() + 
     theme(legend.position = 'right') +      
     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
     theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +
     theme(axis.text = element_text(hjust = 0.5, size = 10, angle = 0)) +
     theme(strip.text = element_text(size = 10)) 
g1


##  My SQL
set.seed(1234)
pd_model_mysql <- partial_dependence(explainer_xgb_mysql, 
    variables = c("normal_form", "scale_factor_X1"))
pd_model_mysql$`_label_` = "PDP"

set.seed(1234)
ad_model_mysql <- accumulated_dependence(explainer_xgb_mysql, 
    variables = c("normal_form", "scale_factor_X1"))
ad_model_mysql$`_label_` = "ALE"

g2 <- plot(pd_model_mssqlserver, ad_model_mssqlserver) +
    ggtitle("Feature Effects (XGBoost) - MySQL", "") +
    theme_bw() + 
    theme(legend.position = 'right') +      
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    theme(axis.text = element_text(hjust = 0.5, size = 10, angle = 0)) +
    theme(strip.text = element_text(size = 10)) 
g2


## PostgreSQL
set.seed(1234)
pd_model_postgresql <- partial_dependence(explainer_xgb_postgresql, 
    variables = c("normal_form", "scale_factor_X1"))
pd_model_postgresql$`_label_` = "PDP"

set.seed(1234)
ad_model_postgresql <- accumulated_dependence(explainer_xgb_postgresql, 
    variables = c("normal_form", "scale_factor_X1"))
ad_model_postgresql$`_label_` = "ALE"

g3 <- plot(pd_model_postgresql, ad_model_postgresql) +
     ggtitle("Feature Effects (XGBoost) - PostgreSQL", "") +
     theme_bw() + 
     theme(legend.position = 'none') +      
     theme(plot.title = element_text(hjust = 0.5, size = 12)) +
     theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +
     theme(axis.text = element_text(hjust = 0.5, size = 10, angle = 0)) +
     theme(strip.text = element_text(size = 10)) 

x <- g1 + g2 + g3 + plot_layout(nrow = 2, byrow = FALSE)
ggsave("72 scoring-Feature effects - PDP_ ALE - xgb.pdf",  plot = x) 

