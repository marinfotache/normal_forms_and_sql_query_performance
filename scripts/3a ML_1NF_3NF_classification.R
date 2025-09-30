####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###   3a. Classification Models (3). Models Building, Tuning, and Interpretation ###
###   Some predictors are converted from numeric into factors, and predictors    ###
###   related to the normal form are removed                                     ###
####################################################################################
# last update: 2025-09-29


options(scipen = 999)
#options(java.parameters = "-Xmx12g")
library(tidyverse)
library(janitor)
library(broom)
library(tidymodels)
#install.packages('ranger')
library(ranger)
#install.packages('xgboost')
library(xgboost)
library(scales)
library(patchwork)
library(viridis)
library(ggsci)
library(svglite)
library(vip)
library(corrr)

# packages for model-level interpretation (IML)
library(DALEX)
library(ingredients)
#install.packages('modelDown')
library(modelDown)
#library(gridExtra)


setwd('/Users/marinfotache/Dropbox/2024-03 Normal Forms and SQL Query Performance/git_repo/normal_forms_and_sql_query_performance')
base_path <- getwd()


####################################################################################
###                           Load data & prepare df_classif
####################################################################################

setwd(paste(base_path, 'data', sep = '/'))
load(file = 'NF_final_dataset_2025-09.RData')
glimpse(main_df)

# all records, included postgresql queries not compatible with mssqlserver syntax
df_classif_init <- main_df |>
     ungroup() |>
     set_names( str_remove_all(names(main_df), 
          'n_of_|operators_|_all|umn')) |>
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
     select(dbserver, scale_factor, normal_form, query_id, query_completion, syntax_problem,
            SELECT_cols:SELECT_aggr_func, FROM_join_paths, FROM_joins, FROM_table_size, 
            WHERE_predicates:offset) 


glimpse(df_classif_init)     


# due to the sparsity of some predictors, we recode some of the predictors:

#  SELECT_aggr_func   
table(df_classif_init$SELECT_aggr_func)
#  SELECT_date_func   
table(df_classif_init$SELECT_date_func)
#  SELECT_SUBSTR   
table(df_classif_init$SELECT_SUBSTR)
#  WHERE__between   
table(df_classif_init$WHERE__between)
#  WHERE__in   
table(df_classif_init$WHERE__in)
#  WHERE__like   
table(df_classif_init$WHERE__like)

## correlation plot
## 
corrplot::corrplot(cor(
     df_classif_init %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)

temp <- df_classif_init %>% 
          select_if(., is.numeric ) %>%
     corrr::correlate(method = "spearman") %>%
     stretch(remove.dups = TRUE)


df_classif <- df_classif_init |>
     select(dbserver, scale_factor, normal_form, query_completion, 
            SELECT_cols:SELECT_aggr_func, FROM_join_paths, FROM_joins, FROM_table_size, 
            WHERE_predicates:offset) |>
     mutate(normal_form = as.integer(str_extract(normal_form, '[0-9]'))) |>
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
     select(dbserver:FROM_join_paths, WHERE_predicates:offset)
     
glimpse(df_classif)     

## correlation plot
corrplot::corrplot(cor(
     df_classif %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)


df_classif_mssqlserver <- df_classif |>
     ungroup() |>
     filter(dbserver == 'mssqlserver') 

df_classif_mysql <- df_classif |>
  ungroup() |>
  filter(dbserver == 'mysql') 

df_classif_postgresql <- df_classif |>
     ungroup() |>
     filter(dbserver == 'postgresql') 


## correlation plot
corrplot::corrplot(cor(
     df_classif_mssqlserver %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)

corrplot::corrplot(cor(
  df_classif_mysql %>% 
    select_if(., is.numeric ) , 
  method = "spearman"), method = "number", 
  type = "upper",
  tl.cex = .6, number.cex = 0.7)

corrplot::corrplot(cor(
     df_classif_postgresql %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)


setwd(paste(base_path, 'figures', sep = '/'))

df <- df_classif_postgresql



###############################################################################
###          Distribution of query parameters for 3NF and SF 0.1
###############################################################################
glimpse(df)
df_query_parameters <- df |>
     filter(normal_form == 3 & scale_factor == 0.1) |>
     select(SELECT_cols:offset) 

cardinality_vars <- df_query_parameters |>
     mutate(across(everything(), as.character))  |>
     mutate(row_num = row_number()) |>
     pivot_longer(-row_num) |>
     group_by(name) |>
     summarise(n_distinct = n_distinct(value)) |>
     ungroup()


# compute the frequencies for each categorical variables and values
eda_factors <- df_query_parameters |>
     mutate(across(everything(), as.character))  |>
     mutate (id = row_number()) |>
     pivot_longer(-id, names_to = "variable", values_to = "value" ) |>
     mutate (value = coalesce(value, 'N/A')) |>
     group_by(variable, value) |>
     summarise (n_value = n()) |>
     ungroup() |>
     mutate (percent = round(n_value * 100 / nrow(df_query_parameters),2)) |>
     arrange(variable, value) 

#View(eda_factors)

# plot only the factors with less than 12 distinct values
g1 <- eda_factors |>
     filter (variable %in% 
                  (cardinality_vars |>
                         filter(n_distinct <= 12) |>
                         pull(name))) |>
     group_by(variable) |>
     summarise(n_of_values = n()) |>
     ungroup() |>
     select (variable) |>
     inner_join(eda_factors) |>
ggplot(aes(x = value, y = n_value)) +
     geom_col(alpha = 0.4) +
     geom_text (aes(label = paste0(round(percent,0), '%'),
                  vjust = if_else(n_value > 100, 1.5, -0.5)), size = 3) +
     facet_wrap(~ variable, scale = "free", nrow = 3) +
     theme_bw() +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)) +
     theme(strip.text.x = element_text(size = 11)) +
     xlab("") + ylab("frequency") 


x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("61a low_cardinality_query_parameters_for_sf01.pdf", plot = x,  #device = cairo_pdf(), 
       dpi = 600,width = 40, height = 20, units = "cm")


###############################################################################
### Distribution of variables having than 12 distinct values (only numerical)                    ###
num_variables <- df_query_parameters |>
     mutate(across(everything(), as.character))  |>
     mutate(row_num = row_number()) |>
     pivot_longer(-row_num, names_to = "variable", values_to = "value" ) |>
     filter (variable %in% 
                  (cardinality_vars |>
                         filter(n_distinct > 12) |>
                         pull(name))) |>
     mutate(value = as.integer(value))


###############################################################################
#    variables have more than 12 distinct values, display bar histogram
g1 <- num_variables |>
#     mutate(across(everything(), as.integer))  |>
ggplot(aes(value)) +
     geom_histogram(alpha = 0.6) +
     facet_wrap(variable ~ ., scale = "free", ncol = 2) +
     theme_bw() +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5)) +
     theme(strip.text = element_text(size = 9)) +
     xlab("") + ylab("frequency") +
     scale_x_continuous(breaks= pretty_breaks())     

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("61b higher_cardinality_query_parameters_for_sf01.pdf", plot = x,  #device = cairo_pdf(), 
       dpi = 600, width = 40, height = 20, units = "cm")

names(df)


###############################################################################
###          Distribution of query parameters for 3NF and SF 1
###############################################################################
#glimpse(df)
df_query_parameters <- df |>
    filter(normal_form == 3 & scale_factor == 1) |>
    select(SELECT_cols:offset) 

cardinality_vars <- df_query_parameters |>
    mutate(across(everything(), as.character))  |>
    mutate(row_num = row_number()) |>
    pivot_longer(-row_num) |>
    group_by(name) |>
    summarise(n_distinct = n_distinct(value)) |>
    ungroup()


# compute the frequencies for each categorical variables and values
eda_factors <- df_query_parameters |>
    mutate(across(everything(), as.character))  |>
    mutate (id = row_number()) |>
    pivot_longer(-id, names_to = "variable", values_to = "value" ) |>
    mutate (value = coalesce(value, 'N/A')) |>
    group_by(variable, value) |>
    summarise (n_value = n()) |>
    ungroup() |>
    mutate (percent = round(n_value * 100 / nrow(df_query_parameters),2)) |>
    arrange(variable, value) 

#View(eda_factors)

# plot only the factors with less than 12 distinct values
g1 <- eda_factors |>
    filter (variable %in% 
            (cardinality_vars |>
               filter(n_distinct <= 12) |>
               pull(name))) |>
    group_by(variable) |>
    summarise(n_of_values = n()) |>
    ungroup() |>
    select (variable) |>
    inner_join(eda_factors) |>
    ggplot(aes(x = value, y = n_value)) +
    geom_col(alpha = 0.4) +
    geom_text (aes(label = paste0(round(percent,0), '%'),
                 vjust = if_else(n_value > 100, 1.5, -0.5)), size = 3) +
    facet_wrap(~ variable, scale = "free", nrow = 3) +
    theme_bw() +
    theme(legend.position="none")    +
    theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)) +
    theme(strip.text.x = element_text(size = 11)) +
    xlab("") + ylab("frequency") 


x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("62a low_cardinality_query_parameters_for_sf1.pdf", plot = x,  #device = cairo_pdf(), 
       dpi = 600,width = 40, height = 20, units = "cm")


###############################################################################
### Distribution of variables having than 12 distinct values (only numerical)                    ###
num_variables <- df_query_parameters |>
    mutate(across(everything(), as.character))  |>
    mutate(row_num = row_number()) |>
    pivot_longer(-row_num, names_to = "variable", values_to = "value" ) |>
    filter (variable %in% 
            (cardinality_vars |>
               filter(n_distinct > 12) |>
               pull(name))) |>
    mutate(value = as.integer(value))


###############################################################################
#    variables have more than 12 distinct values, display bar histogram
g1 <- num_variables |>
    #     mutate(across(everything(), as.integer))  |>
    ggplot(aes(value)) +
    geom_histogram(alpha = 0.6) +
    facet_wrap(variable ~ ., scale = "free", ncol = 2) +
    theme_bw() +
    theme(legend.position="none")    +
    theme(axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5)) +
    theme(strip.text = element_text(size = 9)) +
    xlab("") + ylab("frequency") +
    scale_x_continuous(breaks= pretty_breaks())     

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("62b higher_cardinality_query_parameters_for_sf1.pdf", plot = x,  #device = cairo_pdf(), 
       dpi = 600, width = 40, height = 20, units = "cm")




####################################################################################
###       Model building, tuning and assessment using cross-validation           ###
####################################################################################
df_classif_mssqlserver <- df_classif |>
     ungroup() |>
     filter(dbserver == 'mssqlserver') |>
     select(-dbserver)

df_classif_mysql <- df_classif |>
  ungroup() |>
  filter(dbserver == 'mysql') |>
  select(-dbserver)

df_classif_postgresql <- df_classif |>
     ungroup() |>
     filter(dbserver == 'postgresql') |>
     select(-dbserver)

glimpse(df_classif)

####################################################################################
###                              Main split of the data                          ###
set.seed(1234)
splits   <- initial_split(df, prop = 0.75, strata = query_completion)
splits_mssqlserver <- initial_split(df_classif_mssqlserver, prop = 0.75, strata = query_completion)
splits_mysql <- initial_split(df_classif_mysql, prop = 0.75, strata = query_completion)
splits_postgresql <- initial_split(df_classif_postgresql, prop = 0.75, strata = query_completion)


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
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1)
cv_train_mssqlserver <- vfold_cv(train_tbl_mssqlserver, v = 5, repeats = 1)
cv_train_mysql <- vfold_cv(train_tbl_mysql, v = 5, repeats = 1)
cv_train_postgresql <- vfold_cv(train_tbl_postgresql, v = 5, repeats = 1)

glimpse(train_tbl)


####################################################################################
###                           The recipe for data preparation                    ###
recipe_cl <- recipe(query_completion ~ ., data = train_tbl) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_impute_knn(all_predictors(), neighbors = 3) %>%
    step_zv(all_predictors())
#any(is.na(train_tbl))

recipe_cl_mssqlserver <- recipe(query_completion ~ ., data = train_tbl_mssqlserver) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_impute_knn(all_predictors(), neighbors = 3) %>%
    step_zv(all_predictors())

recipe_cl_mysql <- recipe(query_completion ~ ., data = train_tbl_mysql) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_zv(all_predictors())

recipe_cl_postgresql <- recipe(query_completion ~ ., data = train_tbl_postgresql) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_impute_knn(all_predictors(), neighbors = 3) %>%
    step_zv(all_predictors())


####################################################################################
###                               Model Specification                            ###

## RF
rf_spec <- rand_forest(mtry = tune(), trees = 700, min_n = tune()) |>
     set_engine("ranger", importance = "permutation") |>
     set_mode("classification")

### XGBoost
xgb_spec <- boost_tree(
    trees = 1000,
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(),                     ## model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                         ## step size
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")


####################################################################################
###                              Assemble the workflows                          ###

wf_rf <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(recipe_cl)

wf_rf_mssqlserver <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(recipe_cl_mssqlserver)

wf_rf_mysql <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(recipe_cl_mysql)

wf_rf_postgresql <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(recipe_cl_postgresql)


wf_xgb <- workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(recipe_cl)

wf_xgb_mssqlserver <- workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(recipe_cl_mssqlserver)

wf_xgb_mysql <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(recipe_cl_mysql)

wf_xgb_postgresql <- workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(recipe_cl_postgresql)



####################################################################################
###                      Grids for hyper-parameter tuning                        ### 

set.seed(1234)
rf_grid <- dials::grid_random(
    finalize(mtry(), train_tbl %>% select (-query_completion)),
    min_n(),
    size = 100)


set.seed(1234)
xgb_grid <- dials::grid_random(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_tbl %>% select (-query_completion)),
    learn_rate(),
    size = 300)



####################################################################################
###         Fit the models for all k-fold folders and hyper-parameters grid      ###

# doParallel::registerDoParallel()

set.seed(1234)
rf_resamples <- wf_rf %>%
    tune_grid(resamples = cv_train, grid = rf_grid)

set.seed(1234)
rf_resamples_mssqlserver <- wf_rf_mssqlserver %>%
    tune_grid(resamples = cv_train_mssqlserver, grid = rf_grid)

set.seed(1234)
rf_resamples_mysql <- wf_rf_mysql %>%
  tune_grid(resamples = cv_train_mysql, grid = rf_grid)

set.seed(1234)
rf_resamples_postgresql <- wf_rf_postgresql %>%
    tune_grid(resamples = cv_train_postgresql, grid = rf_grid)

set.seed(1234)
xgb_resamples <- wf_xgb %>%
    tune_grid(resamples = cv_train, grid = xgb_grid)

set.seed(1234)
xgb_resamples_mssqlserver <- wf_xgb_mssqlserver %>%
    tune_grid(resamples = cv_train_mssqlserver, grid = xgb_grid)

set.seed(1234)
xgb_resamples_mysql <- wf_xgb_mysql %>%
  tune_grid(resamples = cv_train_mysql, grid = xgb_grid)

set.seed(1234)
xgb_resamples_postgresql <- wf_xgb_postgresql %>%
    tune_grid(resamples = cv_train_postgresql, grid = xgb_grid)


####################################################################################
###      Explore the results and choose the best hyper-parameter combination     ###

# performance metrics (mean) across folds for each grid line

rf_resamples |> collect_metrics()
autoplot(rf_resamples) 

rf_resamples_mssqlserver |> collect_metrics()
autoplot(rf_resamples_mssqlserver) 

rf_resamples_mysql |> collect_metrics()
autoplot(rf_resamples_mysql) 

rf_resamples_postgresql |> collect_metrics()
autoplot(rf_resamples_postgresql) 


xgb_resamples |> collect_metrics()
autoplot(xgb_resamples)

xgb_resamples_mssqlserver |> collect_metrics()
autoplot(xgb_resamples_mssqlserver)

xgb_resamples_mysql |> collect_metrics()
autoplot(xgb_resamples_mysql)

xgb_resamples_postgresql |> collect_metrics()
autoplot(xgb_resamples_postgresql)


# choose the best hyper-parameter combination

best_rf <- rf_resamples |> select_best(metric = "roc_auc")
best_rf

best_rf_mssqlserver <- rf_resamples_mssqlserver |> select_best(metric = "roc_auc")
best_rf_mssqlserver

best_rf_mysql <- rf_resamples_mysql |> select_best(metric = "roc_auc")
best_rf_mysql

best_rf_postgresql <- rf_resamples_postgresql |> select_best(metric = "roc_auc")
best_rf_postgresql


best_xgb <- xgb_resamples |> select_best(metric = "roc_auc")
best_xgb

best_xgb_mssqlserver <- xgb_resamples_mssqlserver |> select_best(metric = "roc_auc")
best_xgb_mssqlserver

best_xgb_mysql <- xgb_resamples_mysql |> select_best(metric = "roc_auc")
best_xgb_mysql

best_xgb_postgresql <- xgb_resamples_postgresql |> select_best(metric = "roc_auc")
best_xgb_postgresql



####################################################################################
###         Finalize the workflows with the best performing parameters           ###

final_wf_rf <- wf_rf |>
    finalize_workflow(best_rf)

final_wf_rf_mssqlserver <- wf_rf_mssqlserver |>
    finalize_workflow(best_rf_mssqlserver)

final_wf_rf_mysql <- wf_rf_mysql |>
  finalize_workflow(best_rf_mysql)

final_wf_rf_postgresql <- wf_rf_postgresql |>
    finalize_workflow(best_rf_postgresql)


final_wf_xgb <- wf_xgb |>
    finalize_workflow(best_xgb)

final_wf_xgb_mssqlserver <- wf_xgb_mssqlserver |>
    finalize_workflow(best_xgb_mssqlserver)

final_wf_xgb_mysql <- wf_xgb_mysql |>
  finalize_workflow(best_xgb_mysql)

final_wf_xgb_postgresql <- wf_xgb_postgresql |>
    finalize_workflow(best_xgb_postgresql)



## fit the final models on the entire train data set
set.seed(1234)
final_rf_train <- final_wf_rf |>
    fit(data = train_tbl)

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
final_xgb_train <- final_wf_xgb |>
    fit(data = train_tbl)

set.seed(1234)
final_xgb_train_mssqlserver <- final_wf_xgb_mssqlserver |>
    fit(data = train_tbl_mssqlserver)

set.seed(1234)
final_xgb_train_mysql <- final_wf_xgb_mysql |>
  fit(data = train_tbl_mysql)

set.seed(1234)
final_xgb_train_postgresql <- final_wf_xgb_postgresql |>
    fit(data = train_tbl_postgresql)



####################################################################################
###         The moment of truth: model performance on the test data set          ###

### Function last_fit() fits the finalized workflow one last time
### to the training data and evaluates one last time on the testing data.

set.seed(1234)
test__rf <- final_wf_rf |> last_fit(splits)
test__rf |> collect_metrics()

set.seed(1234)
test__rf_mssqlserver <- final_wf_rf_mssqlserver |> last_fit(splits_mssqlserver)
test__rf_mssqlserver |> collect_metrics()

set.seed(1234)
test__rf_mysql <- final_wf_rf_mysql |> last_fit(splits_mysql)
test__rf_mysql |> collect_metrics()

set.seed(1234)
test__rf_postgresql <- final_wf_rf_postgresql |> last_fit(splits_postgresql)
test__rf_postgresql |> collect_metrics()


set.seed(1234)
test__xgb <- final_wf_xgb |> last_fit(splits)
test__xgb |> collect_metrics()

set.seed(1234)
test__xgb_mssqlserver <- final_wf_xgb_mssqlserver |> last_fit(splits_mssqlserver)
test__xgb_mssqlserver |> collect_metrics()

set.seed(1234)
test__xgb_mysql <- final_wf_xgb_mysql |> last_fit(splits_mysql)
test__xgb_mysql |> collect_metrics()

set.seed(1234)
test__xgb_postgresql <- final_wf_xgb_postgresql |> last_fit(splits_postgresql)
test__xgb_postgresql |> collect_metrics()


# the ROC AUC curse for the test set (new data)

g1 <- test__rf %>%
     collect_predictions() %>%
     roc_curve(query_completion, .pred_successful) %>%
     autoplot() +
     ggtitle('Random Forest') +
     theme(text = element_text(size = 13),
          plot.title = element_text(size = 15, hjust = 0.5),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(size = 14),
          #plot.caption = element_text(size = 11),
          #plot.caption = element_blank(),
          legend.text = element_text(size = 14))  +
          scale_fill_viridis(discrete = TRUE)
g1     

g2 <- test__xgb %>%
     collect_predictions() %>%
     roc_curve(query_completion, .pred_successful) %>%
     autoplot() +
     ggtitle('XGBoost') +
     theme(text = element_text(size = 13),
          plot.title = element_text(size = 15, hjust = 0.5),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(size = 14),
          #plot.caption = element_text(size = 11),
          #plot.caption = element_blank(),
          legend.text = element_text(size = 14))  +
          scale_fill_viridis(discrete = TRUE)
     
x <- g1 + g2 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("63 ROC_AUC classification_on_the_test_set.pdf",
     plot = x,  device = "pdf") #, width = 30, height = 15, units = "cm")



####################################################################################
###                     Model-level exploration/interpretation                   ###
####################################################################################


#####################################################################
###                       Prepare the explainer                   ###
set.seed(1234)
baked_train <- recipe_cl %>% prep() %>%
  bake (new_data = train_tbl) 
#glimpse(baked_train)
table(baked_train$query_completion)

set.seed(1234)
baked_train_mssqlserver <- recipe_cl_mssqlserver |> prep() |>
  bake (new_data = train_tbl_mssqlserver) 

set.seed(1234)
baked_train_mysql <- recipe_cl_mysql |> prep() |>
  bake (new_data = train_tbl_mysql) 

set.seed(1234)
baked_train_postgresql<- recipe_cl_postgresql |> prep() |>
  bake (new_data = train_tbl_postgresql) 



custom_predict <- function(object, newdata) {
  # pred <- predict(object, newdata, type = "prob")
  # response <- pred$.pred
  probabilities = predict(object, data = newdata, type='response')$predictions
  return(probabilities[,'successful'])
}


best_rf_mssqlserver
set.seed(1234)
rf_fitted_mssqlserver <- ranger(query_completion ~ .,
                                data = baked_train_mssqlserver,
                                mtry = 3, num.trees = 700, min.node.size = 4, probability = TRUE)

best_rf_mysql
set.seed(1234)
rf_fitted_mysql <- ranger(query_completion ~ .,
                          data = baked_train_mysql,
                          mtry = 3, num.trees = 700, min.node.size = 4, probability = TRUE)

best_rf_postgresql
set.seed(1234)
rf_fitted_postgresql <- ranger(query_completion ~ .,
                               data = baked_train_postgresql,
                               mtry = 3, num.trees = 700, min.node.size = 4, probability = TRUE)


set.seed(1234)
explain_rf_mssqlserver <- explain(
    model = rf_fitted_mssqlserver,
    data =  baked_train_mssqlserver %>% select (-query_completion),
    y = baked_train_mssqlserver$query_completion == "successful",
    predict_function = custom_predict,
    type = 'classification',
    label = "Random Forest",
    probability = TRUE)

set.seed(1234)
explain_rf_mysql <- explain(
    model = rf_fitted_mysql,
    data =  baked_train_mysql %>% select (-query_completion),
    y = baked_train_mysql$query_completion == "successful",
    predict_function = custom_predict,
    type = 'classification',
    label = "Random Forest",
    probability = TRUE)

set.seed(1234)
explain_rf_postgresql <- explain(
    model = rf_fitted_postgresql,
    data =  baked_train_postgresql %>% select (-query_completion),
    y = baked_train_postgresql$query_completion == "successful",
    predict_function = custom_predict,
    type = 'classification',
    label = "Random Forest",
    probability = TRUE)


# DALEX::model_performance(explain_rf)
DALEX::model_performance(explain_rf_mssqlserver)
DALEX::model_performance(explain_rf_mysql)
DALEX::model_performance(explain_rf_postgresql)



###########################################################################
##       Variable importance with `ingredients::feature_importance`


set.seed(1234)
vi_rf_fi_mssqlserver  <- ingredients::feature_importance(
    explain_rf_mssqlserver,
    loss_function = DALEX::loss_one_minus_auc)
describe(vi_rf_fi_mssqlserver)

set.seed(1234)
vi_rf_fi_mysql  <- ingredients::feature_importance(
    explain_rf_mysql,
    loss_function = DALEX::loss_one_minus_auc)
describe(vi_rf_fi_mysql)

set.seed(1234)
vi_rf_fi_postgresql  <- ingredients::feature_importance(
    explain_rf_postgresql,
    loss_function = DALEX::loss_one_minus_auc)
describe(vi_rf_fi_postgresql)

g1 <- plot(vi_rf_fi_mssqlserver, max_vars = 10) + 
    theme_bw() + 
    theme(legend.position = 'none') +      
    ggtitle("Permutation-Based Variable Importance", 
          "Random Forest - MS SQL Server") + 
    theme_bw() +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(axis.text = element_text(hjust = 0.5, size = 11, angle = 0)) +
    #     theme(strip.text = element_text(size = 11)) 
    theme(strip.text = element_blank())

g2 <- plot(vi_rf_fi_mysql, max_vars = 10) + 
    theme_bw() + 
    theme(legend.position = 'none') +      
    ggtitle("Permutation-Based Variable Importance", 
          "Random Forest - MySQL") + 
    theme_bw() +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(axis.text = element_text(hjust = 0.5, size = 11, angle = 0)) +
    #     theme(strip.text = element_text(size = 11)) 
    theme(strip.text = element_blank())

g3 <- plot(vi_rf_fi_postgresql, max_vars = 10) + 
    theme_bw() + 
    theme(legend.position = 'none') +      
    ggtitle("Permutation-Based Variable Importance", 
          "Random Forest - PostgreSQL") + 
    theme_bw() +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 11)) +
    theme(axis.text = element_text(hjust = 0.5, size = 11, angle = 0)) +
    #     theme(strip.text = element_text(size = 11)) 
    theme(strip.text = element_blank())

x <- g1 + g2 + g3 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("64 classification - top 10 variable importance - rf.pdf",  plot = x,
       width = 45, height = 15, units = "cm") 


###########################################################################
##       Partial Depencence Plots and Accumulated Dependence Plots


pd_model_mssqlserver <- partial_dependence(explain_rf_mssqlserver, 
    variables = c("FROM_join_paths", "normal_form", "scale_factor_X1", "WHERE_predicates"))
pd_model_mssqlserver$`_label_` = "PDP"

ad_model_mssqlserver <- accumulated_dependence(explain_rf_mssqlserver, 
    variables = c("FROM_join_paths", "normal_form", "scale_factor_X1", "WHERE_predicates"))
ad_model_mssqlserver$`_label_` = "ALE"

g1 <- plot(pd_model_mssqlserver, ad_model_mssqlserver) +
    ggtitle("Feature Effects (RF) - MS SQL Server", "") +
    theme_bw() + 
    theme(legend.position = 'right') +      
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    theme(axis.text = element_text(hjust = 0.5, size = 10, angle = 0)) +
    theme(strip.text = element_text(size = 10)) 
g1


pd_model_mysql <- partial_dependence(explain_rf_mysql, 
    variables = c("FROM_join_paths", "normal_form", "scale_factor_X1", "WHERE_predicates"))
pd_model_mysql$`_label_` = "PDP"

ad_model_mysql <- accumulated_dependence(explain_rf_mysql, 
    variables = c("FROM_join_paths", "normal_form", "scale_factor_X1", "WHERE_predicates"))
ad_model_mysql$`_label_` = "ALE"

g2 <- plot(pd_model_mysql, ad_model_mysql) +
    ggtitle("Feature Effects (RF) - MySQL", "") +
    theme_bw() + 
    theme(legend.position = 'right') +      
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    theme(axis.text = element_text(hjust = 0.5, size = 10, angle = 0)) +
    theme(strip.text = element_text(size = 10)) 
g2

pd_model_postgresql <- partial_dependence(explain_rf_postgresql, 
    variables = c("FROM_join_paths", "normal_form", "scale_factor_X1", "WHERE_predicates"))
pd_model_postgresql$`_label_` = "PDP"

ad_model_postgresql <- accumulated_dependence(explain_rf_postgresql, 
    variables = c("FROM_join_paths", "normal_form", "scale_factor_X1", "WHERE_predicates"))
ad_model_postgresql$`_label_` = "ALE"


g3 <- plot(pd_model_postgresql, ad_model_postgresql) +
    ggtitle("Feature Effects (RF) - PostgreSQL", "") +
    theme_bw() + 
    theme(legend.position = 'none') +      
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +
    theme(axis.text = element_text(hjust = 0.5, size = 10, angle = 0)) +
    theme(strip.text = element_text(size = 10)) 

x <- g1 + g2 + g3 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("65 classification-Feature effects - PDP_ ALE - rf.pdf",  plot = x,
       width = 45, height = 15, units = "cm") 

