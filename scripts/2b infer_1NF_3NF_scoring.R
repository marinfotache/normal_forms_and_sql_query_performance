####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###                   2b. Inferential Statistics for the Scoring Model            ###
####################################################################################
# last update: 2025-09-24
options(scipen = 999)
library(tidyverse)
library(scales)
library(patchwork)
library(viridis)
library(ggsci)
library(svglite)
#install.packages('ggstatsplot')
library(rstatix)
citation('rstatix')
library(ggstatsplot)
library(effectsize)
#citation('ggstatsplot')



# uncomment and execute the next line to set the default/current directory according to your local confoguration
# setwd('here is the path to your current directory')

base_path <- getwd()


####################################################################################
###                           Load data & prepare df_scoring
####################################################################################

setwd(paste(base_path, 'data', sep = '/'))
load(file = 'NF_final_dataset_2025-09.RData')
glimpse(main_df)

df_scoring_duration <- main_df |>
  filter(completed == 'successful') |>
  semi_join(
    main_df |>
      filter(completed == 'successful') |>
      group_by(query_id) |>
      tally() |>
      filter (n == 9) |>   ### ???????????  6 instead of 3 !!!!!
      distinct(query_id) |>
      ungroup()
  ) |>
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
  select(dbserver, query_id, scale_factor, normal_form, duration_sec, 
         SELECT_cols:SELECT_aggr_func, FROM_join_paths, FROM_joins, FROM_table_size, 
         WHERE_predicates:offset) |>
  #select(-query_id) |>
  #     select(-scale_factor) |>
  mutate(duration_sec = if_else(duration_sec == 0, 0.0001, duration_sec)) 
# mutate(log10_duration = log10(duration_sec)) |>
# select(-duration_sec, -SELECT_n_of_all_aggr_func)


anyNA(df_scoring_duration)

## correlation plot
corrplot::corrplot(cor(
    df_scoring_duration |>
        select(-query_id) |>
            select_if(is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)


df_scoring_duration_mssqlserver <- df_scoring_duration |>
    ungroup() |>
    filter(dbserver == 'mssqlserver') |>
    convert_as_factor( normal_form, query_id)

df_scoring_duration_mysql <- df_scoring_duration |>
    ungroup() |>
    filter(dbserver == 'mysql') |>
    convert_as_factor( normal_form, query_id)

df_scoring_duration_postgresql <- df_scoring_duration |>
    ungroup() |>
    filter(dbserver == 'postgresql') |>
    convert_as_factor( normal_form, query_id)


df_scoring_duration_mssqlserver |>
     group_by(scale_factor, normal_form) |>
     count()

df_scoring_duration_mysql |>
  group_by(scale_factor, normal_form) |>
  count()

df_scoring_duration_postgresql |>
     group_by(scale_factor, normal_form) |>
     count()


setwd(paste(base_path, 'figures', sep = '/'))



####################################################################################
###            RQ3. Association between normal form and query_duration           ###
####################################################################################

###                         mssqlserver

df_scoring_duration_mssqlserver |>
    group_by(scale_factor, normal_form) |>
    get_summary_stats(duration_sec, type = "common")

fr_test_mssqlserver <- df_scoring_duration_mssqlserver |>
     friedman_test(duration_sec ~ normal_form | query_id)
fr_test_mssqlserver 

df_scoring_duration_mssqlserver |>
    friedman_effsize(duration_sec ~ normal_form | query_id)

interpret_kendalls_w(0.31)
# Kendall’s W uses the Cohen’s interpretation guidelines of 0.1 - < 0.3 (small effect), 
# 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect). 
# Confidence intervals are calculated by bootstrap.


# Pairwise comparisons using paired Wilcoxon signed-rank test. 
# P-values are adjusted using the Bonferroni multiple testing correction method.
pwc_mssqlserver <- df_scoring_duration_mssqlserver |>
     wilcox_test(duration_sec ~ normal_form, paired = TRUE, p.adjust.method = "bonferroni")
pwc_mssqlserver



### MySQL


fr_test_mysql <- df_scoring_duration_mysql |>
  friedman_test(duration_sec ~ normal_form | query_id)
fr_test_mysql 

fr_test_mysql <- df_scoring_duration_mysql |>
  friedman_test(duration_sec ~ normal_form | query_id)
fr_test_mysql 

pwc_mysql <- df_scoring_duration_mysql |>
  wilcox_test(duration_sec ~ normal_form, paired = TRUE, p.adjust.method = "bonferroni")
pwc_mysql



### PostgreSQL

df_scoring_duration_postgresql |>
  group_by(scale_factor, normal_form) |>
  get_summary_stats(duration_sec, type = "common")

fr_test_postgresql <- df_scoring_duration_postgresql |>
  friedman_test(duration_sec ~ normal_form | query_id)
fr_test_postgresql 

df_scoring_duration_postgresql |>
  friedman_effsize(duration_sec ~ normal_form | query_id)

interpret_kendalls_w(0.272)
# Kendall’s W uses the Cohen’s interpretation guidelines of 0.1 - < 0.3 (small effect), 
# 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect). 
# Confidence intervals are calculated by bootstrap.


# Pairwise comparisons using paired Wilcoxon signed-rank test. 
# P-values are adjusted using the Bonferroni multiple testing correction method.
pwc_postgresql <- df_scoring_duration_postgresql |>
  wilcox_test(duration_sec ~ normal_form, paired = TRUE, p.adjust.method = "bonferroni")
pwc_postgresql



################################################################################

set.seed(1234) 
g1a <- ggwithinstats(
     data = df_scoring_duration_mssqlserver,
     x = normal_form,
     y = duration_sec, 
     type = 'np'
     ) +
     ggtitle('Query Duration vs. Normal Form - MS SQL Server') +
    theme(text = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 12),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11)) +
    scale_fill_viridis()     
g1a

set.seed(1234) 
g1b <- ggwithinstats(
      data = df_scoring_duration_mysql,
      x = normal_form,
      y = duration_sec, 
      type = 'np'
    ) +
    ggtitle('Query Duration vs. Normal Form - MySQL') +
    theme(text = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 12),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11)) +
    scale_fill_viridis()     
g1b

g1c <- ggwithinstats(
      data = df_scoring_duration_postgresql,
      x = normal_form,
      y = duration_sec, 
      type = 'np'
    ) +
    ggtitle('Query Duration vs. Normal Form - PostgreSQL') +
    theme(text = element_text(size = 12),
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 12),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11)) +
    scale_fill_viridis()     

x <- g1a + g1b + g1c + plot_layout(nrow = 2, byrow = FALSE)
x
ggsave("50 Association duration vs normal form.pdf", plot = x,  device = "pdf")
