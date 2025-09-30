####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###           2a. Inferential Statistics for the Classification Model            ###
####################################################################################
# last update: 2025-09-23
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
library(RVAideMemoire)


# By default, the current directory is associated to the folder when cloning
#  the repo from github.
# Uncomment and execute the next line if you want to change the default/current directory,
# setwd('here is the path to your current directory')

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

df_classif <- main_df |>
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
  select(dbserver, scale_factor, normal_form, query_completion, 
         SELECT_cols:SELECT_aggr_func, FROM_join_paths, FROM_joins, FROM_table_size, 
         WHERE_predicates:offset) 

df_classif_mssqlserver <- df_classif_init |>
     ungroup() |>
     filter(dbserver == 'mssqlserver') 


df_classif_mysql <- df_classif_init |>
  ungroup() |>
  filter(dbserver == 'mysql') 

df_classif_postgresql <- df_classif_init |>
     ungroup() |>
     filter(dbserver == 'postgresql') 
     


setwd(paste(base_path, 'figures', sep = '/'))



####################################################################################
###             RQ1. Association between query_completion and normal_form        ###
####################################################################################
glimpse(df_classif)
#table(df_classif$dbserver)


# Cross-tabulation
xtabs(~query_completion + normal_form, df_classif)


# Compare the proportion of success between treatments


# ... for mssqlserver
cochran_qtest(df_classif_mssqlserver |>
                   select(query_completion, normal_form, query_id), 
              query_completion ~ normal_form|query_id)
# results:
# n = 1966
# statistic = 477.
# df = 2
# p = 2.96e-104 
# method = Cochran's Q test


# ... for mysql
cochran_qtest(df_classif_mysql |>
                select(query_completion, normal_form, query_id), 
              query_completion ~ normal_form|query_id)
# results:
# n = 2000
# statistic = 540
# df = 2
# p = 6.52e-118 
# method = Cochran's Q test


# ... for postgresql
cochran_qtest(df_classif_postgresql |>
                   select(query_completion, normal_form, query_id), 
              query_completion ~ normal_form|query_id)
# results:
# n = 2000
# statistic = 679.
# df = 2
# p = 3.40e-148
# method = Cochran's Q test



options(scipen = 0)


# pairwise comparisons between groups
pairwise_cochran_mssqlserver <- pairwise_mcnemar_test(
     df_classif_mssqlserver,
     query_completion ~ normal_form|query_id)

pairwise_cochran_mssqlserver_01 <- pairwise_mcnemar_test(
  df_classif_mssqlserver |> filter(scale_factor == 0.1),
  query_completion ~ normal_form|query_id)

pairwise_cochran_mssqlserver_1 <- pairwise_mcnemar_test(
  df_classif_mssqlserver |> filter(scale_factor == 1),
  query_completion ~ normal_form|query_id)


pairwise_cochran_mysql <- pairwise_mcnemar_test(
  df_classif_mysql,
  query_completion ~ normal_form|query_id)

pairwise_cochran_mysql_01 <- pairwise_mcnemar_test(
  df_classif_mysql |> filter(scale_factor == 0.1),
  query_completion ~ normal_form|query_id)

pairwise_cochran_mysql_1 <- pairwise_mcnemar_test(
  df_classif_mysql |> filter(scale_factor == 1),
  query_completion ~ normal_form|query_id)



pairwise_cochran_postgresql <- pairwise_mcnemar_test(
     df_classif_postgresql , 
     query_completion ~ normal_form|query_id)

pairwise_cochran_postgresql_01 <- pairwise_mcnemar_test(
  df_classif_postgresql |> filter(scale_factor == 0.1) , 
  query_completion ~ normal_form|query_id)

pairwise_cochran_postgresql_1 <- pairwise_mcnemar_test(
  df_classif_postgresql |> filter(scale_factor == 1) , 
  query_completion ~ normal_form|query_id)

options(scipen = 999)

rio::export(pairwise_cochran_mssqlserver, file = '41a pairwise_cochran_mssqlserver.xlsx')
rio::export(pairwise_cochran_mysql, file = '41b pairwise_cochran_mysql.xlsx')
rio::export(pairwise_cochran_postgresql, file = '41c pairwise_cochran_postgresql.xlsx')




# Pearson’s ϕ (phi()) and Cramér’s V (cramers_v()) can be used to estimate 
# the strength of association between two categorical variables (Cramér 1946), 
# while Cohen’s g (cohens_g()) estimates the deviance between paired 
# categorical variables (Cohen 1988).


ggbarstats(
     data = df_classif_mssqlserver,
     x = query_completion, 
     y = normal_form, 
     paired = TRUE,
     label = 'both'
)

ggbarstats(
     data = df_classif_postgresql,
     x = query_completion, 
     y = normal_form, 
     paired = TRUE,
     label = 'both'
)


ggbarstats(
     data = df_classif_mssqlserver,
     x = query_completion, 
     y = normal_form, 
     paired = FALSE,
     label = 'both'
)
effectsize::interpret_cramers_v(0.17)


ggbarstats(
     data = df_classif_postgresql,
     x = query_completion, 
     y = normal_form, 
     paired = FALSE,
     label = 'both'
)
effectsize::interpret_cramers_v(0.21)



g1 <- ggstatsplot::ggbarstats(
     data = df_classif |> mutate(scale_factor = paste0('SF ', scale_factor, 
                                                       ' GB')),
     x = query_completion, 
     y = scale_factor, 
     paired = FALSE,
     label = 'both'
          ) +
          theme(legend.position = 'right') +
          ggtitle('Query completion vs. Scale factor') +
          #xlab("") +
#     theme(axis.text.x = element_text(size = 11)) +
          theme(axis.text = element_text(size = 14)) +
          theme(text = element_text(size = 14),
               plot.title = element_text(size = 14, hjust = 0.5),
               plot.subtitle = element_text(size = 13, hjust = 0.5),
               legend.title = element_text(size = 13),
               #plot.caption = element_text(size = 11),
               plot.caption = element_blank(),
               legend.text = element_text(size = 12))  +
               scale_fill_viridis(discrete = TRUE)

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave(paste0("42 query_completion vs scale_factor.pdf"),
          plot = x,  device = "pdf")

interpret_cramers_v(0.14)


