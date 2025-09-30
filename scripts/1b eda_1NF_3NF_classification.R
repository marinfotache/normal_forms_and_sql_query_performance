####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###          1b. Exploratory Data Analysis for the Classification Model          ###
####################################################################################
# last update: 2025-09-23
options(scipen = 999)

library(tidyverse)
library(corrplot)
library(ggforce)
#
library(patchwork)


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
setwd(paste(base_path, 'figures', sep = '/'))


# all records, included postgresql queries not compatible with mssqlserver syntax
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
     
     
glimpse(df_classif)     

## check there is no collinearity (correlation plot)
corrplot::corrplot(cor(
     df_classif %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)



####################################################################################
###                      Query completion vs. other variables of interest
####################################################################################
glimpse(df_classif)

completed_a <- df_classif |>
     group_by(dbserver, scale_factor, normal_form, query_completion) |>
     tally() 

completed_b <- df_classif |>
     group_by(dbserver, scale_factor, normal_form, query_completion) |>
     tally() |>
     pivot_wider(names_from = query_completion, values_from = n)

glimpse(df_classif)

df_fig_results_classif_init <- df_classif |>
     group_by(dbserver, scale_factor = paste('SF', scale_factor), 
              normal_form, query_completion) |>
     summarise(n = n()) |>
     ungroup() 

df_fig_results_classif_interm <- df_fig_results_classif_init |>
     group_by(dbserver, scale_factor,  normal_form) |>
     summarise(n_total = sum(n)) |>
     ungroup() 

df_fig_results_classif <- df_fig_results_classif_init |>
     inner_join(df_fig_results_classif_interm) |>
     mutate(percent = round(n * 100 / n_total,2))


g1 <- ggplot(df_fig_results_classif, aes(x = normal_form, y = n, 
                                         fill = query_completion)) +
     geom_bar(stat="identity") +
#     geom_col(alpha = 0.4) +
     # geom_text (aes(label = paste0(round(percent,0), '%'), 
     #           vjust = if_else(n > 150, 1.5, -0.5)), size = 3) +
     geom_text (aes(label = paste0(round(percent,0), '%')), 
                position=position_stack(vjust=0.5),
                size = 4.5) +
#     facet_wrap( scale_factor ~ ., scale = "free", ncol = 2) +
     facet_grid(dbserver ~ scale_factor, scale = "free") +
     theme_bw() +
     theme(legend.position="bottom")    +
     theme(axis.text = element_text(size = 10, angle = 0, hjust = 0.5)) +
     theme(strip.text = element_text(size = 12)) +
     xlab("") + ylab("frequency") +
     scale_y_continuous(breaks= seq(0, 1000, by = 100))     


x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("20a_query_completion.pdf", plot = x, width = 18, height = 18, units = "cm", dpi = 600)


table_3b1 <- df_classif |>
     filter(scale_factor == 0.1 & dbserver == 'mssqlserver') |>
     mutate(scale_factor = paste0('SF: ',scale_factor, 'GB')) |>
     rename(`query completion for SF 0.1GB in mssqlserver` = query_completion) |>
     tbl_cross(row = `query completion for SF 0.1GB in mssqlserver`, 
               col = normal_form, percent = "cell", margin = "column") |>
     bold_labels()
table_3b1

table_3b2 <- df_classif |>
     filter(scale_factor == 1 & dbserver == 'mssqlserver') |>
     mutate(scale_factor = paste0('SF: ',scale_factor, 'GB')) |>
     rename(`query completion for SF 1GB in mssqlserver` = query_completion) |>
     tbl_cross(row = `query completion for SF 1GB in mssqlserver`, 
               col = normal_form, percent = "cell", margin = "column") |>
     bold_labels()
table_3b2


table_3b3 <- df_classif |>
     filter(scale_factor == 0.1 & dbserver == 'postgresql') |>
     mutate(scale_factor = paste0('SF: ',scale_factor, 'GB')) |>
     rename(`query completion for SF 0.1GB in postgresql` = query_completion) |>
     tbl_cross(row = `query completion for SF 0.1GB in postgresql`, 
               col = normal_form, percent = "cell", margin = "column") |>
     bold_labels()
table_3b3

table_3b4 <- df_classif |>
     filter(scale_factor == 1 & dbserver == 'postgresql') |>
     mutate(scale_factor = paste0('SF: ',scale_factor, 'GB')) |>
     rename(`query completion for SF 1GB in postgresql` = query_completion) |>
     tbl_cross(row = `query completion for SF 1GB in postgresql`, 
               col = normal_form, percent = "cell", margin = "column") |>
     bold_labels()
table_3b4

table_3b5 <- df_classif |>
  filter(scale_factor == 0.1 & dbserver == 'mysql') |>
  mutate(scale_factor = paste0('SF: ',scale_factor, 'GB')) |>
  rename(`query completion for SF 0.1GB in mysql` = query_completion) |>
  tbl_cross(row = `query completion for SF 0.1GB in mysql`, 
            col = normal_form, percent = "cell", margin = "column") |>
  bold_labels()
table_3b5

table_3b6 <- df_classif |>
  filter(scale_factor == 1 & dbserver == 'mysql') |>
  mutate(scale_factor = paste0('SF: ',scale_factor, 'GB')) |>
  rename(`query completion for SF 1GB in mysql` = query_completion) |>
  tbl_cross(row = `query completion for SF 1GB in mysql`, 
            col = normal_form, percent = "cell", margin = "column") |>
  bold_labels()
table_3b6

table_3b <- tbl_stack(list(table_3b1, table_3b2, table_3b3, table_3b4,
                           table_3b5, table_3b6))
table_3b


table_3b |>
     as_flex_table() |>
#     border_inner_h() |>    
#     border_inner() |>    
     #bold(., ~ `p.value` < 0.05, ~ `p.value`, bold = TRUE) |>
     save_as_image(path = "20b_query_completion_vs_normal_forms.svg")

table_3b |>
  as_flex_table() |>
  #     border_inner_h() |>    
  #     border_inner() |>    
  #bold(., ~ `p.value` < 0.05, ~ `p.value`, bold = TRUE) |>
  save_as_docx(path = "20b_query_completion_vs_normal_forms.docx")





