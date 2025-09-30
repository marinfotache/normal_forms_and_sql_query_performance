####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###      1a. EDA for 1NF,2NF and 3NF schemas (1). Schema and query info          ###
####################################################################################
# last update: 2025-09-29
options(scipen = 999)

library(tidyverse)
library(corrplot)
library(rio)
library(scales)
library(patchwork)
library(systemfonts) ## custom fonts
library(gtsummary)
library(flextable)

setwd('/Users/marinfotache/Dropbox/2024-03 Normal Forms and SQL Query Performance/git_repo/normal_forms_and_sql_query_performance')

base_path <- getwd()


####################################################################################
###                                      Load data
####################################################################################
setwd(paste(base_path, 'data', sep = '/'))

load(file = 'all_table_statistics.RData')
glimpse(all_table_statistics)
load(file = 'schemas_statistics.RData')

load(file = 'NF_final_dataset_2025-09.RData')



####################################################################################
###                          Info about table & schema size
####################################################################################
setwd(paste(base_path, 'figures', sep = '/'))

table_statistics <- all_table_statistics |>
     filter(scale_factor %in% c(.1, 1)) |>
     filter(schema_no %in% c(1, 513, 1024)) |>
     mutate(normal_form = case_when(
          schema_no == 1 ~ '1NF',
          schema_no == 513 ~ '2NF',
          schema_no == 1024 ~ '3NF',
          .default = 'ERROR'
     )) |>
     select(-schema_no) |>
     relocate(normal_form, .after = scale_factor) |>
     arrange(scale_factor, normal_form, desc(average_record_size))

rm(all_table_statistics)
rio::export(table_statistics, file = '01a_table_statistics_123NF_sf_01_1.xlsx')

schema_statistics <- schemas_statistics |>
     filter(scale_factor %in% c(0.1, 1)) |>
     filter(schema_no %in% c(1, 513, 1024)) |>
     mutate(normal_form = case_when(
          schema_no == 1 ~ '1NF',
          schema_no == 513 ~ '2NF',
          schema_no == 1024 ~ '3NF',
          .default = 'ERROR'
     )) |>
     select(-schema_no) |>
     relocate(normal_form, .after = scale_factor) |>
     arrange(normal_form) |>
     pivot_longer(cols = schema_size:size_of_the_largest_table_in_crt_schema, 
                  names_to = "schema_metric", values_to = "metric_value") |>
     mutate(metric_value = round(metric_value, 0)) |>
     arrange(schema_metric, scale_factor, normal_form) |>
     mutate(scale_factor = paste0('SF', scale_factor)) |>
     pivot_wider(names_from = normal_form, values_from = metric_value) |>
     arrange(scale_factor, schema_metric)

rio::export(schema_statistics, file = '01b_schema_statistics_123NF_sf_01_1.xlsx')



####################################################################################
###            Info about constant (among normal forms) query parameters
####################################################################################

df_constant_query_parameters <- df_info_select |>
     inner_join(df_info_from |>
                     distinct(query_id, FROM_n_of_join_paths)) |>
     inner_join(df_info_where) |>
     inner_join(df_info_group_by) |>
     inner_join(df_info_having) |>
     inner_join(df_info_order_by) |>
     inner_join(df_info_limit_skip) 

glimpse(df_constant_query_parameters)

df_constant_query_parameters <- df_constant_query_parameters |>
     set_names( str_remove_all(names(df_constant_query_parameters), 
          'n_of_|operators_|_all|umn')) |>
     rename(
          SELECT_non_aggr_func = SELECT_all_non_aggr_func,
          SELECT_aggr_func = SELECT_all_aggr_func,
          WHERE_non_aggr_func = WHERE_all_non_aggr_func,
          WHERE_func__date = WHERE_non_aggr_func__date,
          HAVING_non_scalar_subq = HAVING_main_predicates__non_scalar_subquery,
          HAVING_scalar_subq = HAVING_main_predicates__scalar_subquery)

glimpse(df_constant_query_parameters)

df_constant_query_parameters_new <- df_constant_query_parameters |>
     set_names( str_remove_all(names(df_constant_query_parameters), 
          'n_of_|operators_|_all|umn')) |>
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
          ) 

glimpse(df_constant_query_parameters_new)


###############################################################################
###     Table 1: Descriptive statistics for constant query parameters
table1 <- df_constant_query_parameters_new |>
     select (-query_id) |>
     mutate(scale_factor = factor(scale_factor)) |>
     tbl_summary(
          by = scale_factor, 
          type = list(
               c(
                    SELECT_cols,
                    SELECT_non_aggr_func,
                    #SELECT_SUBSTR,
                    #SELECT_date_func,
                    SELECT_aggr_func,
                    FROM_join_paths,
                    WHERE_predicates,
                    WHERE_pkey_attribs,
                    #WHERE__between,
                    #WHERE__in,
                    #WHERE__like,
                    #WHERE_non_aggr_func,
                    #WHERE_func__date,
                    GROUP_BY_cols,
                    HAVING_non_scalar_subq,
                    HAVING_scalar_subq,
                    ORDER_BY_cols,
                    limit,
                    offset
                   ) ~ 'continuous'),
          statistic = list(all_continuous() ~ c("{median} [{min}, {max}]" )),
          digits = all_continuous() ~ 0) |>
          add_p(list(all_continuous() ~ "wilcox.test")) |>
          modify_header(label ~ "**Variable**") |>
          bold_labels() |>
          modify_spanning_header(c("stat_1", "stat_2") ~ 
                                      "**Scale Factor (GB)**")     

table1 |> as_flex_table() |> border_inner_h() |> 
     save_as_image(path = "11a_table_constant_query_parameters.svg")
table1 |> as_flex_table() |>  
     save_as_docx(path = "11a_table_constant_query_parameters.docx")


###############################################################################
###     Distribution of constant query parameters for SF1 and 3NF
###     !!!   All variables are numerical !!!
###############################################################################
# glimpse(df_constant_query_parameters_new)
# summary(df_constant_query_parameters)
# 
# df_constant_query_parameters_sf1_3fn <- df_constant_query_parameters_new |>
#      filter(scale_factor == 1) |>
#      select(-scale_factor)
# glimpse(df_constant_query_parameters_sf1_3fn)


###############################################################################
###                      Data distribution - nominal variables              ###

# compute the frequencies for each categorical variables and values
eda_factors <- df_constant_query_parameters_new %>%
     mutate (scale_factor  = factor(scale_factor)) %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(df_constant_query_parameters_new),2)) %>%
     arrange(variable, value) 

#View(eda_factors)

g1 <- eda_factors %>%
     filter (!variable %in% c('normal_form', 'query_completion',
                              'scale_factor')) %>%
     group_by(variable) %>%
     summarise(n_of_values = n()) %>%
     ungroup() %>%
     select (variable) %>%
     inner_join(eda_factors) %>%
ggplot(., aes(x = value, y = n_value)) +
     geom_col(alpha = 0.4) +
     geom_text (aes(label = paste0(value, ' - ', round(percent,0), '%'), 
                  vjust = if_else(n_value > 100, 1.5, -0.5))) +
     facet_wrap(~ variable, scale = "free", nrow = 2) +
     theme_bw() +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 10, angle = 35, hjust = 1)) +
     theme(strip.text.x = element_text(size = 11)) +
     xlab("") + ylab("frequency") 


x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("12a_nominal_constant_query_parameters.pdf", plot = x,  #device = cairo_pdf(), 
       width = 30, height = 25, units = "cm", dpi = 600)




###############################################################################
###            Data distribution - numerical variables  - overall           ###
num_variables <- df_constant_query_parameters_new %>%
     select(-query_id) %>%
     select_if(., is.numeric ) %>%
#     select(-scale_factor, -FROM_schema_size) |>
     mutate(row_num = row_number()) %>%
     pivot_longer(-row_num, names_to = "variable", values_to = "value" ) 
#View(num_variables)

num_variables_distinct_values <- num_variables %>%
     group_by(variable) %>%
     summarise(n_distinct_values  = n_distinct(value)) %>%
     ungroup()


###############################################################################
#    for variables with less of equal 11 distinct values, display bar plots
g1 <- num_variables %>%
     semi_join(
          num_variables_distinct_values %>%
               filter (n_distinct_values <= 11)
     ) %>%
     ungroup() %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / 
                    nrow(df_constant_query_parameters_new),2)) %>%
     arrange(variable, value) %>%
ggplot(., aes(x = value, y = n_value)) +
     geom_col(alpha = 0.4) +
     geom_text (aes(label = paste0(round(percent,0), '%'), 
               vjust = if_else(n_value > 300, 1.5, -0.5)), size = 3) +
     facet_wrap( variable ~ ., scale = "free", ncol = 3) +
     theme_bw() +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5)) +
     theme(strip.text = element_text(size = 9)) +
     xlab("") + ylab("frequency") +
     scale_x_continuous(breaks= pretty_breaks())     

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("13a_low_cardinality_constant_query_parameters.pdf", plot = x,  #device = cairo_pdf(), 
       width = 30, height = 25, units = "cm", dpi = 600)



###############################################################################
#    for variables with more than 11 distinct values, display bar histogram
g1 <- num_variables %>%
     semi_join(
          num_variables_distinct_values %>%
               filter (n_distinct_values > 11)
     ) %>%
     ungroup() %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(df_constant_query_parameters_new),2)) %>%
     arrange(variable, value) %>%
ggplot(., aes(value)) +
     geom_histogram(alpha = 0.6) +
     facet_wrap(variable ~ ., scale = "free", ncol = 4) +
     theme_bw() +
     theme(legend.position="none")    +
     theme(axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5)) +
     theme(strip.text = element_text(size = 9)) +
     xlab("") + ylab("frequency") +
     scale_x_continuous(breaks= pretty_breaks())     

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("13b_higher cardinality numeric variables - classification.pdf", plot = x,
       device = "pdf", width = 30, height = 10, units = "cm", dpi = 600)




####################################################################################
###            Info about variable (among normal forms) query parameters
####################################################################################


df_variable_query_parameters <- df_info_from |>
     select(-FROM_n_of_join_paths)

glimpse(df_variable_query_parameters)

df_variable_query_parameters <- df_variable_query_parameters |>
     set_names( str_remove_all(names(df_variable_query_parameters), 
          'n_of_|operators_|_all|umn')) |>
     rename(FROM_table_size = FROM_size_of_processed_tables) |>
     transmute(scale_factor, normal_form, query_id, 
               FROM_joins = FROM_INNER_joins + FROM_OUTER_joins,
               FROM_table_size)

glimpse(df_variable_query_parameters)

table2 <- df_variable_query_parameters |>
     group_by(scale_factor, normal_form) |>
     summarise(across(starts_with("FROM"), list(mean = mean,median = median)))
rio::export(table2, file = "14a joins and table size, by nf and sf.xlsx")

