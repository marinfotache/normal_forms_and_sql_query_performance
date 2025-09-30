####################################################################################
###  Query Performance & Normal Form (TPC-H) in MS-SQL Server, MySQL, PostgreSQL ###
###      In all schemas, indexes were created for all primary and foreign keys   ###
####################################################################################
###        1c. Exploratory Data Analysis for the Scoring/Regression Model        ###
####################################################################################
# last update: 2025-09-24

options(scipen = 999)


library(tidyverse)
library(corrplot)
library(patchwork)
library(scales)


# uncomment and execute the next line to set the default/current directory according to your local confoguration
# setwd('here is the path to your current directory')

base_path <- getwd()


####################################################################################
###                           Load data & prepare df_scoring
####################################################################################

setwd(paste(base_path, 'data', sep = '/'))
load(file = 'NF_final_dataset_2025-09.RData')
glimpse(main_df)

table(main_df$dbserver)
table(main_df$dbserver, main_df$scale_factor)
table(main_df$dbserver, main_df$scale_factor, main_df$normal_form)


test0 <- main_df |>
     arrange(desc(duration_sec))


test <- main_df |>
     group_by(dbserver, scale_factor, normal_form, query_id) |>
     tally() |>
     ungroup() |>
     filter(n != 1)

test <- main_df |>
     filter(dbserver == 'mssqlserver' & scale_factor == 0.1 & normal_form == '2NF')



setwd(paste(base_path, 'figures', sep = '/'))

main_df |>
     count(completed)

test <- main_df |>
     filter(completed == 'successful' & is.na(duration_sec)) 


df_scoring_duration <- main_df |>
     filter(completed == 'successful') |>
     semi_join(
          main_df |>
               filter(completed == 'successful') |>
               group_by(query_id) |>
               tally() |>
               filter (n == 9) |>   ### carefull with the number of dbservers and scale factors!
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
     select(-query_id) |>
#     select(-scale_factor) |>
     mutate(duration_sec = if_else(duration_sec == 0, 0.0001, duration_sec)) 
     # mutate(log10_duration = log10(duration_sec)) |>
     # select(-duration_sec, -SELECT_n_of_all_aggr_func)

anyNA(df_scoring_duration)

glimpse(df_scoring_duration)

ds <- df_scoring_duration |>
     group_by(dbserver, scale_factor, normal_form) |>
     summarise(
          first_q = quantile(duration_sec, .25),
          median = quantile(duration_sec, .50),
          mean = mean(duration_sec),
          third_q = quantile(duration_sec, .75),
          max = max(duration_sec) )|>
     ungroup()

rio::export(ds, file = '30_stats_duration.xlsx')


min(df_scoring_duration$duration_sec)
test <- df_scoring_duration |>
     filter(duration_sec == 0)

df_scoring_log10duration <- main_df |>
  filter(completed == 'successful') |>
  semi_join(
    main_df |>
      filter(completed == 'successful') |>
      group_by(query_id) |>
      tally() |>
      filter (n == 9) |>   ### 
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
  select(-query_id) |>
  #     select(-scale_factor) |>
  mutate(duration_sec = if_else(duration_sec == 0, 0.0001, duration_sec)) |>
  mutate(log10_duration = log10(duration_sec)) |>
     select(-duration_sec)

#df_scoring <- df_scoring_log10duration
glimpse(df_scoring_log10duration)


table(df_scoring_log10duration$scale_factor, df_scoring_log10duration$dbserver, df_scoring_log10duration$normal_form)




g1 <- df_scoring_duration |>
     ggplot(aes(duration_sec, col = normal_form)) +
          geom_density() +
          facet_wrap( ~ scale_factor + dbserver)
g1

g2 <- df_scoring_duration |>
     ggplot(aes(x = normal_form, y = duration_sec)) +
          geom_boxplot() +
          facet_wrap( ~ scale_factor + dbserver)
g2


g3a <- df_scoring_duration |>
     filter(scale_factor == 0.1 & dbserver == 'mssqlserver') |>
     ggplot(aes(x = normal_form, y = duration_sec)) +
          geom_boxplot() +
          #scale_y_continuous(breaks = seq(0, 1800, by = 100)) +
        scale_y_continuous(limits= c(0, 1600)) +
          ggforce::facet_zoom(ylim = c(0, 20)) +
          ggtitle('SF 0.1GB - MS SQL Server') +
          theme(axis.text = element_text(size = 11)) +
          theme(text = element_text(size = 11),
               plot.title = element_text(size = 12, hjust = 0.5),
               plot.subtitle = element_text(size = 11.5, hjust = 0.5),
               legend.title = element_text(size = 11),
               #plot.caption = element_text(size = 11),
               plot.caption = element_blank(),
               legend.text = element_text(size = 11))  
g3a

g3b <- df_scoring_duration |>
     filter(scale_factor == 0.1 & dbserver == 'mysql') |>
     ggplot(aes(x = normal_form, y = duration_sec)) +
          geom_boxplot() +
#          scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
          scale_y_continuous(limits= c(0, 1600)) +
          ggforce::facet_zoom(ylim = c(0, 20)) +
          ggtitle('SF 0.1GB - MySQL') +
          theme(axis.text = element_text(size = 11)) +
          theme(text = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 11.5, hjust = 0.5),
        legend.title = element_text(size = 11),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11))  
g3b

g3c <- df_scoring_duration |>
  filter(scale_factor == 0.1 & dbserver == 'postgresql') |>
  ggplot(aes(x = normal_form, y = duration_sec)) +
  geom_boxplot() +
  #          scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
  scale_y_continuous(limits= c(0, 1600)) +
  ggforce::facet_zoom(ylim = c(0, 20)) +
  ggtitle('SF 0.1GB - PostgreSQL') +
  theme(axis.text = element_text(size = 11)) +
  theme(text = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 11.5, hjust = 0.5),
        legend.title = element_text(size = 11),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11))  

x <- g3a + g3b + g3c + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("31a query_duration for sf01.pdf", plot = x,  device = "pdf") 
ggsave("31a query_duration for sf01.png", plot = x,  device = "png") 



g3d <- df_scoring_duration |>
     filter(scale_factor == 1 & dbserver == 'mssqlserver') |>
     ggplot(aes(x = normal_form, y = duration_sec)) +
          geom_boxplot() +
          scale_y_continuous(limits= c(0, 1600)) +
          ggforce::facet_zoom(ylim = c(0, 90)) +
  #          ggtitle('Initial (right) and magnified (left) distribution of the query duration for SF 1GB') +
          ggtitle('SF 1GB - MS SQL Server') +
  theme(axis.text = element_text(size = 11)) +
  theme(text = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 11.5, hjust = 0.5),
        legend.title = element_text(size = 11),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11))  

g3d


g3e <- df_scoring_duration |>
  filter(scale_factor == 1 & dbserver == 'mysql') |>
  ggplot(aes(x = normal_form, y = duration_sec)) +
  geom_boxplot() +
  scale_y_continuous(limits= c(0, 1600)) +
  ggforce::facet_zoom(ylim = c(0, 90)) +
  #          ggtitle('Initial (right) and magnified (left) distribution of the query duration for SF 1GB') +
  ggtitle('SF 1GB - MySQL') +
  theme(axis.text = element_text(size = 11)) +
  theme(text = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 11.5, hjust = 0.5),
        legend.title = element_text(size = 11),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11))  
g3e


g3f <- df_scoring_duration |>
  filter(scale_factor == 1 & dbserver == 'postgresql') |>
  ggplot(aes(x = normal_form, y = duration_sec)) +
  geom_boxplot() +
  scale_y_continuous(limits= c(0, 1600)) +
  ggforce::facet_zoom(ylim = c(0, 90)) +
  #          ggtitle('Initial (right) and magnified (left) distribution of the query duration for SF 1GB') +
  ggtitle('SF 1GB - PostgreSQL') +
  theme(axis.text = element_text(size = 11)) +
  theme(text = element_text(size = 11),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 11.5, hjust = 0.5),
        legend.title = element_text(size = 11),
        #plot.caption = element_text(size = 11),
        plot.caption = element_blank(),
        legend.text = element_text(size = 11))  

g3f

x <- g3d + g3e + g3f + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("31b_query_duration for sf1.pdf", plot = x,  device = "pdf") 
ggsave("31b_query_duration for sf1.png", plot = x,  device = "png") 


## check there is no collinearity (correlation plot)
corrplot::corrplot(cor(
     df_scoring_log10duration %>% 
          select_if(., is.numeric ) , 
             method = "spearman"), method = "number", 
     type = "upper",
     tl.cex = .6, number.cex = 0.7)



