# library(tidyverse)
# library(lubridate)
# library(gt)
#
# source("R/summaries.R")
# source("R/plots.R")
# source("R/gt.R")
#
# `%>%` <- magrittr::`%>%`
#
# # load("data/df_main.Rdata")
# #
# # add_na <- function(x, prop = 0.15) {
# #   x[runif(length(x)) < prop] <- NA
# #   x
# # }
# #
# # df <- map_dfc(df_main, function(x) add_na(x)) %>%
# #   mutate(
# #     ddn = ymd(paste0(2018 - age, "-01-01")) + sample(1:365, nrow(df_main))
# #   )
# #
# # save(df, file = "data/df.Rda")
# load("data/df.Rda")
# data(diamonds)
#
# # categorical plots ----
# test_cat_plots <- function(plotter) {
#   plotter(df, genre, activite) %>% print()
#   plotter(df, genre, activite, na.rm = T) %>% print()
#   plotter(df, genre) %>% print()
#   plotter(df, genre, na.rm = T) %>% print()
#
#   df %>%
#     mutate(activite = as.factor(activite)) %>%
#     filter(activite == "actif") %>%
#     plotter(genre, na.rm = T) %>% print()
#
#   df %>%
#     mutate(activite = as.factor(activite)) %>%
#     filter(activite == "actif") %>%
#     plotter(genre, activite, na.rm = F) %>% print()
#
#   plotter(diamonds, color) %>% print()
#   plotter(diamonds, color, cut) %>% print()
# }
#
# test_cat_plots(default_categ)
#
# # numeric plots ----
#
# test_num_plots <- function(plotter) {
#   plotter(df, age, genre) %>% print()
#   plotter(df, ddn, genre, na.rm = T) %>% print()
#   plotter(df, ddn) %>% print()
#   plotter(df, age, na.rm = T) %>% print()
#
#   df %>%
#     mutate(genre = as.factor(genre)) %>%
#     filter(genre == "femme") %>%
#     plotter(age, na.rm = T) %>% print()
#
#   df %>%
#     mutate(genre = as.factor(genre)) %>%
#     filter(genre == "femme") %>%
#     plotter(age, genre, na.rm = T) %>% print()
#
#   plotter(diamonds, carat) %>% print()
#   plotter(diamonds, carat, clarity) %>% print()
# }
#
# test_num_plots(default_hist)
# test_num_plots(default_violin)
#
# # numerical summaries ----
# numeric_summary(df, age, genre)
# numeric_summary(df, age)
# numeric_summary(df, ddn, genre)
# numeric_summary(df, ddn)
#
# # categorical summaries ----
# categorical_summary(df, genre, activite)
# categorical_summary(df, genre)
#
# categorical_levels_summary(df, genre, activite)
# categorical_levels_summary(df, genre)
#
# # numerical gt ----
# numeric_gt(df, age, genre)
# numeric_gt(df, age)
# numeric_gt(df, ddn, genre)
# numeric_gt(df, ddn)
#
# # categorical gt ----
# categorical_gt(df, genre, activite)
# categorical_gt(df, genre)
#
# # levels
# categorical_levels_gt(df, genre, activite, na.rm = F)
# categorical_levels_gt(df, genre, activite, na.rm = T)
# categorical_levels_gt(df, genre, na.rm = F)
# categorical_levels_gt(df, genre, na.rm = T)
# categorical_levels_gt(df, activite, genre, na.rm = F)
# categorical_levels_gt(df, activite, genre, na.rm = T)
#
# # levels wide
# categorical_levels_gt_wide(df, activite, genre, na.rm = F)
# categorical_levels_gt_wide(df, activite, genre, na.rm = T)
#
# # Quo/Enquo test ====
#
# test <- function(quq) {
#   quq <- enquo(quq)
#   inner(quq)
# }
#
# inner <- function(x) {
#   print(x)
# }
#
#
# categorical_levels_gt(diamonds, cut, clarity)
# categorical_levels_gt_wide(diamonds, cut, clarity)
#
#
