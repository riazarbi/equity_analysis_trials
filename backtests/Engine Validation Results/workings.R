# specify directory
direc <- "backtests/Engine Validation Results/"

# load all trial returns
trials <- as.character(8:14)
trial_returns <- paste(trials, "_returns.feather", sep="")
trial_full_paths <- paste(direc, trial_returns, sep="")
library(feather)
library(magrittr)
library(dplyr)
for (i in seq_along(1:length(trial_full_paths))) {
  df <- read_feather(trial_full_paths[i])
  assign(paste("trial", i+7, sep = ""), df)
}
rm(df)

trial8 <- trial8 %>% dplyr::rename("trial_8" = !!names(.[2])) %>%
  mutate(trial_8_index = (1+trial_8) * lag(1+trial_8) )
trial9 <- trial9 %>% dplyr::rename("trial_9" = !!names(.[2])) %>%
  mutate(trial_9_index = (1+trial_9) * lag(1+trial_9) )
trial10 <- trial10 %>% dplyr::rename("trial_10" = !!names(.[2])) %>%
  mutate(trial_10_index = (1+trial_10) * lag(1+trial_10) )
trial11 <- trial11 %>% dplyr::rename("trial_11" = !!names(.[2]))%>%
  mutate(trial_11_index = (1+trial_11) * lag(1+trial_11) )
trial12 <- trial12 %>% dplyr::rename("trial_12" = !!names(.[2])) %>%
  mutate(trial_12_index = (1+trial_12) * lag(1+trial_12) )
trial13 <- trial13 %>% dplyr::rename("trial_13" = !!names(.[2])) %>%
  mutate(trial_13_index = (1+trial_13) * lag(1+trial_13) )
trial14 <- trial14 %>% dplyr::rename("trial_14" = !!names(.[2])) %>%
  mutate(trial_14_index = (1+trial_14) * lag(1+trial_14) )

# build a returns matrix
all_returns <- full_join(trial8, trial9)
all_returns <-full_join(all_returns, trial10)
all_returns <-full_join(all_returns, trial11)
all_returns <-full_join(all_returns, trial12)
all_returns <-full_join(all_returns, trial13)
all_returns <-full_join(all_returns, trial14)

# load price data .Rds from trial 8 temp dir after backtest completes
price_data <- readRDS(paste(direc, "ticker_data.Rds", sep=""))
constituents <- readRDS(paste(direc, "constituent_list.Rds", sep=""))
# compute market returns
start_date <- min(all_returns$date)
end_date <- max(all_returns$date)

index_cap_seq <- numeric()
for (score_date in seq(from=start_date, to=end_date, by="day")) {
  score_date <- lubridate::as_date(score_date)
  print(score_date)
  index <- constituents %>% 
    dplyr::filter(date <= score_date) %>% 
    dplyr::filter(date == max(date)) %>%
    select(ticker)
  market_size <- numeric()
  for (i in seq_along(1:length(index$ticker))) {
    tick_cap <- price_data[[index$ticker[i]]] %>% filter(date ==score_date) %>%
      select(market_cap) %>% pull()
    market_size <- c(market_size, tick_cap)
  }
  index_cap <- sum(market_size)
  index_cap_seq <- c(index_cap_seq, index_cap)
}

index_cap_ts <- data.frame(all_returns$date, index_cap_seq) 
colnames(index_cap_ts) <- c("date", "market_cap")
index_returns <- index_cap_ts %>% 
  mutate(market_return = market_cap / lag(market_cap) - 1) %>% 
  select(-market_cap)
all_returns <-full_join(all_returns, index_returns, by = "date")
all_returns <- all_returns %>% tidyr::drop_na()
colnames(all_returns)
correlation <- cor(all_returns %>% dplyr::select(trial_8, 
                                                 trial_9, 
                                                 trial_10, 
                                                 market_return))
# Rename to be in coformance with the labeling reported in my dissertation
colnames(correlation) <- c("Trial A", "Trial B", "Trial C", "Market Return")
rownames(correlation) <- c("Trial A", "Trial B", "Trial C", "Market Return")

saveRDS(correlation, "paper/data/correlation.Rds")

#######################################################################
trial_stats <- paste(direc, trials, "_portfolio_stats.feather", sep="")

for (i in seq_along(1:length(trial_stats))) {
  df <- read_feather(trial_stats[i]) %>% select(date, portfolio_value)
  assign(paste("trial_stats", i+7, sep = ""), df)
}
rm(df)

trial8_trial_stats <- trial_stats8 %>% dplyr::rename("trial_8_trial_stats" = !!names(.[2]))
trial9_trial_stats <- trial_stats9 %>% dplyr::rename("trial_9_trial_stats" = !!names(.[2]))
trial10_trial_stats <- trial_stats10 %>% dplyr::rename("trial_10_trial_stats" = !!names(.[2])) 
trial11_trial_stats <- trial_stats11 %>% dplyr::rename("trial_11_trial_stats" = !!names(.[2]))
trial12_trial_stats <- trial_stats12 %>% dplyr::rename("trial_12_trial_stats" = !!names(.[2])) 
trial13_trial_stats <- trial_stats13 %>% dplyr::rename("trial_13_trial_stats" = !!names(.[2])) 
trial14_trial_stats <- trial_stats14 %>% dplyr::rename("trial14_trial_stats" = !!names(.[2])) 

# build a returns matrix
all_values <- full_join(trial8_trial_stats, trial9_trial_stats)
all_values <-full_join(all_values, trial10_trial_stats)
all_values <-full_join(all_values, trial11_trial_stats)
all_values <-full_join(all_values, trial12_trial_stats)
all_values <-full_join(all_values, trial13_trial_stats)
all_values <-full_join(all_values, trial14_trial_stats)
all_values <- full_join(all_values, index_cap_ts)
saveRDS(all_values, "paper/data/all_trial_returns.Rds")
library(xts)
library(dygraphs)
all_values_xts <- xts(all_values %>% 
                          select(trial_8_trial_stats, 
                                 trial_11_trial_stats,
                                 trial_12_trial_stats), order.by=all_values$date)

# Rename to be in coformance with the labeling reported in my dissertation
colnames(all_values_xts) <- c("Trial A", "Trial D", "Trial E")
dygraph(all_values_xts, 
        main = "All Trials Normalised Total Returns", 
        ylab = "Indexed Value") %>%
  dyLegend(width = 600) %>%
  dyOptions(maxNumberWidth = 20, stackedGraph = FALSE) %>%
  dyRangeSelector %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

all_values_xts <- xts(all_values %>% 
                        select(trial_12_trial_stats, 
                               trial_13_trial_stats), order.by=all_values$date)
# Rename to be in coformance with the labeling reported in my dissertation
colnames(all_values_xts) <- c("Trial E", "Trial F")
dygraph(all_values_xts, 
        main = "All Trials Normalised Total Returns", 
        ylab = "Indexed Value") %>%
  dyLegend(width = 600) %>%
  dyOptions(maxNumberWidth = 20, stackedGraph = FALSE) %>%
  dyRangeSelector %>%
  dyRebase(value=100) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
  
