library(data.table)
library(xts)
library(rvest)
library(magrittr)
library(nsprcomp)
library(portfolioBacktest)
library(ggplot2)
library(dplyr)

# Import script from the library
source("trading_library.R")

# Download list MIB40
url = "https://en.wikipedia.org/wiki/FTSE_MIB"
mib40_list = url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="constituents"]') %>%
  html_table() %>% 
  extract2(1) %>%
  select(Company, Ticker, `ISIN (and link to quote webpage)`) %>%
  rename(company = Company, ticker = Ticker, isin = `ISIN (and link to quote webpage)`) %>%
  as.data.table() 

# Download data from 2010 (Not all of them will be available)
mib40_data = stockDataDownload(mib40_list$ticker,
                               from = "2010-01-01",
                               to = Sys.Date(),
                               local_file_path = NULL)

# Compute strategy
NAV = compute_startegy(data = mib40_data)

# Melt data table for ggplot 
NAV = data.table(data = index(NAV),
                 NAV$markowitz,
                 NAV$pca_nn)

NAV_melt = melt(NAV,id.vars = "data", variable.name = "Portfolio")
ggplot(NAV_melt, aes(x = data, y = value, col = Portfolio)) +
  geom_line() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme_bw() +
  labs(title = "Portfolio example",
       subtitle = "The strategy is updated weekly with\n10 weeks of window to update the weights",
       caption = "Data Source: Yahoo") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 8)) +
  xlab("Time") + ylab("NAV")
  
