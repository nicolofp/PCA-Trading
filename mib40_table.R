library(data.table)
library(xts)
library(rvest)
library(magrittr)
library(nsprcomp)
library(portfolioBacktest)
library(ggplot2)

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
