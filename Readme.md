# Exploring Trade Between Africa and other parts of the World


The goal of this repository is to use publicly available databases such as the World Bank's WITS Trade Stats to understand the flow of goods and services between African countries of interest and their largest trade partners. 

This Reposirory uses the **wbstats** package developed by Jesse Piburn to pull the latest data. I then use the **tidyverse** suite of packages to clean and explore the data. Finally, I will use the **caret** package to build some basic macroeconomic models to understand both between-country trade relationships as well as trade flows between large economic blocks like the East African Community, SADC etc with China, India, the European Union, and the United States. 


## Load Required Libraries
```
library(wbstats)
library(sf) # For plotting map shapefiles
library(tidyverse) 
```


## Define Helper Functions

```
#' Capitalize and make table titles bold
#'
#' @param indicator_id Indicator Id
#' @param indicator Indicator Name
#' @param source Database Source
#'
#' @return a formatted table with capitalized and bold titles.
#' @export
#'
#' @examples 
#' gt::gt() %>% prettify_table()
prettify_table <- function(indicator_id = NULL, indicator = NULL, source = NULL){
  
  labelled_table <- gt::gt() %>% 
    # Use clearer labels
     cols_label(indicator_id = "Indicator ID",
             indicator = "Indicator Name",
             source = "Source") %>% 
    # Make columns bold
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style = list(cell_text(weight = "bold")
     )
   )

}

```
The World Bank tracks are over *200* Trade indicators with varying levels of missing data. In this repo, I only focus on indicators that are routinely used in macroeconomics publications. I will refer to both published economic literature and other trustworthy financial publications such as *Financial Times* and the *Wall Street Journal*.

```
trade_indicators <- wbstats::wb_indicators() %>%
  filter(grepl("Trade", indicator)) %>% 
  # The unit variable is empty(all NAs.). Exclude
  select(-unit) %>% 
  # Select aggregate metrics that are easity to understand. 
  # For example a metric as a % of GDP 
  filter(grepl("GDP", indicator, ignore.case = T))
```

![](plots/trade_indicators.png)

## A simple glance at one metric

### Trade (% of GDP)

```
trade_pct_gdp <- 
  trade_indicators %>% 
  filter(indicator_id == "NE.TRD.GNFS.ZS") 
```

Pull data for trade as a % of GDP. The earliest available year is 1960. I suspect that most of the data before 2000 will be missing given that the Statistical Capacity of most countries was still very low and hence the data collection was not as reliable. 

The **wbstats** package includes a crosswalk that maps each country's "iso3c" code to the corresponding country name and continent. We pull this crosswalk and use it to label indicator data. 


```
df_trade <- wb_data(indicator = trade_pct_gdp$indicator_id) %>% 
  # Clean variable names
  janitor::clean_names() %>% 
  #Use Proper scale
  mutate(ne_trd_gnfs_zs = ne_trd_gnfs_zs*100) %>%
  select(iso3c, ne_trd_gnfs_zs, last_updated, date)

# Pull country labels
country_xwalk <- wb_countries() %>% 
  select(iso3c, region, income_level) %>% 
  unique()

# Add continent labels, joining by each country's isoc
df_tbl_labelled <- 
  df_trade %>%
  filter(date > 1999) %>% 
  left_join(country_xwalk, "iso3c") %>% 
  # Combine South Asia, East Asia & Pacific
  mutate(new_region = ifelse(region %in% c("East Asia & Pacific", "South Asia"), 
                             "Asia", region)) %>% 
  mutate_at(.vars = c("iso3c", "new_region", "income_level"), .funs = as_factor)  %>% 
  # Sum the metric by region-year
  group_by(new_region, date) %>% 
  summarize(median_trade_pct_gdp = median(ne_trd_gnfs_zs, na.rm = TRUE)/100) %>%
  ungroup()
  
 
# Plot the median of Trade as a % of GDP aggregated by region   

plt_trade_pct_gdp <- 
  ggplot(data = df_tbl_labelled) +
  geom_point(
    aes(
      x = date,
      y = median_trade_pct_gdp,
      color = new_region
      )
    ) +
  geom_line(aes(x= date, y = median_trade_pct_gdp, colour = new_region)) +
  xlab("YEAR") +
  ylab("Median Trade as a % of GDP") +
  scale_y_continuous(expand = c(0, 0), limits = c(20,120)) +
  labs(colour = "Region") + 
  ggthemes::theme_tufte(base_size = 14) 

ggsave(plt_trade_pct_gdp, 
       file = file.path(here::here(), "LNSHUTI.github.io/plots/trade_by_region_year.png"))
```

As expected, there was a dip in GDP for all regions starting in late 2019 due to Covid-19 pandemic. 

![](plots/plt_trade_pct_gdp.png)


```
 ![](plots/trade_by_region_year.gif)
```

# Trade Flows

The previous plot is interesting for two reasons. It shows what is widely known i.e that the global economy has been growing over the last few decades as shown by the overall growth in trade for all regions. The recession of 2008 and the contraction that followed the beginning of Covid-19 line up well with the plot. We can think of this step as a sanity check for ensuring that the data source is credible. 

Now to the interesting part. Let us use the same APIs to pull trade flow metrics from the World Bank database. We will link to an interactive shiny app that interested users can play with to examine trade flows between Africa and the rest of the world. <Link to Shiny APP>

Use the **wb_search()** function to search for all indicators that are related to trade. Display some of them and their corresponding descriptions. 

```
trade_metrics_tbl <- wb_search("TRD") %>% 
  gt::gt(data = .) %>% 
  # Exclude measures without a valid description
  filter(!is.na(indicator_desc)) %>% 
  # Exclude metrics that are too specific
  filter(!grepl(pattern = "FRM", x = indicator_id, ignore.case = T)) %>% 
  gt::gt(data = .) 

trade_metrics_tbl 
```

In addition to the World Bank data, we rely on **International Trade Data** from the [Harvard data-verse](atlas.cid.harvard.edu/data). This source is richer and has more information related to the flow of goods and services between countries. 

Before we can visualize trade flows, we need to understand the population distribution within Africa. It makes sense that the largest trade ports would also have the highest population density. Let us start by simply plotting the largest population centers in Africa. 

```
# Load shape files

countryweb <-
  "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"

# Get country codes
continents_except_oceania <- 
  read.csv(countryweb) %>% 
  janitor::clean_names() %>% 
  mutate(continent_code = ifelse(continent_name == "North America", "NA", continent_code)) %>% 
  filter(continent_code != "OC") %>% 
  select(continent_name, continent_code, two_letter_country_code) %>% 
  # Add country specific demographic information
  left_join(worldbank_df, by = c("two_letter_country_code" = "iso_a2"))

# World map
wolrd_map <-
  world %>% 
  filter(!(continent %in% c("Oceania", "Seven seas (open ocean)", "Antarctica"))) %>% 
  filter(!is.na(iso_a2)) %>% 
  left_join(continents_except_oceania, by = c("iso_a2" = "two_letter_country_code"))

simplified_world_shape <- 
  ggplot(wolrd_map) +
  geom_sf(aes(geometry = geom, fill = continent)) + 
  remove_all_axes +
  labs(color = 'Continent') 
```

![](plots/simplified_world_shape.png)


Run script that analyzes trade data as networks. 

*TODO:* Add details on what the script does and release the output.  
```
source(here::here("/R/analyze_trade_data.R"))
```

**References:** 

Jesse Piburn (2020). wbstats: Programmatic Access to the World Bank API. Oak Ridge
  National Laboratory. Oak Ridge, Tennessee. URL
  https://doi.org/10.11578/dc.20171025.1827
  
John Graves(2019). health-care-markets: Defining Markets for Health Care Services. Vanderbilt
  University Medical Center. Nashville, TN. URL 
  https://github.com/graveja0/health-care-markets
  
The Growth Lab at Harvard University. International Trade Data (SITC, Rev. 2). 2019-05-31.
  2019. V5. Harvard Dataverse. URL. https://doi.org/10.7910/DVN/H8SFD2. 
  doi/10.7910/DVN/H8SFD2
  
Animate ggplot. Stack Overflow.
  https://stackoverflow.com/questions/54855334/gganimate-time-series-and-two-line-plot

Matt Grainger (2021). SDGsR: Interface with the UN SDGs API to get data about the 
  Sustainable Development Goals. R package version
  0.0.0.9000. https://drmattg.github.io/SDGsR/