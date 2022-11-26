library(gtrendsR)
library(tidyverse)
library(gtExtras)
library(svglite)

# Has the sentiment for work from home jobs declined since the recession? 
# Has the search term reduce for this?


# search_terms <- c("recession", "work from home", "home", "stability", "economic safety", "resign", "quit", 
#                   "job promotion", "job search", "unemployed", "fired")

install.packages("readr")
install.packages("colorspace")
install.packages("vctrs")
install.packages("gtrendsR")

# Maximum three key words to optimize
# We will need to add 3 key words here max in the shiny app
main_lst <- gtrends(c("recession", "economy"),
        geo = "US",
        time = "today 1-m")



main_lst %>% 
  pluck("interest_by_region") %>% 
  as_tibble()



main_lst <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))


main_lst$interest_by_region %>% 
  as_tibble() %>% 
  mutate(
    state = state.abb[match(location, state.name)]
  ) %>% 
  View()



main_lst$interest_over_time %>% as_tibble()


plot(main_lst) + theme_minimal()

main_lst$interest_by_region %>% 
  dplyr::group_by(location) %>%
  # must end up with list of data for each row in the input dataframe
  dplyr::summarize(hits_data = list(hits),
                   hits      = mean(hits, na.rm = TRUE)) %>%
  gt() %>%
  gt_plt_sparkline(hits_data)
  

mtcars %>%
  dplyr::group_by(cyl) %>%
  # must end up with list of data for each row in the input dataframe
  dplyr::summarize(mpg_data = list(mpg), .groups = "drop") %>%
  gt() %>%
  gt_plt_sparkline(mpg_data)


# For tomorrow:
# 1) See the example in Netlify
# 2) Start designing the Shiny App to see what information we can add (check the other parameters in main_lst)
# 3) Create a plot of the US to show the interest by region on specific key words! Use main_lst$interest_by_region
main_lst$interest_by_region


highchart() %>%
  hc_add_series(data = abs(rnorm(5)), type = "column") %>%
  hc_add_series(data = purrr::map(0:4, function(x) list(x, x)), type = "scatter", color = "orange")
