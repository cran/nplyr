## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(nplyr)

gm_nest <- 
  gapminder::gapminder_unfiltered %>%
  tidyr::nest(country_data = -continent)

gm_nest

# we can use nplyr to perform operations on the nested data
gm_nest %>%
  nest_filter(country_data, year == max(year)) %>%
  nest_mutate(country_data, pop_millions = pop/1000000) %>%
  slice_head(n = 1) %>%
  tidyr::unnest(country_data)

# in this case, we could have obtained the same result with tidyr and dplyr
gm_nest %>%
  tidyr::unnest(country_data) %>%
  group_by(continent) %>%
  filter(year == max(year)) %>%
  mutate(pop_millions = pop/1000000) %>%
  ungroup() %>%
  filter(continent == "Asia")

## -----------------------------------------------------------------------------
path <- "https://raw.githubusercontent.com/markjrieke/nplyr/main/data-raw/"

surveys <- 
  tibble::tibble(survey_file = c("job_survey", "personal_survey")) %>%
  mutate(survey_data = purrr::map(survey_file, ~readr::read_csv(paste0(path, .x, ".csv"))))

surveys

## ---- error=TRUE--------------------------------------------------------------
surveys %>%
  tidyr::unnest(survey_data)

## -----------------------------------------------------------------------------
surveys %>%
  slice(1) %>%
  tidyr::unnest(survey_data) %>%
  glimpse()

surveys %>%
  slice(2) %>%
  tidyr::unnest(survey_data) %>%
  glimpse()

## -----------------------------------------------------------------------------
surveys <- 
  surveys %>%
  nest_mutate(survey_data,
              age_group = if_else(Q1 < 65, "Adult", "Retirement Age")) %>%
  nest_group_by(survey_data, Q3) %>%
  nest_add_count(survey_data, 
                 name = "n_respondents_in_industry") %>%
  nest_mutate(survey_data, 
              median_industry_age = median(Q1)) %>%
  nest_ungroup(survey_data)

surveys %>%
  slice(1) %>%
  tidyr::unnest(survey_data)

surveys %>%
  slice(2) %>%
  tidyr::unnest(survey_data)

