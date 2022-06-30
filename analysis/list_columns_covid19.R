library(tidyverse) 
library(lubridate)
library(countrycode)

url_in <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

df <- tibble(
  file_names = 
    c('time_series_covid19_confirmed_global.csv', 
      'time_series_covid19_deaths_global.csv',
      'time_series_covid19_confirmed_US.csv',
      'time_series_covid19_deaths_US.csv')
)

df %>%  
  mutate(url = str_c(url_in, file_names, sep = '')) -> df

df %>%
  mutate(data = map(url, read_csv)) -> df

df %>%  
  mutate(case_types = as_factor(str_extract(file_names,"[:alpha:]*_[gU][:alpha:]*"))) -> df

df %>%  
  select("case_types", "data" ) -> df

df %>%  
  mutate(vars = map(df$data, names)) -> df

df %>% 
  mutate(vars = map(df$vars, ~unlist(.)[1:15]))

fix_names <- function(dataframe, string_pattern, replacement_pattern) {
  
  stopifnot(is.data.frame(dataframe))
  stopifnot(is.character(string_pattern))
  stopifnot(is.character(replacement_pattern))
  
  names(dataframe) = str_replace_all(names(dataframe), string_pattern, replacement_pattern)
  
  return(dataframe)
}

df %>%  
  mutate(data = map(data, ~fix_names(. , "([ey])/", "\\1_"))) -> df

df %>%  
  mutate(data = map(data, ~fix_names(. , "Admin2", "County" ))) %>%  
  mutate(data = map(data, ~fix_names(. , "Long_", "Long") ))-> df

df %>%  
  mutate(data = map_if(data, str_detect(df$case_types, "_US"), ~select(.,-c("UID", "iso2", "iso3", "code3", "FIPS", "Combined_Key")))) -> df

map_if(df$data, ~!'Population'%in% colnames(.), ~mutate(. ,Population = 1 )) -> df$data
map_if(df$data, ~!'County'%in% colnames(.), ~mutate(. ,County = 1 )) -> df$data
map(df$data, ~ mutate(., Country_State = str_c(.$Province_State, .$Country_Region, sep = "_"))) -> df$data

df %>%
  mutate(vars = map(df$data, names)) %>%  
  mutate(vars = map(df$vars, ~unlist(.)[1:15])) -> df

df_long <- df

map(df_long$data, ~ pivot_longer(., cols = -c("County", "Province_State", "Country_Region", "Lat", "Long", "Population", "Country_State"), names_to = "Date", values_to = "cases", names_transform = list(Date=mdy)) ) -> df_long$data

df_long %>%
  mutate(data = map(data, ~mutate(., Continent = countrycode(Country_Region,
                                                             origin = "country.name",
                                                             destination = "continent")))) -> df_long
df_long %>%
  mutate(data = map(data, ~ mutate(., Continent = case_when(
    Country_Region == "Diamond Princess" ~ "Asia",
    Country_Region == "Kosovo" ~ "Americas",
    Country_Region == "MS Zaandam" ~ "Europe",
    Country_Region == "Micronesia" ~ "Oceania", 
    TRUE ~ Continent))))  -> df_long

map(df_long$data, ~unique(.  $Continent))

df_long$data[[1]]$County <- as.character(df_long$data[[1]]$County)
df_long$data[[2]]$County <- as.character(df_long$data[[2]]$County)
df_long$data[[3]]$County <- as.character(df_long$data[[3]]$County)
df_long$data[[4]]$County <- as.character(df_long$data[[4]]$County)

df_long %>%
  unnest(cols = data) %>%
  ungroup() -> df_all

remove(df, df_long)

df_all %>%
  select(-vars) -> df_all

df_pop <- read_csv("../data/WPP2019_TotalPopulation.csv")

a <- unique(df_all$Country_Region)
b <- unique(df_pop$Location)
setdiff(a,b)

a <- unique(df_all$Country_Region)
b <- unique(df_pop$Location)
length(a)  < length(b)

total_population <- sum(df_pop$PopTotal)
df_pop %>%  
  mutate(percen_world_pop = PopTotal/ total_population) -> df_pop

semi_join(df_pop, df_all, by = c("Location" = "Country_Region")) -> df_pop

df_pop %>% 
  mutate(rank_p = rank(-PopTotal, na.last = TRUE),
         rank_d = rank(-PopDensity, na.last = TRUE),
         PopTotal = (PopTotal*1000)) -> df_pop
df_pop %>% 
  slice_min(rank_p, n = 10)
df_pop %>%  
  slice_min(rank_d, n = 10)

ggplot(df_pop, aes(x = rank_p, y = rank_d))+
  geom_point()+
  geom_smooth(method = "lm",
              se = F,
              color = "blue")

lmout <-lm(rank_p ~ rank_d, data = df_pop)
summary(lmout)

# Interpretation: We can see that the blue linear line is very close to horizontal to x- axis. What this indicates is that there is no relationship between rank_p and rank_d. Since p value is 0.4744 which is bigger than 0.05, means this is not statistically significant. Furthermore, adjusted r-squared appears to be negative. "Because R-square is defined as the proportion of variance explained by the fit, if the fit is actually worse than just fitting a horizontal line then R-square is negative."

df_all %>%
  inner_join(df_pop, by = c("Country_Region" = "Location")) -> df_allp
df_allp

df_allp %>%
  select(Country_Region, Country_State) %>%
  distinct() %>%  
  group_by(Country_Region) %>% 
  summarise(count = n()) %>%  
  filter(count> 1) %>%  
  nrow()

df_allp %>%
  select(Country_Region, Country_State) %>%
  distinct() %>%  
  group_by(Country_Region) %>% 
  summarise(count = n()) %>%  
  arrange(desc(count)) -> states_region 

head(states_region)

df_allp %>%
  group_by(Country_Region, Continent, case_types, rank_p, rank_d) %>%
  summarise(current_totals = max(cases), percentage_total_pop = current_totals/last(PopTotal)*100) %>%
  ungroup() -> df_with_summarize

df_with_summarize %>%
  filter(case_types == "confirmed_global") %>%
  arrange(desc(current_totals)) %>%
  head(20) %>%  
  select(Country_Region, current_totals, percentage_total_pop, rank_p, rank_d) -> countries20_confirmed
countries20_confirmed

df_with_summarize %>%
  filter(case_types == "deaths_global") %>%
  arrange(desc(current_totals)) %>%
  head(20) %>%  
  select(Country_Region, current_totals, percentage_total_pop, rank_p, rank_d)-> countries20_deaths
countries20_deaths

# Interpretation: US has the highest confirmed cases and deaths among all countries. Eventhough the rankings for total_population is 3rd ranking and rankings for population density is 141 rankings. 

countries20_confirmed %>%  
  select(Country_Region) %>%  
  list()-> confirmed_20

df_with_summarize %>%
  filter(case_types == "confirmed_global") %>% 
  arrange(desc(percentage_total_pop)) %>%
  head(20) %>%  
  select(Country_Region) %>%  
  list()-> perc20

setdiff(perc20[[1]], confirmed_20[[1]])

countries20_deaths %>%  
  select(Country_Region) %>%  
  list()-> deaths_20

df_with_summarize %>%
  filter(case_types == "deaths_global") %>% 
  arrange(desc(percentage_total_pop)) %>%
  head(20) %>%  
  select(Country_Region) %>%  
  list()-> perc_deaths_20

setdiff(deaths_20[[1]], perc_deaths_20[[1]])

confirmed <- countries20_confirmed$Country_Region
df_allp %>%
  filter(case_types == "confirmed_global", Country_Region == confirmed) %>%
  ggplot() +
  geom_line(aes(x = Date, y = cases, color = Country_Region)) +
  facet_wrap(~Continent) +
  scale_y_log10() +
  theme_bw() +
  ylab("Cumulative Cases") +
  ggtitle("The COVID-19 confirmed cases vs Time (Top 20 countries)") +
  theme(legend.position="bottom",legend.background = element_rect(fill="white",
                                                                  size=0.5, linetype="solid"))

deaths <- countries20_deaths$Country_Region
df_allp %>%
  filter(case_types == "deaths_global", Country_Region == deaths) %>%
  ggplot() +
  geom_line(aes(x = Date, y = cases, color = Country_Region)) +
  facet_wrap(~Continent) +
  scale_y_log10() +
  theme_bw() +
  ylab("Cumulative Cases") +
  ggtitle("The COVID-19 deaths cases vs Time(Top 20 countries)") +
  theme(legend.position="bottom", legend.background = element_rect(fill="white",
                                                                   size=0.5, linetype="solid"))



