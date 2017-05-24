install.packages("dplyr")
install.packages("hflights")
suppressMessages(library(dplyr))
library(hflights)
data(hflights)
head(hflights)
flights <- tbl_df(hflights)
jan1 <- flights[flights$Month == 1 & flights$DayofMonth == 1, ]
djan1 <- filter(flights, Month == 1, DayofMonth == 1)
filter(flights, UniqueCarrier == "AA"| UniqueCarrier == "UA")
filter(flights, UniqueCarrier %in% c("AA", "UA"))
flights[, c("DepTime","ArrTime", "FlightNum")]
select(flights, DepTime, ArrTime, FlightNum)
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60) %>%
  as.data.frame()
flights[order(desc(flights$DepDelay)), c("UniqueCarrier", "DepDelay")]
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))
flights[, c("Distance", "AirTime", "Speed")]
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)
flights <- flights %>% mutate(Speed = Distance/AirTime*60)
head(aggregate(ArrDelay ~ Dest, flights, mean))
with(flights, tapply(ArrDelay, Dest, mean, na.rm = TRUE))
flights %>%
  group_by(Dest) %>%
  summarize(avg_delay = mean(ArrDelay, na.rm = TRUE))
flights %>%
  group_by(UniqueCarrier) %>%
  summarize_each(funs(mean), Cancelled, Diverted)
flights %>%
  group_by(UniqueCarrier) %>%
  summarize_each(funs(min(., na.rm = TRUE), max(., na.rm = TRUE)), matches("Delay"))
flights %>%
  group_by(Month, DayofMonth) %>%
  summarize(flight_count = n()) %>%
  arrange(desc(flight_count))
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)
flights %>%
  group_by(Dest) %>%
  summarize(flight_count = n(), plane_count = n_distinct(TailNum))
flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>% 
  head()
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))
flights %>%
  group_by(Month) %>%
  summarize(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count))
flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))
flights %>% sample_n(5)
flights %>% sample_frac(0.25, replace = TRUE)
glimpse(flights)
