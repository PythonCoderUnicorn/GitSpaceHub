# NYC Flights dataset

library(tidyverse)
library(dplyr)
library(nycflights13)

flights = nycflights13::flights

colnames(flights)

# get flights for Jan 1
(jan1 = filter(flights, month==1, day==1))

# get flights nov or dec using %in%
nov_dec = filter(flights, month %in% c(11,12))
nov_dec

# get flights not delayed arrival or departure by >2 hrs
filter(flights, !(arr_delay > 120 | dep_delay > 120))

# Had an arrival delay of two or more hours
filter(flights, arr_delay > 120)

# Flew to Houston (IAH or HOU)
unique(flights$dest)

houston = filter(flights, dest %in% c("IAH","HOU"))
houston

# Were operated by United, American, or Delta
unique(flights$carrier)

carriers3 = filter(flights, carrier %in% c("UA","AA","DL"))
carriers3

# Departed in summer (July, August, and September)
filter(flights, dep_time %in% c(7,8,9))

# Arrived more than two hours late, but didn’t leave late
late_early = filter(flights, arr_time > 120 & !(dep_delay >0))
late_early

# ==========================




# --- arrange
arrange(flights, month, day, year)

arrange(flights, desc(air_time))



# ---- select
select(flights, starts_with("A"))

select(flights, contains("TIME"))


# 
# =====================
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist_avg = mean(distance, na.rm = TRUE),
    delay_avg = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delays, mapping = aes(x = dist_avg, y = delay_avg)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)






# ------- remove NA
flights %>% 
  group_by( origin, flight, carrier, tailnum) %>% 
  summarise(dep_delay_avg = mean(dep_delay, na.rm = TRUE))


# missing values represent canceled flights, 
# we could also tackle the problem by first removing the canceled flights.
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day, origin, tailnum, carrier) %>% 
  summarise(dep_delay_avg = mean(dep_delay))



# look at the planes (identified by their tail number) 
# that have the highest average delays
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)+
  ggdark::dark_mode()

# ---
fdelays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n() )

ggplot(data = fdelays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)+
  ggdark::dark_mode()

fdelays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)+
  ggdark::dark_mode()


not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )
