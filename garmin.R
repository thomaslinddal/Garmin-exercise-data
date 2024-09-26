library(pacman) # til at installere og loade pakker
pacman::p_load(
  rio, # til data importering
  here, # til filstier
  here, # til filstier
  janitor, # til data cleaning
  lubridate, # til at arbejde med datoer
  hms,# til at arbejde med tid
  tidyverse, # til data management
  easystats, # til at lave statistik
  skimr # summary af data
)

#importer data
garmin_raw <- import("Activities.csv")

#skim data
skim(garmin_raw)

garmin_raw <- garmin_raw %>% 
  janitor::clean_names()

names(garmin_raw)


#clean data ---------------------------------------------------------------
garmin_clean <- garmin_raw %>%
  janitor::clean_names() %>% # clean kollonne navne
  distinct() %>% 
  
  # rename kollonner
  rename(
    aerobic_training_effect = aerobic_te,
    normalized_power = normalized_power_r_np_r,
    training_stress_score = training_stress_score_r,
    max_20min_power = max_avg_power_20_min,
    date_time = date) %>%
  
  # fjern række med filter()
  filter(
    !row_number() %in% c(375, 295)) %>% # fjern rækken.
  
  # væk med kollonner som ingen reel data har
  select(
    -favorite,
    -avg_vertical_ratio,
    -avg_vertical_oscillation,
    -avg_vertical_ratio,
    -avg_ground_contact_time,
    -avg_stride_length,
    -avg_stroke_rate,
    -avg_swolf,
    -flow,
    -total_reps,
    -decompression,
    -total_strokes,
    -grit,
    -best_lap_time,
    -number_of_laps) %>% 
  
  # mutate kollonner
  mutate(
    date_time = ymd_hms(date_time),# convert to date-time
    date_onset = date(date_time), # extract date
    time_onset = as_hms(date_time), # extract time
    
    # distance
    distance = gsub("[^0-9.-]", "", distance), # remove non-numeric characters
    distance = as.numeric(distance), # convert to numeric
    distance = distance / 100,# convert to kilometers
    distance = ifelse(distance <= 1, NA, distance), # remove outlier
    
    # Kalorier
    calories = as.numeric(calories), # convert to numeric
    calories = ifelse(calories > 20, calories / 100, calories), # convert to kilocalories
    calories = ifelse(calories == 16.000, NA, calories), # bugged value
  
    # Tid
    time = as_hms(time), # convert to time
    
    # Fart
    avg_speed = gsub("[^0-9\\.]", "", avg_speed),
    avg_speed = as.numeric(avg_speed), # convert to numeric
    avg_speed = avg_speed / 10, # convert to km/h
    
    # Hr
    avg_hr = as.numeric(avg_hr),
    max_hr = as.numeric(max_hr),
    
    # ascent
    total_ascent = as.numeric(total_ascent), 
    ascent = case_when(
      total_ascent < 400. ~ "Flat",
      total_ascent >= 400 ~ "Moderate",
      total_ascent > 800 ~ "Hilly")) # categorize ascent
  
  

# T test af gennemsnitsfarven før og efter 30 år --------------------------------------------------------
# er min gennemsnitsfart højere eller lavere før eller efter 30 år?

# students t-test
garmin_clean %>% 
  mutate(
    year_cat = case_when(
      year(date_onset) < "2018-01-20" ~ "Før 30 år",
      year(date_onset) >= "2018-01-20" ~ "Efter 30 år")) %>% # categorize years
  t.test(avg_speed ~ year_cat, data = .) %>% # students t-test
  report()

#  t-test viser at min gennemsnitshastighed før 30 år var 25.73 km/t
# og den efter 30 år var 27.73 km/t.
# forskellen i gennemsnitshastighed mellem aktiviteter før 2017
# i forhold til aktiviteter efter 30 år var -1.29 km/t.
# 95% CI [-1.97, -0.61] og en p = < 0.001 betyder at
# forskellen i gennemsnitshastighed er statistisk signifikant
# Det betyder at jeg desværre er blevet signifikant langsommere med #alderen :(

garmin_clean %>% 
  mutate(
    year_cat = case_when(
      year(date_onset) < "2018-01-20" ~ "Før 30 år",
      year(date_onset) >= "2018-01-20" ~ "Efter 30 år"), # categorize years
  year_cat = fct_relevel(year_cat, "Før 30 år", "Efter 30 år")) %>% 
  ggplot(aes(
    x = year_cat,
    y = avg_speed,
    fill = year_cat)) +
  geom_boxplot(show.legend = FALSE) + # add boxplot
  scale_y_continuous(breaks = seq(0, 45, 5),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal() +
  labs (
    x = "År",
    y = "Gennensnitshastighed (km/t)",
    title = "Gennemsnitshastigheden før/efter 30 års alderen",
    fill = "Years of cycling") +
  theme(plot.title = element_text(hjust = 0.5))
  
#activities by weekday --------------------------------------------------------

garmin_clean %>% 
  mutate(
    weekday = wday(date_onset, label = TRUE, abbr = FALSE, week_start = 1),
    weekday = recode(weekday,
      "Monday" = "Mandag",
      "Tuesday" = "Tirsdag",
      "Wednesday" = "Onsdag",
      "Thursday" = "Torsdag",
      "Friday" = "Fredag",
      "Saturday" = "Lørdag",
      "Sunday" = "Søndag")) %>% 
    count(weekday) %>% 
  ggplot(mapping = aes(
    x = weekday,
    y = n,
    fill = weekday)) +
  geom_col(
    show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 115, 15),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal() +
  labs(
    x = "Ugedage",
    y = "Antal aktiviteter",
    title = "Antal aktiviteter fordelt på ugedage") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5))

#activities by month --------------------------------------------------------
garmin_clean %>% 
  mutate(
    month = month(date_onset, label = TRUE, abbr = FALSE),
    month = recode(month,
                     "January" = "Januar",
                     "February" = "Februar",
                     "March" = "Marts",
                     "May" = "Maj",
                     "June" = "Juni",
                     "July" = "Juli",
                     "October" = "Oktober")) %>% 
  count(month) %>% 
  ggplot(mapping = aes(
    x = month,
    y = n,
    fill = month)) +
  geom_col(
    show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 90, 10),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal() +
  labs(
    x = "Måneder",
    y = "Antal aktiviteter",
    title = "Antal aktiviteter fordelt på måneder") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5))

# lineær regression mellem gennemsnitsfart og alder-----------------------

fart_model <- lm(avg_speed ~ year(date_onset), data = garmin_clean)
parameters(fart_model)
report(fart_model)
check_model(fart_model)

ggplot(data = garmin_clean,
       mapping = aes(
         x = year(date_onset),
         y = avg_speed,
         color = year(date_onset))) +
  geom_jitter(
    alpha = 0.3,
    width = 0.4,
    height = 0) +
  geom_smooth(method = lm, se = TRUE, color = "red") +
  scale_x_continuous(breaks = seq(2000, 2030, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 40, 5),
                     expand = c(0, 0)) +
  theme_minimal() +
  labs(
    x = "År",
    y = "Gennemsnitsfart",
    title = "Lineær regression mellem gennemsnitsfart og tid",
    caption = "Kilde: Privat Garmin data mellem 2013-06-09 til 2024-09-11",
    color = "År") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5))

# histogram over puls --------------------------------
garmin_clean %>% 
  filter(avg_hr > 115) %>% 
  ggplot(mapping = aes(x = avg_hr)) +
  geom_histogram(
    binwidth = 5, 
    aes(fill = after_stat(count)),
    show.legend = FALSE,
    color = "black") +
  scale_x_continuous(
    breaks = seq(0, 170, 5),
    expand = c(0, 0)) + 
  scale_y_continuous(
    breaks = seq(0, 80, 10),
    expand = c(0, 0)) +
  theme_minimal() +
  labs(
    x = "Gennemsnitspuls",
    y = "Antal aktiviteter",
    title = "Distribution af gennemsnitspuls under aktiviteter") +
  theme(
    plot.title = element_text(hjust = 0.5))


# Tid sammenlignet med distance i forhold til gennemsnitsfart----------

ggplot(garmin_clean, aes(x = time,
                         y = distance,
                         color = avg_speed)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 150, 20)) +
  annotate(x = 6000, y = 120, geom = "text", label = "Prikkernes farve som afspejler gennemsnitsfart viser 
           en tydelig sammenhæng mellem kortere ture og en højere gennemsnitsfart", 
           size = 3) +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.085),
        legend.direction = "horizontal",
        legend.title.position = "top") +
  labs(title = "Tid sammenlignet med distance",
       x = "Tid",
       y = "Distance (km)",
       color = "Gennemsnitsfart")

  

