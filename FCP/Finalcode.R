# %% import library
library(ggplot2)
library(dygraphs)
library(tseries)
library(quantmod)
library(dplyr)
library(tidyr)
library(hrbrthemes)

# %% read data
df1 <- read.csv("platform_economy_reviews.csv", header=TRUE)
df2 <- read.csv("platform_economy_mobility.csv")

# %% data processing
## change the date column to Date datatype
df1$time <- as.Date(paste0(df1$time, "-01", format = "%Y-%m-%d"))
df2$time <- as.Date(paste0(df2$time, "-01", format = "%Y-%m-%d"))

# %% caculate the mean bad_service and bad_food by location and time
df1_avg <- df1 %>%
  group_by(location, time) %>%
  summarise(
    avg_bad_service = mean(bad_service, na.rm = TRUE),
    avg_bad_food = mean(bad_food, na.rm = TRUE),
    avg_combined = mean(c(avg_bad_service, avg_bad_food), na.rm = TRUE)  
  )

# seperate austin & dallas data
austin_data <- df1_avg %>%
  filter(location == "austin") %>%
  select(time, avg_bad_service) %>%
  rename(austin_bad_service = avg_bad_service)

dallas_data <- df1_avg %>%
  filter(location == "dallas") %>%
  select(time, avg_bad_service) %>%
  rename(dallas_bad_service = avg_bad_service)

# conbine austin & dallas data and caculate the difference
difference_data <- austin_data %>%
  left_join(dallas_data, by = "time") %>%
  mutate(value = austin_bad_service - dallas_bad_service) %>%
  select(time, value)

# %% df2
## change booling to 0 & 1
df2_quit_bool <- df2 %>%
  mutate(quit = ifelse(quit == "True", 1, 0))

df2_avg <- df2_quit_bool %>%
  group_by(restaurant, time, dma) %>%
  summarise(
    avg_hourly_wage = mean(avg_hourly_wage, na.rm = TRUE),
    avg_tenure = mean(tenure, na.rm = TRUE),
    quit_total = sum(quit, na.rm = TRUE)) %>% 
  group_by(dma, time) %>%
  summarise(
    avg_hourly_wage = mean(avg_hourly_wage, na.rm = TRUE),
    avg_tenure = mean(avg_tenure, na.rm = TRUE),
    quit_total = sum(quit_total, na.rm = TRUE))

df_long <- df2_avg %>%
  pivot_longer(
    cols = c(avg_hourly_wage, avg_tenure, quit_total), # Specify the columns to be merged
    names_to = "source",            # new column to record the name
    values_to = "value"             # new column to store value
  )

# %% Figure 1
## plot bad_service & austin - dallas bad_service
ggplot() +
  # first line：avg_bad_service
  geom_line(
    data = df1_avg,
    aes(x = time, y = avg_bad_service, color = location, group = location),
    linewidth = 1
  ) +
  geom_smooth(
    data = df1_avg,
    aes(x = time, y = avg_bad_service, color = location),
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  # second bar：austin - dallas bad_service
  geom_bar(
    data = difference_data,
    aes(x = time, y = value, fill = value),
    stat = "identity",
    position = "dodge",
    alpha = 0.6
  ) +
  scale_color_manual(values = c("austin" = "#3c63ff", "dallas" = "#fdab54")) +
  labs(
    title = "Restaurant's Average Bad Service and Disparity by Location",
    x = "Time",
    color = "Location",
    subtitle = "Theme is theme_ipsum()", 
    caption = "Fig. 1"
  ) +
  # add two vlines
  geom_vline(
    xintercept = as.Date(c("2016-05-01", "2017-05-01")),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  # add labels
  annotate(
    "text",
    x = as.Date("2016-05-01"), y = 0.11,
    label = "Uber & Lift left",
    color = "purple",
    angle = 90,
    vjust = -0.5
  ) +
  annotate(
    "text",
    x = as.Date("2017-05-01"), y = 0.11,
    label = "Uber & Lift return",
    color = "#0d5b0d",
    angle = 90,
    vjust = -0.5
  ) +
  annotate(
    "text",
    x = as.Date("2014-07-01"), y = 0.05,
    label = "Regional Difference",
    color = "#0d5b0d",
    angle = 0,
    vjust = 0
  ) +
  theme_ipsum()

# %% Figure 2
## plot with 3 y axes (facet)
ggplot(data = df_long, aes(x = time, y = value, color = dma, group = dma)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c("AUSTIN" = "#12d2d9", "DALLAS" = "#fdab54")) +
  labs(
    title = "Average Wage, Tenure and Quit number of Restaurant",
    x = "Time",
    y = "Average Score",
    color = "Location",
    subtitle = "Theme is theme_minimal()", 
    caption = "Fig. 2"
  ) +
  # add two lines
  geom_vline(
    xintercept = as.Date(c("2016-05-01", "2017-05-01")),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  facet_grid(rows = vars(source), scales = "free_y") +
  theme_minimal()

