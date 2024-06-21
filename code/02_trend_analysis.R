# R-code for Stillbirth Rates across Europe between 2010 and 2021: The Contribution of Maternal Age and Multiplicity
# code by Maxi S. Kniffka

# Linear modeling of trends, Mann-Kendall test, Plots for trends and population structure, 
# Plot of stillbirth risk by maternal age over time

# Preperation  -----------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(funtimes)
library(cowplot)

set.seed(2024)

totals <- read_rds("./dat_out/total.rds") %>%  # this includes annual aggregated data for all countries
mutate(rate = deaths_24 / exposure_24 * 1000) # calculating stillbirth rates

age_data <- read_rds("./dat_out/by_maternal_age.rds") %>% # this includes detailed data for 2010, 2015 and 2021 for all countries with information on maternal age
  group_by(Country, Year) %>% 
  mutate(cx = (exposure_24) / sum(exposure_24) * 100)

multi_data <- read_rds("./dat_out/by_multiplicity.rds") %>% # this includes detailed data for 2010, 2015 and 2021 for all countries with information on multiplicity
  group_by(Country, Year) %>% 
  mutate(cx = (exposure_24) / sum(exposure_24) * 100)

# Step 1.1: Linear modeling of trends and Mann-Kendall-Test --------------------

# function for fitting a linear model

fit_model <- function(data) {
  model <- lm(rate ~ Year, data = data)
  predictions <- predict(model, interval = "confidence", level = 0.95)
  ci <- confint(model, level = 0.95)  # Calculate confidence intervals for model parameters
  data$predicted_rate <- predictions[,1] # predicted rate
  data$lower_ci <- predictions[,2] # lower confidence interval
  data$upper_ci <- predictions[,3] # upper confidence interval
  data$coefficient <- coef(model)[2]    # annual change of rates (slope of the linear fit)
  data$slope_lower_ci <- ci[2, 1]  # Lower confidence interval for the slope
  data$slope_upper_ci <- ci[2, 2]  # Upper confidence interval for the slope
  return(data)
}

# applying the function for linear fit

totals <- totals %>%
  group_by(Country) %>%
  do(fit_model(.))

# Mann-Kendall test for monotonic trend

tests <- # this includes all countries with a timeline starting in 2010
  totals %>%
  ungroup() %>%
  group_by(Country) %>%
  summarise(
    "sbrate_{2010}" :=
      rate[Year == 2010],
    "sbrate_{2021}" :=
      rate[Year == 2021],
    MK_p = notrend_test(
      as.ts(rate), B = 10000, test = 't',
      ar.order = 4,
    )[['p.value']]) %>%
  mutate(across(starts_with('sbrate'), ~ round(.x*1e3))) %>%
  ungroup()

tests2 <- # this includes all countries with a timeline starting in 2014
  totals %>%
  filter(Country == "France") %>% 
  ungroup() %>%
  group_by(Country) %>%
  summarise(
    "sbrate_{2010}" :=
      rate[Year == 2014],
    "sbrate_{2021}" :=
      rate[Year == 2021],
    MK_p = notrend_test(
      as.ts(rate), B = 10000, test = 't',
      ar.order = 4, 
    )[['p.value']]) %>%
  mutate(across(starts_with('sbrate'), ~ round(.x*1e3))) %>%
  ungroup()

tests3 <- # this includes all countries with a timeline starting in 2013
  totals %>%
  filter(Country == "UK" | Country == "Slovenia") %>% 
  ungroup() %>%
  group_by(Country) %>%
  summarise(
    "sbrate_{2010}" :=
      rate[Year == 2013],
    "sbrate_{2021}" :=
      rate[Year == 2021],
    MK_p = notrend_test(
      as.ts(rate), B = 10000, test = 't',
      ar.order = 4,
    )[['p.value']]) %>%
  mutate(across(starts_with('sbrate'), ~ round(.x*1e3))) %>%
  ungroup()

tests4 <- # this includes all countries with a timeline starting in 2015
  totals %>%
  filter(Country == "Luxembourg") %>% 
  ungroup() %>%
  group_by(Country) %>%
  summarise(
    "sbrate_{2010}" :=
      rate[Year == 2015],
    "sbrate_{2021}" :=
      rate[Year == 2021],
    MK_p = notrend_test(
      as.ts(rate), B = 10000, test = 't',
      ar.order = 4, 
    )[['p.value']]) %>%
  mutate(across(starts_with('sbrate'), ~ round(.x*1e3))) %>%
  ungroup()

tests <- rbind(tests, tests2, tests3, tests4)

remove(tests2, tests3, tests4)

coeff <- totals %>% # extracting the annual change in rates to add to the MK-test dataset
  select(Country, coefficient, slope_lower_ci, slope_upper_ci) %>% 
  unique()

tests <- tests %>% 
  merge(coeff, all = T) %>% 
  mutate(coefficient = round(coefficient, digits = 3),
         slope_lower_ci = round(slope_lower_ci, digits = 3),
         slope_upper_ci = round(slope_upper_ci, digits = 3),
         sig = case_when(
           (MK_p <= 0.05 & MK_p > 0.01) ~ "*",
           MK_p <= 0.01 ~ "**",
           TRUE ~ " "
         ),
         sig2 = paste0(coefficient, sig)
  ) %>% 
  arrange(coefficient)

tests <- tests %>% 
  mutate(coefficient_2 = paste0(coefficient, " [", slope_lower_ci, ", ", slope_upper_ci, "]"))

# save dataset on results of the MK test and annual change in rates

write_csv(tests, "./dat_out/01_table.csv")


# Step 1.2: Plot of trends -----------------------------------------------------

trend <- totals %>%
  ggplot() +
  aes(x = Year, y = rate) +
  geom_point(
    aes(y = rate),
    size = 1, shape = 1
  ) +
  geom_line(aes(y = predicted_rate), size = 1, color = "#d95f0e") +
     geom_ribbon(
       aes(ymin = lower_ci, ymax = upper_ci),
       alpha = 0.2, color = NA , fill = "#d95f0e"
     ) +
  geom_text(data = tests, 
            mapping = aes(x=2015, y= 5.5, 
                          label= sig2), 
            hjust=0, size = 6) +
  guides(size = 'none', color=guide_legend(nrow=3, byrow=TRUE)) +
  theme_bw(9, base_line_size = 0.2 ) +
  theme(
    text = element_text(size = 26),
    legend.position = 'top',
    legend.background = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.y = element_line())+
  labs(
    y = 'Stillbirths per 1000 Births', x = NULL
  ) +
  scale_x_continuous(breaks = seq(2010, 2021, by = 5))+
  facet_wrap(~factor(Country, c("Luxembourg", "Estonia", "Latvia", "UK", "Lithuania", "Norway", "Netherlands", "Malta", "Ireland", "Slovenia", "Poland",     
                                "Spain", "Sweden", "Denmark", "Croatia", "Finland", "Italy", "Switzerland", "Austria", "Cyprus", "France", "Czech Rep.", 
                                "Iceland", "Belgium", "Germany")) , ncol = 5) # arrange countries by annual change of rates

trend

# saving plots as png and pdf

ggsave("./plots/02_trends.png", trend,  height = 10, width = 20)
ggsave("./plots/02_trends.pdf", trend,  height = 10, width = 20)


# Step 2: Pregnancy composition over time --------------------------------------

# plot on maternal age structure over time

age <- age_data %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(Characteristics = factor(Characteristics, levels=c("Age 40 and above", "Age 35 to 39", "Age 30 to 34", "Age 25 to 29", "Age 20 to 24", "Age below 20" ))) %>% 
  ggplot(aes(fill = Characteristics, x = Year, y= cx))+
  geom_bar(position="stack", stat="identity", width = 0.95)+
  scale_fill_manual(values=c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4")) +
  labs(y = "Population share in %", x = "", title = "A: Maternal age groups") +
  guides(size = 'none', color=guide_legend(nrow=2, byrow=TRUE)) +  
  theme_bw(9, base_line_size = 0.2 ) +
  theme(
    text = element_text(size = 26),
    legend.position = 'top',
    legend.background = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.y = element_line())+
  facet_wrap(~ Country, ncol = 5)

age

# plot on multiplicity structure over time

multiples <- multi_data %>% 
  filter(Characteristics == "Births from multiple pregnancies") %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y= cx))+
  geom_bar(position="stack", stat="identity", width = 0.95, fill = "#4575b4")+
  guides(size = 'none', color=guide_legend(nrow=3, byrow=TRUE)) +  
  labs(y = "Multiple births per 100 total birth", x = "", title = "B: Births from multiple pregnancies") +
  theme_bw(9, base_line_size = 0.2 ) +
  theme(
    text = element_text(size = 26),
    legend.position = 'top',
    legend.background = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.y = element_line())+
  facet_wrap(~ Country, ncol = 5)

multiples

# combine them to a double plot

struktur <- plot_grid(age, multiples, rel_widths = c(1,2), nrow = 2)

struktur

# save plot as png and pdf

ggsave("./plots/03_structure.png", struktur, height = 20, width = 18)
ggsave("./plots/03_structure.pdf", struktur, height = 20, width = 18)


# Step 3.1: Risk of group specific stillbirth over time: Maternal age ----------

age_data_agg_year <- age_data %>% 
  filter(Country != "France", # no total timeline
         Country != "Slovenia", # no total timeline
         Country != "UK", # no total timeline
         Country != "Cyprus", # no exclusion of abortions
         Country != "Ireland", # no exclusion of abortions
         Country != "Belgium") %>% # no exclusion of abortions
  group_by(Year, Characteristics) %>% 
  summarise(deaths_24 = sum(deaths_24),
            exposure_24 = sum(exposure_24)) %>% 
  mutate(rate = deaths_24 / exposure_24 * 1000) %>% 
  mutate(Characteristics = factor(Characteristics, levels=c("Age below 20", "Age 20 to 24", "Age 25 to 29", "Age 30 to 34", "Age 35 to 39", "Age 40 and above" )))
  

age_data_agg_year %>%
  mutate(Year = as.character(Year)) %>% 
  ggplot() +
  aes(x = Characteristics, y = rate, group = Year, color = Year) +
  geom_line(
    aes(y = rate),
    size = 1, shape = 1
  ) +
  guides(size = 'none', color=guide_legend(nrow=3, byrow=TRUE)) +
  theme_classic(9, base_line_size = 0.2) +
  theme(
    legend.position = 'top',
    legend.background = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line(),
    text = element_text(size = 26)
  ) +
  labs(
    y = 'Stillbirths per 1000 Births', x = NULL
  ) 

ggsave("./plots/04_age_over_time.png",  height = 10, width = 20)
