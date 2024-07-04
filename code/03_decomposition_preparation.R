# R-code for Stillbirth Rates across Europe between 2010 and 2021: The Contribution of Maternal Age and Multiplicity
# code by Maxi S. Kniffka

# Calculation of decomposition over time (trends), calculation and plot of decomposition between countries for 2021

# preparation ------------------------------------------------------------------
library(tidyverse)

totals <- read_rds("./dat_out/total.rds")

age_data <- read_rds("./dat_out/by_maternal_age.rds") %>% 
  group_by(Country, Year) %>% 
  mutate(cx = (exposure_24) / sum(exposure_24),
         rate = deaths_24 / exposure_24 * 1000)

multi_data <- read_rds("./dat_out/by_multiplicity.rds") %>%
  group_by(Country, Year) %>% 
  mutate(cx = exposure_24 / sum(exposure_24),
         rate = deaths_24 / exposure_24 * 1000)

# Step 1.1: Decomposition over time. Maternal age ------------------------------

data_t1 <- age_data %>% 
  filter(Year == 2010) %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t1 = rate, cx_t1 = cx)

data_t1.2 <- age_data %>% # as there is no data on 2010 for France, Slovenia and the UK, the first time point is going to be 2015 for those countries
  filter(Year == 2015) %>% 
  filter(Country == "France" | Country == "UK" | Country == "Slovenia") %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t1 = rate, cx_t1 = cx)

data_t1 <- rbind(data_t1, data_t1.2)
remove(data_t1.2)

data_t2 <- age_data %>% 
  filter(Year == 2021) %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t2 = rate, cx_t2 = cx)


dekompo_time <- merge(data_t1, data_t2) %>% 
  mutate(Struktureffekt = (cx_t2 - cx_t1) * ((mx_t1 + mx_t2)/2),
         Sterblichkeitseffekt = (mx_t2 - mx_t1) * ((cx_t1 + cx_t2)/2)) %>% 
  group_by(Country) %>% 
  summarise(Structure = sum(Struktureffekt),
            Mortality = sum(Sterblichkeitseffekt)) %>% 
  ungroup()


rates_mean_t1 <- totals %>%
  filter(Country != "France" | Country != "UK" | Country != "Slovenia") %>% 
  filter(Year == "2010")

rates_mean_t1.2 <- totals %>%
  filter(Year == "2015") %>%
  filter(Country == "France" | Country == "UK" | Country == "Slovenia") 

rates_mean_t1 <- rbind(rates_mean_t1, rates_mean_t1.2)

rates_mean_t1 <- rates_mean_t1 %>% 
  mutate(rate_t1 = deaths_24 / exposure_24 * 1000) %>% 
  select(Country, rate_t1)

remove(rates_mean_t1.2)


rates_mean_t2 <- totals %>%
  filter(Year == "2021")

rates_mean_t2 <- rates_mean_t2 %>% 
  mutate(rate_t2 = deaths_24 / exposure_24 * 1000) %>%
  select(Country, rate_t2)

rates_mean <- merge(rates_mean_t1, rates_mean_t2)

dekompo_time <- merge(dekompo_time, rates_mean)

#plot prep

dekompo_time3 <- dekompo_time %>% 
  mutate(ymin_m = rate_t1,
         ymax_m = rate_t1 + Mortality) %>% 
  mutate(ymin_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m,
                            (Structure > 0 & Mortality < 0) ~ ymax_m,
                            (Structure < 0 & Mortality > 0) ~ ymax_m,
                            (Structure < 0 & Mortality < 0) ~ ymax_m
  )) %>% 
  mutate(ymax_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure > 0 & Mortality < 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality < 0) ~ ymax_m + Structure
  )) %>% 
  arrange(rate_t1) %>% 
  mutate(x_m = seq(1, length.out = n(), by = 3)) %>% 
  mutate(one = case_when(
    Mortality > 0 ~ 1,
    Mortality < 0 ~ 0
  ),
  two = case_when(
    Structure > 0 ~ 1,
    Structure < 0 ~ 0
  ),
  three = one + two,
  x_s = case_when(
    three == 1 ~ (x_m + 0.4),
    TRUE ~ x_m
  )
  ) %>% 
  mutate(y_curveup = case_when((Structure < 0 & Mortality > 0) ~ ymax_m)) %>%  
  mutate(x_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_m)) %>% 
  mutate(xmax_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_s)) %>% 
  mutate(y_curvedown = case_when((Structure > 0 & Mortality < 0) ~ ymax_m)) %>%  
  mutate(x_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_m)) %>% 
  mutate(xmax_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_s))

# Step 1.2: Decomposition over time. Multiplicity ------------------------------

data_t1_m <- multi_data %>% 
  filter(Year == 2010) %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t1 = rate, cx_t1 = cx)

data_t1.2_m <- multi_data %>% # as there is no data on 2010 for France, Slovenia, Spain and the UK, the first time point is going to be 2015 for those countries
  filter(Year == 2015) %>% 
  filter(Country == "France" | Country == "UK" | Country == "Spain" | Country == "Slovenia") %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t1 = rate, cx_t1 = cx)

data_t1_m <- rbind(data_t1_m, data_t1.2_m)
remove(data_t1.2_m)

data_t2_m <- multi_data %>% 
  filter(Year == 2021) %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t2 = rate, cx_t2 = cx)


dekompo_time_m <- merge(data_t1_m, data_t2_m) %>% 
  mutate(Struktureffekt = (cx_t2 - cx_t1) * ((mx_t1 + mx_t2)/2),
         Sterblichkeitseffekt = (mx_t2 - mx_t1) * ((cx_t1 + cx_t2)/2)) %>% 
  group_by(Country) %>% 
  summarise(Structure = sum(Struktureffekt),
            Mortality = sum(Sterblichkeitseffekt)) %>% 
  ungroup()


rates_mean_t1_m <- totals %>%
  filter(Country != "France" , Country != "UK" , Country != "Spain" , Country != "Slovenia") %>% 
  filter(Year == "2010")

rates_mean_t1.2_m <- totals %>%
  filter(Year == "2015") %>%
  filter(Country == "France" | Country == "UK" | Country == "Spain" | Country == "Slovenia")

rates_mean_t1_m <- rbind(rates_mean_t1_m, rates_mean_t1.2_m) %>% 
  mutate(rate_t1 = deaths_24 / exposure_24 * 1000) %>% 
  select(Country, rate_t1)

remove(rates_mean_t1.2_m)

rates_mean_t2_m <- totals %>%
  filter(Year == "2021") %>% 
  mutate(rate_t2 = deaths_24 / exposure_24 * 1000) %>% 
  select(Country, rate_t2)

rates_mean_m <- merge(rates_mean_t1_m, rates_mean_t2_m)

dekompo_time_m <- merge(dekompo_time_m, rates_mean_m)

dekompo_time3_m <- dekompo_time_m %>% 
  mutate(ymin_m = rate_t1,
         ymax_m = rate_t1 + Mortality) %>% 
  mutate(ymin_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m,
                            (Structure > 0 & Mortality < 0) ~ ymax_m,
                            (Structure < 0 & Mortality > 0) ~ ymax_m,
                            (Structure < 0 & Mortality < 0) ~ ymax_m
  )) %>% 
  mutate(ymax_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure > 0 & Mortality < 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality < 0) ~ ymax_m + Structure
  )) %>% 
  arrange(rate_t1) %>% 
  mutate(x_m = seq(2.5, length.out = n(), by = 3)) %>% 
  mutate(one = case_when(
    Mortality > 0 ~ 1,
    Mortality < 0 ~ 0
  ),
  two = case_when(
    Structure > 0 ~ 1,
    Structure < 0 ~ 0
  ),
  three = one + two,
  x_s = case_when(
    three == 1 ~ (x_m + 0.4),
    TRUE ~ x_m
  )
  ) %>% 
  mutate(y_curveup = case_when((Structure < 0 & Mortality > 0) ~ ymax_m)) %>%  
  mutate(x_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_m)) %>% 
  mutate(xmax_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_s)) %>% 
  mutate(y_curvedown = case_when((Structure > 0 & Mortality < 0) ~ ymax_m)) %>%  
  mutate(x_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_m)) %>% 
  mutate(xmax_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_s))

dekompo_time3_m <- dekompo_time3_m %>% 
  mutate(Country = paste0(Country, "2"))

age_and_multi <- rbind(dekompo_time3, dekompo_time3_m) %>% 
  arrange(x_m)

age_and_multi_table <- dekompo_time3_m %>% 
  select(Country, rate_t1, rate_t2, x_m) %>% 
  mutate(x_m = x_m - 1.5,
         rate_diff = rate_t2 - rate_t1)

write_rds(age_and_multi, "./dat_out/decomposition_time.rds")
write_rds(age_and_multi_table, "./dat_out/decomposition_time_table.rds")

# # Step 1.3: Decomposition over time. Plot --------------------------------------
# 
# dekompo_arrow_a_m <- ggplot(age_and_multi) +
#   geom_segment(aes(y = x_s, x = ymin_s,
#                    yend = x_s, xend = ymax_s),
#                colour = rep(c("#953735", "#4575b4"),  24), # repeat colors 24 times for 48 rows
#                lineend = "butt", linejoin = "mitre",
#                lwd = 3,
#                arrow = arrow(length = unit(0.04, "inches")))+
#   geom_curve(aes(y = x_curveup, x = y_curveup, yend = xmax_curveup, xend = y_curveup), curvature = 1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
#   geom_curve(aes(y = x_curvedown, x = y_curvedown, yend = xmax_curvedown, xend = y_curvedown), curvature = -1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
#   geom_segment(aes(y = x_m,  x = ymin_m,
#                    yend = x_m, xend = ymax_m
#   ),
#   colour = "grey", lwd = 3)+
#   geom_point(aes(y = x_m,
#                  x = rate_t1),
#              color = "black", size = 3) +
#   geom_text(data = age_and_multi_table, 
#     mapping = aes(x=6.3, y= x_m + 1, 
#                           label= round(rate_t1, digits = 1)), 
#             hjust=0, size = 7) +
#   annotate("text", x=6.35, y=75, label= "Rate", size = 7) + 
#   annotate("text", x=6.35, y=73, label= "2010", size = 7) + 
#   geom_text(data = age_and_multi_table, 
#     mapping = aes(x=6, y= x_m + 1, 
#                           label= round(rate_t2, digits = 1)), 
#             hjust=0, size = 7) +
#   annotate("text", x=6.05, y=75, label= "Rate", size = 7) + 
#   annotate("text", x=6.05, y=73, label= "2021", size = 7) + 
#   annotate("text", x=6.2, y=74, label= "-", size = 7) + 
#   annotate("text", x=6.55, y=74, label= "=", size = 7) + 
# 
#    annotate("text", x=6.73, y=75, label= "Rate", size = 7) + 
#    annotate("text", x=6.73, y=73, label= "diff.", size = 7) + 
#    geom_text(data = age_and_multi_table, 
#              mapping = aes(x=6.6, y= x_m + 1, 
#                            label= round(rate_diff, digits = 2)), 
#              hjust=0, size = 7) +
#   annotate("text", x=6.9, y=74, label= "=", size = 7) + 
#   
#   geom_text(data = age_and_multi, 
#             mapping = aes(x=7.4, y= x_m + 0.4, 
#                           label= round(Structure, digits = 2)), 
#             hjust=0, size = 7, colour = rep(c("#953735", "#4575b4"),  24)) +
#   annotate("text", x=7.56, y=75, label= "Structural", size = 7) + 
#   annotate("text", x=7.56, y=73, label= "effect", size = 7) + 
#   annotate("text", x=7.36, y=74, label= "+", size = 7) + 
#   geom_text(data = age_and_multi, 
#             mapping = aes(x=7, y= x_m + 0.4, 
#                           label= round(Mortality, digits = 2)), 
#             hjust=0, size = 7, color = "#737373") +
#   annotate("text", x=7.15, y=75, label= "Mortality", size = 7) + 
#   annotate("text", x=7.15, y=73, label= "effect", size = 7) + 
#   scale_y_continuous(breaks = c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71),
#                      labels =c("Finland", "Czech Rep.", "Switzerland", "Germany", "Cyprus", "Italy", "Austria", "Iceland", "Denmark" ,"Spain" ,"Norway", "Estonia", "Slovenia", "Sweden", "Poland",
#                                "France",  "UK", "Netherlands", "Malta", "Lithuania", "Belgium", "Croatia", "Ireland", "Latvia"))+
#   geom_hline(yintercept=c(3.4,6.4,9.4,12.4,15.4,18.4,21.4,24.4,27.4,30.4,33.4,36.4,39.4,42.4,45.4,48.4,51.4, 54.4, 57.4, 60.4, 63.4, 66.4, 69.4), color = "grey90") +
#   theme_classic(9,base_line_size = 0.2) +
#   theme(
#     text = element_text(size = 32),
#     legend.position="right",
#     panel.background=element_blank(),
#     legend.background=element_blank(),
#     panel.border=element_blank(),
#     panel.grid.major=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank(),
#     axis.title = element_text(size = 25)) +
#   annotate("text", x=5.3, y=65, label= "Difference explained by 
# multiplicity structure", size = 8, color = "#4575b4") + 
#   annotate("text", x=5.3, y=57.5, label= "Difference explained by 
# maternal age structure", size = 8, color = "#953735") + 
#   annotate("text", x=3.8, y=62.5, label= "Remaining mortality difference", 
#            size = 8, color = "#737373") + 
#   annotate("text", x=2.7, y=54, label= "Rate
# in 2021", size = 8) + 
#   annotate("text", x=4.4, y=54, label= "Rate
# in 2010", size = 8) + 
#   geom_segment(aes(y = 53, x = 2.9,
#                    yend = 52, xend = 3.15),
#                lineend = "butt", linejoin = "mitre",
#                lwd = 2,
#                arrow = arrow(length = unit(0.09, "inches")))+
#   geom_segment(aes(y = 53, x = 4.2,
#                    yend = 52, xend = 3.95),
#                lineend = "butt", linejoin = "mitre",
#                lwd = 2,
#                arrow = arrow(length = unit(0.09, "inches")))+
#   labs(
#     x = 'Stillbirths per 1000 Births', y = NULL,
#     title = '') 
# 
# dekompo_arrow_a_m 
# 
# ggsave("./plots/05_decompo_time.pdf", dekompo_arrow_a_m , width = 21, height = 20)
# ggsave("./plots/05_decompo_time.png", dekompo_arrow_a_m , width = 21, height = 20)


# Step 2.1: Decomposition between countries. Maternal age ----------------------

data_c1 <- age_data %>% 
  filter(Year == 2021) %>% 
  group_by(Characteristics) %>% 
  summarise(exposure_24 = sum(exposure_24),
            deaths_24 = sum(deaths_24)) %>% 
  ungroup() %>% 
  mutate(mx_t1 = deaths_24 / exposure_24 * 1000,
         cx_t1 = exposure_24 / sum(exposure_24)) %>% 
  select(Characteristics, mx_t1, cx_t1)

data_c2 <- age_data %>% 
  filter(Year == 2021) %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t2 = rate, cx_t2 = cx)


dekompo_count_ave_age <- merge(data_c1, data_c2) %>% 
  mutate(Struktureffekt = (cx_t2 - cx_t1) * ((mx_t1 + mx_t2)/2),
         Sterblichkeitseffekt = (mx_t2 - mx_t1) * ((cx_t1 + cx_t2)/2)) %>% 
  group_by(Country) %>% 
  summarise(Structure = sum(Struktureffekt),
            Mortality = sum(Sterblichkeitseffekt)) %>% 
  mutate(sum = Structure + Mortality) %>% 
  ungroup()

rates_mean_c1 <- totals %>%
  filter(Year == "2021") %>% 
  ungroup() %>% 
  summarise(exposure_24 = sum(exposure_24),
            deaths_24 = sum(deaths_24)) %>% 
  mutate(rates_mean_t1 = deaths_24 / exposure_24 * 1000) %>% 
  select(rates_mean_t1)


rates_mean_c2 <- totals %>%
  filter(Year == "2021") %>% 
  mutate(rates_mean_t2 = deaths_24 / exposure_24 * 1000)

rates_mean_c <- cbind(rates_mean_c2, rates_mean_c1) %>% 
  select(Country, rates_mean_t1, rates_mean_t2)

dekompo_count_ave_age <- merge(dekompo_count_ave_age, rates_mean_c)

dekompo_count_ave_age2 <- dekompo_count_ave_age %>% 
  mutate(ymin_m = rates_mean_t1,
         ymax_m = rates_mean_t1 + Mortality) %>% 
  mutate(ymin_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m,
                            (Structure > 0 & Mortality < 0) ~ ymax_m,
                            (Structure < 0 & Mortality > 0) ~ ymax_m,
                            (Structure < 0 & Mortality < 0) ~ ymax_m
  )) %>% 
  mutate(ymax_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure > 0 & Mortality < 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality < 0) ~ ymax_m + Structure
  )) %>% 
  arrange(rates_mean_t2) %>% 
  mutate(x_m = seq(1, length.out = n(), by = 3)) %>% 
  mutate(one = case_when(
    Mortality > 0 ~ 1,
    Mortality < 0 ~ 0
  ),
  two = case_when(
    Structure > 0 ~ 1,
    Structure < 0 ~ 0
  ),
  three = one + two,
  x_s = case_when(
    three == 1 ~ (x_m + 0.4),
    TRUE ~ x_m
  )
  ) %>% 
  mutate(y_curveup = case_when((Structure < 0 & Mortality > 0) ~ ymax_m)) %>%  
  mutate(x_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_m)) %>% 
  mutate(xmax_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_s)) %>% 
  mutate(y_curvedown = case_when((Structure > 0 & Mortality < 0) ~ ymax_m)) %>%  
  mutate(x_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_m)) %>% 
  mutate(xmax_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_s))



# Step 2.2: Decomposition between countries. Multiplicity ----------------------

data_m1 <- multi_data %>% 
  filter(Year == 2021) %>% 
  group_by(Characteristics) %>% 
  summarise(exposure_24 = sum(exposure_24),
            deaths_24 = sum(deaths_24)) %>% 
  ungroup() %>% 
  mutate(mx_t1 = deaths_24 / exposure_24 * 1000,
         cx_t1 = exposure_24 / sum(exposure_24)) %>% 
  select(Characteristics, mx_t1, cx_t1)

data_m2 <- multi_data %>% 
  filter(Year == 2021) %>% 
  ungroup() %>% 
  select(Characteristics, Country, mx_t2 = rate, cx_t2 = cx)

dekompo_count_ave_multi <- merge(data_m1, data_m2) %>% 
  mutate(Struktureffekt = (cx_t2 - cx_t1) * ((mx_t1 + mx_t2)/2),
         Sterblichkeitseffekt = (mx_t2 - mx_t1) * ((cx_t1 + cx_t2)/2)) %>% 
  group_by(Country) %>% 
  summarise(Structure = sum(Struktureffekt),
            Mortality = sum(Sterblichkeitseffekt)) %>% 
  mutate(sum = Structure + Mortality) %>% 
  ungroup()

dekompo_count_ave_multi <- merge(dekompo_count_ave_multi, rates_mean_c)

dekompo_count_ave_multi2 <- dekompo_count_ave_multi %>% 
  mutate(ymin_m = rates_mean_t1,
         ymax_m = rates_mean_t1 + Mortality) %>% 
  mutate(ymin_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m,
                            (Structure > 0 & Mortality < 0) ~ ymax_m,
                            (Structure < 0 & Mortality > 0) ~ ymax_m,
                            (Structure < 0 & Mortality < 0) ~ ymax_m
  )) %>% 
  mutate(ymax_s = case_when((Structure > 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure > 0 & Mortality < 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality > 0) ~ ymax_m + Structure,
                            (Structure < 0 & Mortality < 0) ~ ymax_m + Structure
  )) %>% 
  arrange(rates_mean_t2) %>% 
  mutate(x_m = seq(2.5, length.out = n(), by = 3)) %>% 
  mutate(one = case_when(
    Mortality > 0 ~ 1,
    Mortality < 0 ~ 0
  ),
  two = case_when(
    Structure > 0 ~ 1,
    Structure < 0 ~ 0
  ),
  three = one + two,
  x_s = case_when(
    three == 1 ~ (x_m + 0.4),
    TRUE ~ x_m
  )
  ) %>% 
  mutate(y_curveup = case_when((Structure < 0 & Mortality > 0) ~ ymax_m)) %>%  
  mutate(x_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_m)) %>% 
  mutate(xmax_curveup = case_when((Structure < 0 & Mortality > 0) ~ x_s)) %>% 
  mutate(y_curvedown = case_when((Structure > 0 & Mortality < 0) ~ ymax_m)) %>%  
  mutate(x_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_m)) %>% 
  mutate(xmax_curvedown = case_when((Structure > 0 & Mortality < 0) ~ x_s))


dekompo_count_ave_multi2 <- dekompo_count_ave_multi2 %>% 
  mutate(Country = paste0(Country, "2"))


age_and_multi_ave <- rbind(dekompo_count_ave_age2, dekompo_count_ave_multi2) %>% 
  arrange(x_m)


age_and_multi_ave_table <- dekompo_count_ave_multi2 %>% 
  select(Country, rates_mean_t1, rates_mean_t2, x_m) %>% 
  mutate(x_m = x_m - 1.5,
         rate_diff = rates_mean_t2 - rates_mean_t1)

write_rds(age_and_multi_ave, "./dat_out/decomposition_between_country.rds")
write_rds(age_and_multi_ave_table, "./dat_out/decomposition_between_country_table.rds")

# # Step 2.3: Decomposition between countries. Plot ------------------------------
# 
# 
# 
# dekompo_arrow_a_m_ave <- ggplot(age_and_multi_ave) +
#   geom_segment(aes(y = x_s, x = ymin_s,
#                    yend = x_s, xend = ymax_s),
#                colour = rep(c("#953735", "#4575b4"),  24),
#                lineend = "butt", linejoin = "mitre",
#                lwd = 3,
#                arrow = arrow(length = unit(0.04, "inches")))+
#   geom_curve(aes(y = x_curveup, x = y_curveup, yend = xmax_curveup, xend = y_curveup), curvature = 1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
#   geom_curve(aes(y = x_curvedown, x = y_curvedown, yend = xmax_curvedown, xend = y_curvedown), curvature = -1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
#   geom_segment(aes(y = x_m,  x = ymin_m,
#                    yend = x_m, xend = ymax_m
#   ),
#   color = "grey", lwd = 3)+ 
#   geom_point(aes(y = x_m,
#                  x = rates_mean_t1),
#              color = "black", size = 3) +
#   geom_text(data = age_and_multi_ave_table, 
#             mapping = aes(x=6.3, y= x_m + 1, 
#                           label= round(rates_mean_t1, digits = 2)), 
#             hjust=0, size = 7) +
#   annotate("text", x=6.35, y=75, label= "Ave.", size = 7) + 
#   annotate("text", x=6.35, y=73, label= "Rate", size = 7) + 
#   annotate("text", x=6.2, y=74, label= "-", size = 7) + 
#   
#   geom_text(data = age_and_multi_ave_table, 
#             mapping = aes(x=6, y= x_m + 1, 
#                           label= round(rates_mean_t2, digits = 2)), 
#             hjust=0, size = 7) +
#   annotate("text", x=6.05, y=75, label= "Nat.", size = 7) + 
#   annotate("text", x=6.05, y=73, label= "Rate", size = 7) + 
#   annotate("text", x=6.55, y=74, label= "=", size = 7) + 
#    geom_text(data = age_and_multi_ave_table, 
#              mapping = aes(x=6.6, y= x_m + 1, 
#                            label= round(rate_diff, digits = 2)), 
#              hjust=0, size = 7) +
#    annotate("text", x=6.73, y=75, label= "Rate", size = 7) + 
#    annotate("text", x=6.73, y=73, label= "diff.", size = 7) + 
#   annotate("text", x=6.9, y=74, label= "=", size = 7) + 
#   geom_text(data = age_and_multi_ave, 
#             mapping = aes(x=7.4, y= x_m + 0.4, 
#                           label= round(Structure, digits = 2)), 
#             hjust=0, size = 7, colour = rep(c("#953735", "#4575b4"),  24)) +
#   annotate("text", x=7.57, y=75, label= "Structural", size = 7) + 
#   annotate("text", x=7.57, y=73, label= "effect", size = 7) + 
#   
#   annotate("text", x=7.35, y=74, label= "+", size = 7) + 
#   
#   geom_text(data = age_and_multi_ave, 
#             mapping = aes(x=7, y= x_m + 0.4, 
#                           label= round(Mortality, digits = 2)), 
#             hjust=0, size = 7, color = "#737373") +
#   annotate("text", x=7.15, y=75, label= "Rate", size = 7) + 
#   annotate("text", x=7.15, y=73, label= "effect", size = 7) + 
# 
#   scale_y_continuous(breaks = c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71),
#                      labels = c("Finland", "Estonia", "Slovenia", "Norway", "Spain", "Italy", "Denmark", "Sweden", "Malta", "Switzerland", "Austria",
#                                 "Netherlands", "Czech Rep.", "Croatia", "Poland", "Lithuania", "Iceland", "UK", "Germany",
#                                 "Ireland", "France", "Cyprus", "Latvia", "Belgium"))+
#   geom_hline(yintercept=c(3.4,6.4,9.4,12.4,15.4,18.4,21.4,24.4,27.4,30.4,33.4,36.4,39.4,42.4,45.4,48.4,51.4, 54.4, 57.4, 60.4, 63.4, 66.4, 69.4, 72.4), color = "grey90") +
#   theme_classic(9,base_line_size = 0.2) +
#   theme_classic(9,base_line_size = 0.2) +
#   theme(
#     text = element_text(size = 32),
#     legend.position="right",
#     panel.background=element_blank(),
#     legend.background=element_blank(),
#     panel.border=element_blank(),
#     panel.grid.major=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank(),
#     axis.title = element_text(size = 25)) +
#   annotate("text", x=4.6, y=64, label= "Difference explained by 
# multiplicity structure", size = 8, color = "#4575b4") + 
#   annotate("text", x=4.4, y=58, label= "Difference explained by 
# maternal age structure", size = 8, color = "#953735") + 
#   annotate("text", x=5, y=68, label= "Remaining mortality difference", 
#            size = 8, color = "#737373") + 
#   annotate("text", x=2.7, y=53, label= "Average rate
# in 2021", size = 8) + 
#   annotate("text", x=4.3, y=53, label= "National rate
# in 2021", size = 8) + 
#   geom_segment(aes(y = 53, x = 3,
#                    yend = 52, xend = 3.35),
#                lineend = "butt", linejoin = "mitre",
#                lwd = 2,
#                arrow = arrow(length = unit(0.09, "inches")))+
#   geom_segment(aes(y = 53, x = 4,
#                    yend = 52, xend = 3.8),
#                lineend = "butt", linejoin = "mitre",
#                lwd = 2,
#                arrow = arrow(length = unit(0.09, "inches")))+
#   labs(
#     x = 'Stillbirths per 1000 Births', y = NULL,
#     title = '') 
# 
# dekompo_arrow_a_m_ave 
# ggsave("./plots/06_decompo_between_country.pdf", dekompo_arrow_a_m_ave , width = 21, height = 20)
# ggsave("./plots/06_decompo_between_country.png", dekompo_arrow_a_m_ave , width = 21, height = 20)
