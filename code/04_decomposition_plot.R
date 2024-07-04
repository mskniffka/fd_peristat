# R-code for Stillbirth Rates across Europe between 2010 and 2021: The Contribution of Maternal Age and Multiplicity
# code by Maxi S. Kniffka

# Plot of decomposition over time (trends), calculation and plot of decomposition between countries for 2021

# preparation ------------------------------------------------------------------
library(tidyverse)

age_and_multi <- read_rds("./dat_out/decomposition_time.rds")
age_and_multi_table <- read_rds("./dat_out/decomposition_time_table.rds")

age_and_multi_ave <- read_rds("./dat_out/decomposition_between_country.rds")
age_and_multi_ave_table <- read_rds("./dat_out/decomposition_between_country_table.rds")

# Step 1.3: Decomposition over time. Plot --------------------------------------

dekompo_arrow_a_m <- ggplot(age_and_multi) +
  geom_segment(aes(y = x_s, x = ymin_s,
                   yend = x_s, xend = ymax_s),
               colour = rep(c("#953735", "#4575b4"),  24), # repeat colors 24 times for 48 rows
               lineend = "butt", linejoin = "mitre",
               lwd = 3,
               arrow = arrow(length = unit(0.04, "inches")))+
  geom_curve(aes(y = x_curveup, x = y_curveup, yend = xmax_curveup, xend = y_curveup), curvature = 1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
  geom_curve(aes(y = x_curvedown, x = y_curvedown, yend = xmax_curvedown, xend = y_curvedown), curvature = -1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
  geom_segment(aes(y = x_m,  x = ymin_m,
                   yend = x_m, xend = ymax_m
  ),
  colour = "grey", lwd = 3)+
  geom_point(aes(y = x_m,
                 x = rate_t1),
             color = "black", size = 3) +
  geom_text(data = age_and_multi_table, 
            mapping = aes(x=6.3, y= x_m + 1, 
                          label= round(rate_t1, digits = 1)), 
            hjust=0, size = 7) +
  annotate("text", x=6.35, y=75, label= "Rate", size = 7) + 
  annotate("text", x=6.35, y=73, label= "2010", size = 7) + 
  geom_text(data = age_and_multi_table, 
            mapping = aes(x=6, y= x_m + 1, 
                          label= round(rate_t2, digits = 1)), 
            hjust=0, size = 7) +
  annotate("text", x=6.05, y=75, label= "Rate", size = 7) + 
  annotate("text", x=6.05, y=73, label= "2021", size = 7) + 
  annotate("text", x=6.2, y=74, label= "-", size = 7) + 
  annotate("text", x=6.55, y=74, label= "=", size = 7) + 
  
  annotate("text", x=6.73, y=75, label= "Rate", size = 7) + 
  annotate("text", x=6.73, y=73, label= "diff.", size = 7) + 
  geom_text(data = age_and_multi_table, 
            mapping = aes(x=6.6, y= x_m + 1, 
                          label= round(rate_diff, digits = 2)), 
            hjust=0, size = 7) +
  annotate("text", x=6.9, y=74, label= "=", size = 7) + 
  
  geom_text(data = age_and_multi, 
            mapping = aes(x=7.4, y= x_m + 0.4, 
                          label= round(Structure, digits = 2)), 
            hjust=0, size = 7, colour = rep(c("#953735", "#4575b4"),  24)) +
  annotate("text", x=7.56, y=75, label= "Structural", size = 7) + 
  annotate("text", x=7.56, y=73, label= "effect", size = 7) + 
  annotate("text", x=7.36, y=74, label= "+", size = 7) + 
  geom_text(data = age_and_multi, 
            mapping = aes(x=7, y= x_m + 0.4, 
                          label= round(Mortality, digits = 2)), 
            hjust=0, size = 7, color = "#737373") +
  annotate("text", x=7.15, y=75, label= "Mortality", size = 7) + 
  annotate("text", x=7.15, y=73, label= "effect", size = 7) + 
  scale_y_continuous(breaks = c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71),
                     labels =c("Finland", "Czech Rep.", "Switzerland", "Germany", "Cyprus", "Italy", "Austria", "Iceland", "Denmark" ,"Spain" ,"Norway", "Estonia", "Slovenia", "Sweden", "Poland",
                               "France",  "UK", "Netherlands", "Malta", "Lithuania", "Belgium", "Croatia", "Ireland", "Latvia"))+
  geom_hline(yintercept=c(3.4,6.4,9.4,12.4,15.4,18.4,21.4,24.4,27.4,30.4,33.4,36.4,39.4,42.4,45.4,48.4,51.4, 54.4, 57.4, 60.4, 63.4, 66.4, 69.4), color = "grey90") +
  theme_classic(9,base_line_size = 0.2) +
  theme(
    text = element_text(size = 32),
    legend.position="right",
    panel.background=element_blank(),
    legend.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    axis.title = element_text(size = 25)) +
  annotate("text", x=5.3, y=65, label= "Difference explained by 
multiplicity structure", size = 8, color = "#4575b4") + 
  annotate("text", x=5.3, y=57.5, label= "Difference explained by 
maternal age structure", size = 8, color = "#953735") + 
  annotate("text", x=3.8, y=62.5, label= "Remaining mortality difference", 
           size = 8, color = "#737373") + 
  annotate("text", x=2.7, y=54, label= "Rate
in 2021", size = 8) + 
  annotate("text", x=4.4, y=54, label= "Rate
in 2010", size = 8) + 
  geom_segment(aes(y = 53, x = 2.9,
                   yend = 52, xend = 3.15),
               lineend = "butt", linejoin = "mitre",
               lwd = 2,
               arrow = arrow(length = unit(0.09, "inches")))+
  geom_segment(aes(y = 53, x = 4.2,
                   yend = 52, xend = 3.95),
               lineend = "butt", linejoin = "mitre",
               lwd = 2,
               arrow = arrow(length = unit(0.09, "inches")))+
  labs(
    x = 'Stillbirths per 1000 Births', y = NULL,
    title = '') 

dekompo_arrow_a_m 

ggsave("./plots/05_decompo_time.pdf", dekompo_arrow_a_m , width = 21, height = 20)
ggsave("./plots/05_decompo_time.png", dekompo_arrow_a_m , width = 21, height = 20)


# Step 2.3: Decomposition between countries. Plot ------------------------------



dekompo_arrow_a_m_ave <- ggplot(age_and_multi_ave) +
  geom_segment(aes(y = x_s, x = ymin_s,
                   yend = x_s, xend = ymax_s),
               colour = rep(c("#953735", "#4575b4"),  24),
               lineend = "butt", linejoin = "mitre",
               lwd = 3,
               arrow = arrow(length = unit(0.04, "inches")))+
  geom_curve(aes(y = x_curveup, x = y_curveup, yend = xmax_curveup, xend = y_curveup), curvature = 1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
  geom_curve(aes(y = x_curvedown, x = y_curvedown, yend = xmax_curvedown, xend = y_curvedown), curvature = -1, colour = rep(c("#953735", "#4575b4"),  24), lwd = 3) + 
  geom_segment(aes(y = x_m,  x = ymin_m,
                   yend = x_m, xend = ymax_m
  ),
  color = "grey", lwd = 3)+ 
  geom_point(aes(y = x_m,
                 x = rates_mean_t1),
             color = "black", size = 3) +
  geom_text(data = age_and_multi_ave_table, 
            mapping = aes(x=6.3, y= x_m + 1, 
                          label= round(rates_mean_t1, digits = 2)), 
            hjust=0, size = 7) +
  annotate("text", x=6.35, y=75, label= "Ave.", size = 7) + 
  annotate("text", x=6.35, y=73, label= "Rate", size = 7) + 
  annotate("text", x=6.2, y=74, label= "-", size = 7) + 
  
  geom_text(data = age_and_multi_ave_table, 
            mapping = aes(x=6, y= x_m + 1, 
                          label= round(rates_mean_t2, digits = 2)), 
            hjust=0, size = 7) +
  annotate("text", x=6.05, y=75, label= "Nat.", size = 7) + 
  annotate("text", x=6.05, y=73, label= "Rate", size = 7) + 
  annotate("text", x=6.55, y=74, label= "=", size = 7) + 
  geom_text(data = age_and_multi_ave_table, 
            mapping = aes(x=6.6, y= x_m + 1, 
                          label= round(rate_diff, digits = 2)), 
            hjust=0, size = 7) +
  annotate("text", x=6.73, y=75, label= "Rate", size = 7) + 
  annotate("text", x=6.73, y=73, label= "diff.", size = 7) + 
  annotate("text", x=6.9, y=74, label= "=", size = 7) + 
  geom_text(data = age_and_multi_ave, 
            mapping = aes(x=7.4, y= x_m + 0.4, 
                          label= round(Structure, digits = 2)), 
            hjust=0, size = 7, colour = rep(c("#953735", "#4575b4"),  24)) +
  annotate("text", x=7.57, y=75, label= "Structural", size = 7) + 
  annotate("text", x=7.57, y=73, label= "effect", size = 7) + 
  
  annotate("text", x=7.35, y=74, label= "+", size = 7) + 
  
  geom_text(data = age_and_multi_ave, 
            mapping = aes(x=7, y= x_m + 0.4, 
                          label= round(Mortality, digits = 2)), 
            hjust=0, size = 7, color = "#737373") +
  annotate("text", x=7.15, y=75, label= "Rate", size = 7) + 
  annotate("text", x=7.15, y=73, label= "effect", size = 7) + 
  
  scale_y_continuous(breaks = c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71),
                     labels = c("Finland", "Estonia", "Slovenia", "Norway", "Spain", "Italy", "Denmark", "Sweden", "Malta", "Switzerland", "Austria",
                                "Netherlands", "Czech Rep.", "Croatia", "Poland", "Lithuania", "UK", "Germany", "Iceland",
                                "Ireland", "France", "Cyprus", "Latvia", "Belgium"))+
  geom_hline(yintercept=c(3.4,6.4,9.4,12.4,15.4,18.4,21.4,24.4,27.4,30.4,33.4,36.4,39.4,42.4,45.4,48.4,51.4, 54.4, 57.4, 60.4, 63.4, 66.4, 69.4, 72.4), color = "grey90") +
  theme_classic(9,base_line_size = 0.2) +
  theme_classic(9,base_line_size = 0.2) +
  theme(
    text = element_text(size = 32),
    legend.position="right",
    panel.background=element_blank(),
    legend.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    axis.title = element_text(size = 25)) +
  annotate("text", x=4.6, y=64, label= "Difference explained by 
multiplicity structure", size = 8, color = "#4575b4") + 
  annotate("text", x=4.4, y=58, label= "Difference explained by 
maternal age structure", size = 8, color = "#953735") + 
  annotate("text", x=5, y=68, label= "Remaining mortality difference", 
           size = 8, color = "#737373") + 
  annotate("text", x=2.7, y=53, label= "Average rate
in 2021", size = 8) + 
  annotate("text", x=4.3, y=53, label= "National rate
in 2021", size = 8) + 
  geom_segment(aes(y = 53, x = 3,
                   yend = 52, xend = 3.35),
               lineend = "butt", linejoin = "mitre",
               lwd = 2,
               arrow = arrow(length = unit(0.09, "inches")))+
  geom_segment(aes(y = 53, x = 4,
                   yend = 52, xend = 3.8),
               lineend = "butt", linejoin = "mitre",
               lwd = 2,
               arrow = arrow(length = unit(0.09, "inches")))+
  labs(
    x = 'Stillbirths per 1000 Births', y = NULL,
    title = '') 

dekompo_arrow_a_m_ave 
ggsave("./plots/06_decompo_between_country.pdf", dekompo_arrow_a_m_ave , width = 21, height = 20)
ggsave("./plots/06_decompo_between_country.png", dekompo_arrow_a_m_ave , width = 21, height = 20)

