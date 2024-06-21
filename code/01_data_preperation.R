# R-code for Stillbirth Rates across Europe between 2010 and 2021: The Contribution of Maternal Age and Multiplicity
# code by Maxi S. Kniffka

# Read, bind and distribute missings

# Preperation  -----------------------------------------------------------------

set.seed(2024)

library(readxl)
library(tidyverse)
library(reshape2)

# Read in data -----------------------------------------------------------------

#austria

au <- read_xlsx("./dat/Data_template_Kniffka_AUSTRIA.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Austria")
au2 <- read_xlsx("./dat/Data_template_Kniffka_AUSTRIA.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Austria")

#belgium

be_a <- read_xlsx("./dat/Belgium/AGE_MTH_GEST24.xlsx") %>% 
  filter(CD_YEAR == "2010" | CD_YEAR == "2015" | CD_YEAR == "2021") %>%
  mutate(Country = "Belgium") %>% 
  select(Country, Year = 1, Characteristics = 3, `Live births >= 24 weeks` = 4)

be_b <- read_xlsx("./dat/Belgium/AGE_MTH_GEST28.xlsx") %>% 
  filter(CD_YEAR == "2010" | CD_YEAR == "2015" | CD_YEAR == "2021") %>% 
  select(`Live births >= 28 weeks` = 4)

be_c <- read_xlsx("./dat/Belgium/MLTPL_GEST24.xlsx") %>% 
  filter(CD_YEAR == "2010" | CD_YEAR == "2015" | CD_YEAR == "2021") %>%
  mutate(Country = "Belgium") %>% 
  select(Country, Year = 1, Characteristics = 3, `Live births >= 24 weeks` = 4)

be_d <- read_xlsx("./dat/Belgium/MLTPL_GEST28.xlsx") %>% 
  filter(CD_YEAR == "2010" | CD_YEAR == "2015" | CD_YEAR == "2021") %>% 
  select(`Live births >= 28 weeks` = 4)

be_e <- rbind(be_a, be_c)

be_f <- rbind(be_b, be_d)

be_g <- cbind(be_e, be_f)

be_h <- read_xlsx("./dat/Belgium/T_2023_121_STILLBIRTH_B.xlsx") %>% 
  filter(CD_AGE_MTH != "UNK") %>% 
  select(`Fetal deaths >= 24 weeks` = 3, `Fetal deaths >= 28 weeks` = 4)

be_i <- read_xlsx("./dat/Belgium/T_2023_121_STILLBIRTH_C.xlsx") %>% 
  select(`Fetal deaths >= 24 weeks` = 3, `Fetal deaths >= 28 weeks` = 4)

be_j <- rbind(be_h, be_i)

be <- cbind(be_g, be_j) %>% 
  arrange(Year) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "00-19" ~ "Age below 20",
    Characteristics == "20-24" ~ "Age 20 to 24",
    Characteristics == "25-30" ~ "Age 25 to 29",
    Characteristics == "30-34" ~ "Age 30 to 34",
    Characteristics == "35-40" ~ "Age 35 to 39",
    Characteristics == "40-99" ~ "Age 40 and above",
    Characteristics == "1" ~ "Births from multiple pregnancies",
    Characteristics == "2" ~ "Births from singleton pregnancies"
  ))

remove(be_a, be_b, be_c, be_d, be_e, be_f, be_g, be_h, be_i, be_j)

be2_a <- read_xlsx("./dat/Belgium/GEST24.xlsx") %>% 
  mutate(Country = "Belgium") %>% 
  select(Country, Year = 1, `Live births >= 24 weeks` = 3)

be2_b <- read_xlsx("./dat/Belgium/GEST28.xlsx") %>% 
  select(`Live births >= 28 weeks` = 3)

be2_c <- read_xlsx("./dat/Belgium/T_2023_121_STILLBIRTH_A.xlsx") %>% 
  select(`Fetal deaths >= 24 weeks` = 2, `Fetal deaths >= 28 weeks` = 3)

be2 <- cbind(be2_a, be2_b, be2_c)
remove(be2_a, be2_b, be2_c)

#croatia

cr <- read_xlsx("./dat/Croatian Data_template_Kniffka.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Croatia")
cr2 <- read_xlsx("./dat/Croatian Data_template_Kniffka.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Croatia")


#cyprus

cy <- read_xlsx("./dat/Data_template_Kniffka_CY.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Cyprus")
cy2 <- read_xlsx("./dat/Data_template_Kniffka_CY.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Cyprus")#totals by gestation not by 2011 to 2013


#czechia

cz <- read_xlsx("./dat/Data_template_Kniffka_Czechia.xlsx", sheet = 2, range = "A1:G25") %>% 
  mutate(Country = "Czech Rep.")
cz2 <- read_xlsx("./dat/Data_template_Kniffka_Czechia.xlsx", sheet = 3, range = "A1:F13") %>% 
  mutate(Country = "Czech Rep.") 

#denmark

vals <- seq(1, 3, 1)


dk <- read_xlsx("./dat/Data_template_Kniffka_DK.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Denmark")    # two cells needs to be estimated

dk$`Fetal deaths >= 24 weeks`[is.na(dk$`Fetal deaths >= 24 weeks`)] <- sample(vals, sum(is.na(dk$`Fetal deaths >= 24 weeks`)), replace = TRUE)
dk$`Fetal deaths >= 28 weeks`[is.na(dk$`Fetal deaths >= 28 weeks`)] <- sample(vals, sum(is.na(dk$`Fetal deaths >= 28 weeks`)), replace = TRUE)

dk2 <- read_xlsx("./dat/Data_template_Kniffka_DK.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Denmark")



#estonia

est <- read_xlsx("./dat/Data_template_Kniffka_Estonia.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Estonia")
est2 <- read_xlsx("./dat/Data_template_Kniffka_Estonia.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Estonia")


#finland

fi <- read_xlsx("./dat/Data_template_Kniffka _Finland.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Finland")
fi2 <- read_xlsx("./dat/Data_template_Kniffka _Finland.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Finland")


#france

fr <- read_xlsx("./dat/Data_template_Kniffka_AV_qualiN_france.xlsx", range = "A2:G26")[-c(1:8),] %>% 
  mutate(Country = "France") #only 2015 and 2021
fr2 <- read_xlsx("./dat/Data_template_Kniffka_AV_qualiN_france.xlsx", sheet = 2, range = "A1:F13")[-c(1:4),] %>% 
  mutate(Country = "France") #only from 2014 

#germany

de <- read_csv2("./dat/Germany/ET_alter_24SSW.csv", skip = 2)[-c(7,14,21),] %>% 
  mutate(Country = "Germany") %>% 
  select(Country, Year = 2, Characteristics = 3, `Live births >= 24 weeks` = 5, `Fetal deaths >= 24 weeks` = 6) %>% 
  fill(Year)

de2 <- read_csv2("./dat/Germany/ET_alter_28SSW.csv", skip = 2)[-c(7,14,21),] %>% 
  select(`Live births >= 28 weeks` = 5, `Fetal deaths >= 28 weeks` = 6)

de <- de %>% 
  cbind(de2)

de3 <- read_csv2("./dat/Germany/ET_mehrling_24SSW.csv", skip = 2) %>% 
  mutate(Country = "Germany") %>% 
  select(Country, Year = 2, Characteristics = 3, `Live births >= 24 weeks` = 5, `Fetal deaths >= 24 weeks` = 6) %>% 
  fill(Year)

de4 <- read_csv2("./dat/Germany/ET_mehrling_28SSW.csv", skip = 2) %>% 
  select(`Live births >= 28 weeks` = 5, `Fetal deaths >= 28 weeks` = 6)

de3 <- de3 %>% 
  cbind(de4)

de <- de %>% 
  rbind(de3)

remove(de2, de3, de4)

de <- de %>% 
  mutate(Characteristics = case_when(
    Characteristics == "unter 20" ~ "Age below 20",
    Characteristics == "20-24" ~ "Age 20 to 24",
    Characteristics == "25-29" ~ "Age 25 to 29",
    Characteristics == "30-34" ~ "Age 30 to 34",
    Characteristics == "35-39" ~ "Age 35 to 39",
    Characteristics == "Mehrling" ~ "Births from multiple pregnancies",
    Characteristics == "Einling" ~ "Births from singleton pregnancies",
    TRUE ~ "Age 40 and above",
  )) %>% 
  arrange(Year) %>% 
  select(Country, Year, Characteristics, `Live births >= 24 weeks`, `Live births >= 28 weeks`, `Fetal deaths >= 24 weeks`, `Fetal deaths >= 28 weeks`)


de2 <- read_csv2("./dat/Germany/ET_24SSW.csv") %>% 
  mutate(Country = "Germany") %>% 
  select(Country, Year = 1, `Live births >= 24 weeks` = 2, `Fetal deaths >= 24 weeks` = 3) 
  
de3 <- read_csv2("./dat/Germany/ET_28SSW.csv") %>% 
  mutate(Country = "Germany") %>% 
  select(`Live births >= 28 weeks` = 2, `Fetal deaths >= 28 weeks` = 3) 

de2 <- de2 %>% 
  cbind(de3) %>% 
  select(Country, Year, `Live births >= 24 weeks`, `Live births >= 28 weeks`, `Fetal deaths >= 24 weeks`, `Fetal deaths >= 28 weeks`)

remove(de3)


#iceland

ic <- read_xlsx("./dat/Iceland_V2.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Iceland")
ic2 <- read_xlsx("./dat/Iceland_V2.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Iceland")


#ireland

ir <- read_xlsx("./dat/N230009_NPRS_Information_Ireland.xlsx", range = "A17:G41") %>% 
  mutate(Country = "Ireland")
ir2 <- read_xlsx("./dat/N230009_NPRS_Information_Ireland.xlsx", sheet = 2, range = "A27:F39") %>% 
  mutate(Country = "Ireland")

#italy

it <- read_xlsx("./dat/ITA-Data_template_Kniffka.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Italy")
it2 <- read_xlsx("./dat/ITA-Data_template_Kniffka.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Italy")


#latvia

lv <- read_xlsx("./dat/SPKC_Vest_P01_attachment.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Latvia")
lv2 <- read_xlsx("./dat/SPKC_Vest_P01_attachment.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Latvia")

#lithuania

lt <- read_xlsx("./dat/Data_template_Kniffka_LT_2021_2015_2010.xlsx", range = "A1:I26")[-7,-c(4,7)]%>% 
  mutate(Country = "Lithuania")
lt2 <- read_xlsx("./dat/Data_template_Kniffka_LT_2021_2015_2010.xlsx", sheet = 2, range = "A1:H13")[,-c(3,6)] %>% 
  mutate(Country = "Lithuania")

#luxembourg

lu2 <- read_xlsx("./dat/Data_template_Kniffka_LIH.xlsx", sheet = 2, range = "A1:H13")[,-c(7,8)] %>% 
  mutate(Country = "Luxembourg") %>% 
  filter(Year >= 2015)


#malta

ma <- read_xlsx("./dat/Maxi kniffka stillbirths Data_template_ submitted-1.xlsx", range = "A1:I26")[-25,-c(8,9)]%>% 
  mutate(Country = "Malta")
ma2 <- read_xlsx("./dat/Maxi kniffka stillbirths Data_template_ submitted-1.xlsx", sheet = 2, range = "A1:H13")[,-c(7,8)] %>% 
  mutate(Country = "Malta")

#netherlands

ne <- read_xlsx("./dat/Data_template_Kniffka_netherlands.xlsx", range = "A1:G28")[-c(7,16,25),] %>% 
  mutate(Country = "Netherlands")
ne2 <- read_xlsx("./dat/Data_template_Kniffka_netherlands.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Netherlands")

#norway

no <- read_xlsx("./dat/Kopi av Data_template_Kniffka.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Norway")
no2 <- read_xlsx("./dat/Kopi av Data_template_Kniffka.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Norway")



#poland

po <- read_xlsx("./dat/Data_template_Kniffka_Poland_KSz_corr.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Poland")
po2 <- read_xlsx("./dat/Data_template_Kniffka_Poland_KSz_corr.xlsx", sheet = 2, range = "A1:F13")[-c(6,7,8),] %>% 
  mutate(Country = "Poland")


#slovenia

slo <- read_xlsx("./dat/Slo Data Kniffka 2.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Slovenia") %>% 
  filter(Year != "2010")

slo2 <- read_xlsx("./dat/Slo Data Kniffka 2.xlsx", sheet = 2, range = "A2:H14")[,-c(5,6)] %>% 
  mutate(Country = "Slovenia") %>% 
  filter(Year >= "2013") %>% 
  rename(`Fetal deaths >= 24 weeks` = 5, `Fetal deaths >= 28 weeks` = 6)


#spain

es <- read_xlsx("./dat/Data_template_Kniffka_Spain_data.xlsx", range = "A1:G25")[-c(7,8),] %>% 
  mutate(Country = "Spain")
es2 <- read_xlsx("./dat/Data_template_Kniffka_Spain_data.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Spain")

#sweden

sw <- read_xlsx("./dat/Data_Sweden_Kniffka.xlsx", range = "A1:G25") %>% 
  mutate(Country = "Sweden")
sw2 <- read_xlsx("./dat/Data_Sweden_Kniffka.xlsx", sheet = 2, range = "A1:F13") %>% 
  mutate(Country = "Sweden")

#switzerland

che <- read_xlsx("./dat/CHE_Data_template_Kniffka_TR_final.xlsx", sheet = 2, range = "A1:G25") %>% 
  mutate(Country = "Switzerland") %>% 
  mutate(`Fetal deaths >= 24 weeks` = case_when(
    `Fetal deaths >= 24 weeks` == "x" ~ "0",
    TRUE ~ `Fetal deaths >= 24 weeks`
  ),
  `Fetal deaths >= 28 weeks` = case_when(
    `Fetal deaths >= 28 weeks` == "x" ~ "0",
    TRUE ~ `Fetal deaths >= 28 weeks`
  ))
che2 <- read_xlsx("./dat/CHE_Data_template_Kniffka_TR_final.xlsx", sheet = 3, range = "A1:F13") %>% 
  mutate(Country = "Switzerland")

#uk

uk <- read_xlsx("./dat/Data_template_Kniffka UK.xlsx", range = "A1:G25")[-c(1:8),] %>% 
  mutate(Country = "UK")
uk2 <- read_xlsx("./dat/Data_template_Kniffka UK.xlsx", sheet = 2, range = "A1:F13")[-c(1,2,3),] %>% 
  mutate(Country = "UK")

# Bind data --------------------------------------------------------------------

#total trends

totals <- rbind(au2, cr2, cy2, est2, fi2, fr2, ic2, ir2, lt2, lv2, ne2, no2, uk2, dk2, es2, it2, slo2, lu2, po2, be2, ma2, sw2, cz2, che2, de2) %>% 
  rename(births_24 = 3, births_28 = 4, deaths_24 = 5, deaths_28 = 6) %>% 
  mutate(births_24 = as.numeric(births_24),
         births_28 = as.numeric(births_28),
         deaths_24 = as.numeric(deaths_24),
         deaths_28 = as.numeric(deaths_28)
         )
  totals <- totals %>% 
  mutate(exposure_24 = births_24 + deaths_24,
         exposure_28 = births_28 + deaths_28)

#detailed trends
  
details <- rbind(au, cr, cy, est, fi, fr, ic, ir, lt, lv, ne, no, uk, dk, es, it, slo, po, be, ma, sw, cz, che, de) %>% 
  rename(births_24 = 4, births_28 = 5, deaths_24 = 6, deaths_28 = 7) %>% 
mutate(births_24 = as.numeric(births_24),
       births_28 = as.numeric(births_28),
       deaths_24 = as.numeric(deaths_24),
       deaths_28 = as.numeric(deaths_28)
) %>% 
  mutate(exposure_24 = births_24 + deaths_24,
         exposure_28 = births_28 + deaths_28)

remove(au2, cr2, cy2, est2, fi2, fr2, ic2, ir2, lt2, lv2, ne2, no2, uk2, dk2, es2, it2, slo2, au, cr, 
       cy, est, fi, fr, ic, ir, lt, lv, ne, no, uk, dk, es, it, slo, lu2, po, po2, be, be2, ma, ma2, 
       sw, sw2, cz, cz2, che2, che, de, de2)

# distribute missings ----------------------------------------------------------

# age

age <- details %>% 
  filter(Characteristics != "Births from singleton pregnancies",
         Characteristics != "Births from multiple pregnancies")

totals <- totals %>% 
  mutate(Characteristics = "Total") %>% 
  select(Country, Year, Characteristics, births_24, births_28, deaths_24, deaths_28, exposure_24, exposure_28)

# deaths 24 weeks
age1 <- rbind(age, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, deaths_24) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Age 20 to 24`)) %>% 
  select(Country, Year, a_15 = 8, a_20 = 3, a_25 = 4, a_30 = 5, a_35 = 6, a_40 = 7, total = 9) %>% 
  mutate(missings = total - (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)) %>% 
  mutate(no_mis = (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)
         ) %>% 
  mutate(a_15 = a_15 + (missings * (a_15 / no_mis)),
         a_20 = a_20 + (missings * (a_20 / no_mis)),
         a_25 = a_25 + (missings * (a_25 / no_mis)),
         a_30 = a_30 + (missings * (a_30 / no_mis)),
         a_35 = a_35 + (missings * (a_35 / no_mis)),
         a_40 = a_40 + (missings * (a_40 / no_mis))
         ) %>% 
  select(Country, Year, a_15, a_20, a_25, a_30, a_35, a_40) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, deaths_24 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "a_15" ~ "Age below 20",
    Characteristics == "a_20" ~ "Age 20 to 24",
    Characteristics == "a_25" ~ "Age 25 to 29",
    Characteristics == "a_30" ~ "Age 30 to 34",
    Characteristics == "a_35" ~ "Age 35 to 39",
    Characteristics == "a_40" ~ "Age 40 and above"
  ))

# deaths 28 weeks
age2 <- rbind(age, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, deaths_28) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Age 20 to 24`)) %>% 
  select(Country, Year, a_15 = 8, a_20 = 3, a_25 = 4, a_30 = 5, a_35 = 6, a_40 = 7, total = 9) %>% 
  mutate(missings = total - (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)) %>% 
  mutate(no_mis = (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)
         ) %>% 
  mutate(a_15 = a_15 + (missings * (a_15 / no_mis)),
         a_20 = a_20 + (missings * (a_20 / no_mis)),
         a_25 = a_25 + (missings * (a_25 / no_mis)),
         a_30 = a_30 + (missings * (a_30 / no_mis)),
         a_35 = a_35 + (missings * (a_35 / no_mis)),
         a_40 = a_40 + (missings * (a_40 / no_mis))
  ) %>%  
  select(Country, Year, a_15, a_20, a_25, a_30, a_35, a_40) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, deaths_28 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "a_15" ~ "Age below 20",
    Characteristics == "a_20" ~ "Age 20 to 24",
    Characteristics == "a_25" ~ "Age 25 to 29",
    Characteristics == "a_30" ~ "Age 30 to 34",
    Characteristics == "a_35" ~ "Age 35 to 39",
    Characteristics == "a_40" ~ "Age 40 and above"
  ))


# birth 24 weeks
age3 <- rbind(age, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, births_24) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Age 20 to 24`)) %>% 
  select(Country, Year, a_15 = 8, a_20 = 3, a_25 = 4, a_30 = 5, a_35 = 6, a_40 = 7, total = 9) %>% 
  mutate(missings = total - (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)) %>% 
  mutate(no_mis = (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)
         ) %>% 
  mutate(a_15 = a_15 + (missings * (a_15 / no_mis)),
         a_20 = a_20 + (missings * (a_20 / no_mis)),
         a_25 = a_25 + (missings * (a_25 / no_mis)),
         a_30 = a_30 + (missings * (a_30 / no_mis)),
         a_35 = a_35 + (missings * (a_35 / no_mis)),
         a_40 = a_40 + (missings * (a_40 / no_mis))
  ) %>% 
  select(Country, Year, a_15, a_20, a_25, a_30, a_35, a_40) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, births_24 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "a_15" ~ "Age below 20",
    Characteristics == "a_20" ~ "Age 20 to 24",
    Characteristics == "a_25" ~ "Age 25 to 29",
    Characteristics == "a_30" ~ "Age 30 to 34",
    Characteristics == "a_35" ~ "Age 35 to 39",
    Characteristics == "a_40" ~ "Age 40 and above"
  ))

#  births 28 weeks
age4 <- rbind(age, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, births_28) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Age 20 to 24`)) %>% 
  select(Country, Year, a_15 = 8, a_20 = 3, a_25 = 4, a_30 = 5, a_35 = 6, a_40 = 7, total = 9) %>% 
  mutate(missings = total - (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)) %>% 
  mutate(no_mis = (a_15 + a_20 + a_25 + a_30 + a_35 + a_40)
         ) %>% 
  mutate(a_15 = a_15 + (missings * (a_15 / no_mis)),
         a_20 = a_20 + (missings * (a_20 / no_mis)),
         a_25 = a_25 + (missings * (a_25 / no_mis)),
         a_30 = a_30 + (missings * (a_30 / no_mis)),
         a_35 = a_35 + (missings * (a_35 / no_mis)),
         a_40 = a_40 + (missings * (a_40 / no_mis))
  ) %>% 
  select(Country, Year, a_15, a_20, a_25, a_30, a_35, a_40) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, births_28 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "a_15" ~ "Age below 20",
    Characteristics == "a_20" ~ "Age 20 to 24",
    Characteristics == "a_25" ~ "Age 25 to 29",
    Characteristics == "a_30" ~ "Age 30 to 34",
    Characteristics == "a_35" ~ "Age 35 to 39",
    Characteristics == "a_40" ~ "Age 40 and above"
  ))

age <- merge(age1, age2, all = T) %>% 
  merge(age3, all = T) %>% 
  merge(age4, all = T)

age <- age %>% 
  mutate(exposure_24 = deaths_24 + births_24,
         exposure_28 = deaths_28 + births_28) %>% 
  mutate(Year = case_when(
    Year == 2014 ~ 2015, # because of Poland
    TRUE ~ Year
  ))

remove(age1, age2, age3, age4)

# multiplicity


multi <- details %>% 
  filter(Characteristics == "Births from singleton pregnancies"|
         Characteristics == "Births from multiple pregnancies")

# deaths 24 weeks
multi1 <- rbind(multi, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, deaths_24) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Births from singleton pregnancies`)) %>% 
  select(Country, Year, multi = 3, single = 4, total = 5) %>% 
  mutate(missings = total - (multi + single)) %>% 
  mutate(no_mis = (multi + single)
         ) %>% 
  mutate(multi = multi + (missings * (multi / no_mis)),
         single = single + (missings * (single/ no_mis))
  ) %>% 
  select(Country, Year, multi, single) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, deaths_24 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "multi" ~ "Births from multiple pregnancies",
    Characteristics == "single" ~ "Births from singleton pregnancies"
  ))

# deaths 28 weeks
multi2 <- rbind(multi, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, deaths_28) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Births from singleton pregnancies`)) %>% 
  select(Country, Year, multi = 3, single = 4, total = 5) %>% 
  mutate(missings = total - (multi + single)) %>% 
  mutate(no_mis = (multi + single)
         ) %>% 
  mutate(multi = multi + (missings * (multi / no_mis)),
         single = single + (missings * (single/ no_mis))
  ) %>% 
  select(Country, Year, multi, single) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, deaths_28 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "multi" ~ "Births from multiple pregnancies",
    Characteristics == "single" ~ "Births from singleton pregnancies"
  ))


#  birth 24 weeks
multi3 <- rbind(multi, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, births_24) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Births from singleton pregnancies`)) %>% 
  select(Country, Year, multi = 3, single = 4, total = 5) %>% 
  mutate(missings = total - (multi + single)) %>% 
  mutate(no_mis = (multi + single)
         ) %>% 
  mutate(multi = multi + (missings * (multi / no_mis)),
         single = single + (missings * (single/ no_mis))
  ) %>% 
  select(Country, Year, multi, single) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, births_24 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "multi" ~ "Births from multiple pregnancies",
    Characteristics == "single" ~ "Births from singleton pregnancies"
  ))


#  births 28 weeks
multi4 <- rbind(multi, totals) %>% 
  arrange(Country, Year) %>% 
  select(Country, Year, Characteristics, births_28) %>% 
  dcast(Country + Year ~ Characteristics) %>% 
  filter(!is.na(`Births from singleton pregnancies`)) %>% 
  select(Country, Year, multi = 3, single = 4, total = 5) %>% 
  mutate(missings = total - (multi + single)) %>% 
  mutate(no_mis = (multi + single)
         ) %>% 
  mutate(multi = multi + (missings * (multi / no_mis)),
         single = single + (missings * (single/ no_mis))
  ) %>% 
  select(Country, Year, multi, single) %>% 
  melt(id.vars = c("Country", "Year")) %>% 
  rename(Characteristics = 3, births_28 = 4) %>% 
  mutate(Characteristics = case_when(
    Characteristics == "multi" ~ "Births from multiple pregnancies",
    Characteristics == "single" ~ "Births from singleton pregnancies"
  ))


multi <- merge(multi1, multi2, all = T) %>% 
  merge(multi3, all = T) %>% 
  merge(multi4, all = T)

multi <- multi %>% 
  mutate(exposure_24 = deaths_24 + births_24,
         exposure_28 = deaths_28 + births_28) %>% 
  mutate(Year = case_when(
    Year == 2014 ~ 2015, # because of Poland
    TRUE ~ Year
  ))

remove(multi1, multi2, multi3, multi4)

write_rds(totals, "./dat_out/total.rds") # the data total is in the format: Country, Year, Characteristics, birth_24, births_28, deaths_24, deaths_28, exposure_24, exposure_28
# and contains data from 25 countries on annual total numbers
write_rds(age, "./dat_out/by_maternal_age.rds") # the data total is in the format: Country, Year, Characteristics, deaths_24, deaths_28, birth_24, births_28, exposure_24, exposure_28
# and contains data from 24 countries on annual total numbers by maternal age
write_rds(multi, "./dat_out/by_multiplicity.rds")# the data total is in the format: Country, Year, Characteristics, deaths_24, deaths_28, birth_24, births_28, exposure_24, exposure_28
# and contains data from 24 countries on annual total numbers by multiplicity