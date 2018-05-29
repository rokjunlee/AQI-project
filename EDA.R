### EDA


### Load Library
library(tidyverse)
library(ggrepel)
library(Hmisc)
library(janitor)  # useful function: clean_names()
library(forcats)
library(standardize)


### Load Data
aqi <- read_csv("data/aqi.csv")
#currently a lot of them are in character
#chinesename column only contains NA


### Altering my data object, aqi
aqi <- aqi %>%
  select(-chinesename)  # chinesename is just a column of NA's so it is taken out

aqi <- aqi %>%          # converting to dbl from character
  mutate(pm25 = as.numeric(pm25),
         pm10 = as.numeric(pm10),
         o3   = as.numeric(o3),
         no2  = as.numeric(no2),
         so2  = as.numeric(so2),
         co   = as.numeric(co),           # changing to double!
         temp = as.numeric(temperature),
         dew  = as.numeric(dewpoint),
         humid= as.numeric(humidity),
         wind = as.numeric(wind))

aqi <- aqi %>% 
  select(-c(temperature, dewpoint, humidity)) 
# same information is contained in tem, dew, humid!
# So the longer columnnames are dropped


# Currently est_time is very hard to read
    # Year month date time are all CONCATENATED  <- bad
aqi <- aqi %>% 
  separate(est_time, into = c("year", "date"), sep = 4) %>% 
  separate(date, into = c("month", "day"), sep = 2) %>% 
  separate(day, into = c("date", "time"), sep = 2) %>% 
  separate(time, into = c("hour", "zero"), sep = 2) %>% 
  select(-zero) %>% 
  mutate(year  = as.numeric(year),
         month = as.numeric(month),
         date  = as.numeric(date),    #converting them to double!
         hour  = as.numeric(hour))

table(aqi$year, aqi$month)   # They are ALL March 2015 
table(aqi$month, aqi$date)            # ALL March 18
#So they do NOT add meaningful information to the data except that these data were collected on March 18, 2015
aqi <- aqi %>% 
  select(-c(date, month, year))  # DE-selected year, month, date columns that do NOT add any useful information

aqi <- aqi %>% clean_names()  #no names changed <- nothing changed but it is a useful function


###---------------- Labels

# adding units to the pollutants <- from Hmisc package
# Knowing the UNITS are important later when I convert these concentrations to AQI scores!
label(aqi$pm25) <- "μg/m^3"
label(aqi$pm10) <- "μg/m^3"
label(aqi$co) <- "ppm"
label(aqi$o3) <- "pphm"
label(aqi$so2) <- "pphm"
label(aqi$no2) <- "pphm"



### --------------Quick facts about my data: aqi

aqi %>% glimpse() #showing me all the variables and variable types
dim(aqi)  #1590 observations; 16 variables

table(aqi$hour)  # measured during 4 different times
length(unique(aqi$stationname)) # 1543 distinct places out of 1590 obs

aqi$stationname %>% 
  str_count("Beijing") %>% 
  sum()  #only 2 places that contain the word, "Beijing"; I expected more

aqi$stationname %>% 
  str_count("Shanghai") %>% 
  sum() #only 3 places that contain the word, "Shanghai"; I expected more


### ---------------Missing Values

sum(is.na(aqi))  # In total there are 2963 missing values
# I do not think this number is too concerning, considering that there are 
# 1590 observations and 16 variables

aqi %>% 
  select(pm25, pm10, o3, no2, so2, co) %>% is.na() %>% 
  sum()          # only 323 out of 2963 missing values are from the pollutants' columns

# The concentrations of the pollutants provide the most important information;
# Among SIX pollutants there are only 323 NA's, so I do not think this is too concerning


###------------- Facts about the pollutants
                 #grouping by TIME

#pollutants by time
aqi %>% 
  group_by(hour) %>% 
  summarise(
    avg_pm25 = mean(pm25, na.rm = TRUE),
    avg_pm10 = mean(pm10, na.rm = TRUE),
    avg_o3 = mean(o3, na.rm = TRUE),
    avg_no2 = mean(no2, na.rm = TRUE),
    avg_so2 = mean(so2, na.rm = TRUE),
    avg_co = mean(co, na.rm = TRUE)
    )
#but since the pollutants are measured in different units, it would be much sensible to
#compare their mean concentrations after scaling them like the following:
aqi %>% 
  group_by(hour) %>% 
  summarise(pm25    = mean(pm25, na.rm = TRUE)/sd(pm25, na.rm = TRUE), #scaled mean
            pm10    = mean(pm10, na.rm = TRUE)/sd(pm10, na.rm = TRUE),
            o3      = mean(o3,   na.rm = TRUE)/sd(o3,   na.rm = TRUE),
            no2     = mean(no2,  na.rm = TRUE)/sd(no2,  na.rm = TRUE),
            so2     = mean(so2,  na.rm = TRUE)/sd(so2,  na.rm = TRUE),
            co      = mean(co,   na.rm = TRUE)/sd(co,   na.rm = TRUE)) %>%
  gather(pm25, pm10, o3, no2, so2, co, key = pollutants, value = scaled_mean) %>%
  ggplot( aes( x = hour ))+
  geom_bar( aes(y = scaled_mean, fill = pollutants), stat = "identity", position = "dodge")

#As you can see, after scaling, pm25 has the greatest concentration.

#Or another way (very similar to above above)
aqi %>% 
  mutate(
    sc_pm25 = pm25 / sd(pm25, na.rm = T),
    sc_pm10 = pm10 / sd(pm10, na.rm = T),
    sc_o3   = o3   / sd(o3,   na.rm = T),
    sc_no2  = no2  / sd(no2,  na.rm = T),
    sc_so2  = so2  / sd(so2,  na.rm = T),
    sc_co   = co   / sd(co,   na.rm = T)
  ) %>% 
  group_by(hour) %>% 
  summarise( pm25    = mean(sc_pm25, na.rm = TRUE), 
             pm10    = mean(sc_pm10, na.rm = TRUE),
             o3      = mean(sc_o3,   na.rm = TRUE),
             no2     = mean(sc_no2,  na.rm = TRUE),
             so2     = mean(sc_so2,  na.rm = TRUE),
             co      = mean(sc_co,   na.rm = TRUE)
             )%>%
  gather(pm25, pm10, o3, no2, so2, co, key = pollutants, value = scaled_mean) %>%
  ggplot( aes( x = hour ))+
  geom_bar( aes(y = scaled_mean, fill = pollutants), stat = "identity", position = "dodge")

#Judging from both bar plots above, one may think pm25 is the most problematic pollutant
#However, AQI scores (later) will tell us that it is actually another pollutant that is
#responsible for raising AQI score. The higher the AQI, the worse.

#Also the units given in the data is not the same as units used for calculating the AQI
#score. Therefore, the above plots are misleading.


#--------------------Detailed facts about pollutants
                    # grouping by pollutants

poll_stat <- aqi %>% 
  select(pm25, pm10, o3, no2, so2, co) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(min   = min(value, na.rm = TRUE),
            max   = max(value, na.rm = TRUE),
            rng   = max - min,
            mean  = mean(value, na.rm = TRUE),    #center
            median = median(value, na.rm = TRUE),
            sd    = sd(value, na.rm = TRUE),      #spread
            iqr   = IQR(value, na.rm = TRUE)) 
poll_stat

#Raw Concentration
poll_stat %>% 
  mutate(sdup = mean + 1.96*sd,      #95% CI
         sddw = mean - 1.96*sd) %>% 
  ggplot(aes(key)) +
  geom_point(aes(y = mean, color = key)) +
  geom_point(aes(y = sdup), pch = 2) +    #one 1.96sd up from mean
  geom_point(aes(y = sddw), pch = 6) +    #one 1.96sd down from mean
  labs(title = "Raw Average Concentrations",
       x = "Key Pollutants",
       y = "Raw Concentration",
       caption = paste("since this shows *raw* concentration, each pollutant's 
                       concentration cannot be compared to one another")) +
  theme_bw()

#Scaled Concentration
poll_stat_helper <- poll_stat %>% 
  mutate(scaled = mean / sd) %>% 
  select(key, scaled)


poll_stat %>% 
  mutate(scaled = mean / sd,
         sdup = scaled + 1.96,     #95% CI
         sddw = scaled - 1.96) %>% 
  ggplot(aes(key)) +
  geom_point(aes(y = scaled, color = key)) +
  geom_point(aes(y = sdup), pch = 2) +    # upper bound for 95% CI
  geom_point(aes(y = sddw), pch = 6) +    # lower bound for 95% CI
  labs(title = "Scaled Average Concentrations",
       x = "Key Pollutants",
       y = "Scaled Concentration",
       caption = paste("since this shows *scaled* concentration, each 
                       pollutant's concentration can be compared to one another")) +
  geom_label_repel(aes(label = key, y = scaled, color = key), 
                   data = poll_stat_helper,
                   segment.color = NA,
                   size = 2.5) +
  theme_bw() +
  theme(legend.position = "none") 

#again pm25 'seems' like the most problematic pollutant

#AGAIN, the units given in the data is not the same as units used for calculating the AQI
#score. Therefore, the above plots are misleading.

### -------------- Should I be worried about Multicorrelinearity? 

#Correlations among pollutants
poll_cor <- aqi %>% 
  select(pm25, pm10, o3, no2, so2, co) %>% 
  na.omit() %>%    #discard NA's for this purpose for now
  cor()
poll_cor
#Below, only the correlation between pm25 and pm10 exceeds 0.5.
#Presumably because they originate from the same source (?)
poll_cor[poll_cor>0.5 & poll_cor != 1]   #0.7294517,
#Overall, multicollinearity does not seem to be a major problem


###------------- Ok... so let's do AQI score   This is the MAIN PART

#First I need to convert some of the pollutants' concentration:
  # unit for o3, so2, and no2 needs to be changed to use the formula given in the pdf
  # o3 is in pphm: needs to be changed to ppm
  # so2 is in pphm: needs to be changed to ppb
  # no2 is in pphm: needs to be changed to ppb
aqi <- aqi %>% 
  mutate(
    o3 = o3/100,
    so2 = 10*so2,       # fixing units so that I can use the formula used by EPA
    no2 = 10*no2)
label(aqi$o3) <- "ppm"
label(aqi$so2) <- "ppb"     #labelling CORRECT units
label(aqi$no2) <- "ppb"

#According to the pdf by the EPA, some rounding has to be done
#However, all of the pollutants in aqi are already rounded. 
#For example, pm25 has to be rounded to 1 decimal place, but it is already in integer

#label that I ingeniously/creatively created
lab <- c("very good", "good", "moderate", "bad", "very bad", "dangerous", "toxic")


#   BREAKPOINTS for each pollutant!

#for o3
range(aqi$o3, na.rm = T)  # 0.01, 2.90
o3_bp <- c(0, 0.054, 0.124, 0.164, 0.204, 0.404, 0.504, 0.604)

#for so2
range(aqi$so2, na.rm = T)   # 0, 1190
so2_bp <- c(0, 35, 75, 185, 304, 604, 804, 1004)

#for no2
range(aqi$no2, na.rm = T)   # 0, 1030
no2_bp <- c(0, 53, 100, 360, 649, 1249, 1649, 2049)

#for co
range(aqi$co, na.rm = T)    # 0, 67
co_bp <- c(0, 4.4, 9.4, 12.4, 15.4, 30.4, 40.4, 50.4)

#for pm25
range(aqi$pm25, na.rm = T)  # 1, 544
pm25_bp <- c(0, 12, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4)

#for pm10
range(aqi$pm10, na.rm = T)  # 0, 999
pm10_bp <- c(0, 54, 154, 254, 354, 424, 504, 604)


#Below, following the table given by the EPA, EACH concentration of EACH pollutant is categorized appropriately!
aqi <- aqi %>% 
  mutate(
    pm25_cat = cut(pm25, pm25_bp, labels = lab),
    pm10_cat = cut(pm10, pm10_bp, labels = lab),
    o3_cat   = cut(o3, o3_bp, labels = lab),
    no2_cat  = cut(no2, no2_bp, labels = lab),   #creating columns that contain appropriate category
    so2_cat  = cut(so2, so2_bp, labels = lab),
    co_cat   = cut(co, co_bp, labels = lab)
    )


#REorder columns -  I want to see pollutants together
aqi <- aqi %>%  
  select(locationid, stationname, latitude, longitude, 
         pm25, pm25_cat, pm10, pm10_cat, o3, o3_cat, no2, no2_cat, so2, so2_cat, co, co_cat,
         everything())

# AQI score BREAKPOINTS
aqi_bp <- c(0, 50, 51, 100, 101, 150, 151, 200, 201, 300,
            301, 400, 401, 500)

# Below are functions that I used to convert concentrations to AQI scores!
# Same formula for each pollutant, but subtle unit difference depending on which pollutant!

##### For pm25
pm25_aqi_convert <- function( abp, polbp ){
  temp <- numeric(dim(aqi)[1])
  for (i in 1:dim(aqi)[1]) {
    if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "toxic" ){
      temp[i] <- (abp[14]-abp[13])/(polbp[8]-(polbp[7]+0.1))*(aqi$pm25[i]-(polbp[7]+0.1))+abp[13]
    } else if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "dangerous" ){
      temp[i] <- (abp[12]-abp[11])/(polbp[7]-(polbp[6]+0.1))*(aqi$pm25[i]-(polbp[6]+0.1))+abp[11]
    } else if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "very bad" ){
      temp[i] <- (abp[10]-abp[9])/(polbp[6]-(polbp[5]+0.1))*(aqi$pm25[i]-(polbp[5]+0.1))+abp[9]
    } else if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "bad" ){
      temp[i] <- (abp[8]-abp[7])/(polbp[5]-(polbp[4]+0.1))*(aqi$pm25[i]-(polbp[4]+0.1))+abp[7]
    } else if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "moderate" ){
      temp[i] <- (abp[6]-abp[5])/(polbp[4]-(polbp[3]+0.1))*(aqi$pm25[i]-(polbp[3]+0.1))+abp[5]
    } else if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "good" ){
      temp[i] <- (abp[4]-abp[3])/(polbp[3]-(polbp[2]+0.1))*(aqi$pm25[i]-(polbp[2]+0.1))+abp[3]
    } else if ( !is.na(aqi$pm25_cat[i]) & aqi$pm25_cat[i] == "very good" ){
      temp[i] <- (abp[2]-abp[1])/(polbp[2]-(polbp[1]+0.1))*(aqi$pm25[i]-(polbp[1]+0.1))+abp[1]
    }
  }
  return(temp)
}
aqi <- aqi %>% 
  mutate( aqi_pm25 = pm25_aqi_convert(aqi_bp, pm25_bp) ) 


##### For pm10
pm10_aqi_convert <- function( abp, polbp ){
  temp <- numeric(dim(aqi)[1])
  for (i in 1:dim(aqi)[1]) {
    if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "toxic" ){
      temp[i] <- (abp[14]-abp[13])/(polbp[8]-(polbp[7]+1))*(aqi$pm10[i]-(polbp[7]+1))+abp[13]
    } else if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "dangerous" ){
      temp[i] <- (abp[12]-abp[11])/(polbp[7]-(polbp[6]+1))*(aqi$pm10[i]-(polbp[6]+1))+abp[11]
    } else if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "very bad" ){
      temp[i] <- (abp[10]-abp[9])/(polbp[6]-(polbp[5]+1))*(aqi$pm10[i]-(polbp[5]+1))+abp[9]
    } else if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "bad" ){
      temp[i] <- (abp[8]-abp[7])/(polbp[5]-(polbp[4]+1))*(aqi$pm10[i]-(polbp[4]+1))+abp[7]
    } else if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "moderate" ){
      temp[i] <- (abp[6]-abp[5])/(polbp[4]-(polbp[3]+1))*(aqi$pm10[i]-(polbp[3]+1))+abp[5]
    } else if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "good" ){
      temp[i] <- (abp[4]-abp[3])/(polbp[3]-(polbp[2]+1))*(aqi$pm10[i]-(polbp[2]+1))+abp[3]
    } else if ( !is.na(aqi$pm10_cat[i]) & aqi$pm10_cat[i] == "very good" ){
      temp[i] <- (abp[2]-abp[1])/(polbp[2]-(polbp[1]+1))*(aqi$pm10[i]-(polbp[1]+1))+abp[1]
    }
  }
  return(temp)
}
aqi <- aqi %>% 
  mutate( aqi_pm10 = pm10_aqi_convert(aqi_bp, pm10_bp) ) 


### For o3
o3_aqi_convert <- function( abp, polbp ){
  temp <- numeric(dim(aqi)[1])
  for (i in 1:dim(aqi)[1]) {
    if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "toxic" ){
      temp[i] <- (abp[14]-abp[13])/(polbp[8]-(polbp[7]+0.001))*(aqi$o3[i]-(polbp[7]+0.001))+abp[13]
    } else if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "dangerous" ){
      temp[i] <- (abp[12]-abp[11])/(polbp[7]-(polbp[6]+0.001))*(aqi$o3[i]-(polbp[6]+0.001))+abp[11]
    } else if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "very bad" ){
      temp[i] <- (abp[10]-abp[9])/(polbp[6]-(polbp[5]+0.001))*(aqi$o3[i]-(polbp[5]+0.001))+abp[9]
    } else if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "bad" ){
      temp[i] <- (abp[8]-abp[7])/(polbp[5]-(polbp[4]+0.001))*(aqi$o3[i]-(polbp[4]+0.001))+abp[7]
    } else if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "moderate" ){
      temp[i] <- (abp[6]-abp[5])/(polbp[4]-(polbp[3]+0.001))*(aqi$o3[i]-(polbp[3]+0.001))+abp[5]
    } else if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "good" ){
      temp[i] <- (abp[4]-abp[3])/(polbp[3]-(polbp[2]+0.001))*(aqi$o3[i]-(polbp[2]+0.001))+abp[3]
    } else if ( !is.na(aqi$o3_cat[i]) & aqi$o3_cat[i] == "very good" ){
      temp[i] <- (abp[2]-abp[1])/(polbp[2]-(polbp[1]+0.001))*(aqi$o3[i]-(polbp[1]+0.001))+abp[1]
    }
  }
  return(temp)
}
aqi <- aqi %>% 
  mutate( aqi_o3 = o3_aqi_convert(aqi_bp, o3_bp))

### For so2
so2_aqi_convert <- function( abp, polbp ){
  temp <- numeric(dim(aqi)[1])
  for (i in 1:dim(aqi)[1]) {
    if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "toxic" ){
      temp[i] <- (abp[14]-abp[13])/(polbp[8]-(polbp[7]+1))*(aqi$so2[i]-(polbp[7]+1))+abp[13]
    } else if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "dangerous" ){
      temp[i] <- (abp[12]-abp[11])/(polbp[7]-(polbp[6]+1))*(aqi$so2[i]-(polbp[6]+1))+abp[11]
    } else if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "very bad" ){
      temp[i] <- (abp[10]-abp[9])/(polbp[6]-(polbp[5]+1))*(aqi$so2[i]-(polbp[5]+1))+abp[9]
    } else if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "bad" ){
      temp[i] <- (abp[8]-abp[7])/(polbp[5]-(polbp[4]+1))*(aqi$so2[i]-(polbp[4]+1))+abp[7]
    } else if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "moderate" ){
      temp[i] <- (abp[6]-abp[5])/(polbp[4]-(polbp[3]+1))*(aqi$so2[i]-(polbp[3]+1))+abp[5]
    } else if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "good" ){
      temp[i] <- (abp[4]-abp[3])/(polbp[3]-(polbp[2]+1))*(aqi$so2[i]-(polbp[2]+1))+abp[3]
    } else if ( !is.na(aqi$so2_cat[i]) & aqi$so2_cat[i] == "very good" ){
      temp[i] <- (abp[2]-abp[1])/(polbp[2]-(polbp[1]+1))*(aqi$so2[i]-(polbp[1]+1))+abp[1]
    }
  }
  return(temp)
}
aqi <- aqi %>% 
  mutate( aqi_so2 = so2_aqi_convert(aqi_bp, so2_bp))


### For no2
no2_aqi_convert <- function( abp, polbp ){
  temp <- numeric(dim(aqi)[1])
  for (i in 1:dim(aqi)[1]) {
    if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "toxic" ){
      temp[i] <- (abp[14]-abp[13])/(polbp[8]-(polbp[7]+1))*(aqi$no2[i]-(polbp[7]+1))+abp[13]
    } else if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "dangerous" ){
      temp[i] <- (abp[12]-abp[11])/(polbp[7]-(polbp[6]+1))*(aqi$no2[i]-(polbp[6]+1))+abp[11]
    } else if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "very bad" ){
      temp[i] <- (abp[10]-abp[9])/(polbp[6]-(polbp[5]+1))*(aqi$no2[i]-(polbp[5]+1))+abp[9]
    } else if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "bad" ){
      temp[i] <- (abp[8]-abp[7])/(polbp[5]-(polbp[4]+1))*(aqi$no2[i]-(polbp[4]+1))+abp[7]
    } else if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "moderate" ){
      temp[i] <- (abp[6]-abp[5])/(polbp[4]-(polbp[3]+1))*(aqi$no2[i]-(polbp[3]+1))+abp[5]
    } else if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "good" ){
      temp[i] <- (abp[4]-abp[3])/(polbp[3]-(polbp[2]+1))*(aqi$no2[i]-(polbp[2]+1))+abp[3]
    } else if ( !is.na(aqi$no2_cat[i]) & aqi$no2_cat[i] == "very good" ){
      temp[i] <- (abp[2]-abp[1])/(polbp[2]-(polbp[1]+1))*(aqi$no2[i]-(polbp[1]+1))+abp[1]
    }
  }
  return(temp)
}
aqi <- aqi %>% 
  mutate( aqi_no2 = no2_aqi_convert(aqi_bp, no2_bp))

### For co
co_aqi_convert <- function( abp, polbp ){
  temp <- numeric(dim(aqi)[1])
  for (i in 1:dim(aqi)[1]) {
    if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "toxic" ){
      temp[i] <- (abp[14]-abp[13])/(polbp[8]-(polbp[7]+0.1))*(aqi$co[i]-(polbp[7]+0.1))+abp[13]
    } else if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "dangerous" ){
      temp[i] <- (abp[12]-abp[11])/(polbp[7]-(polbp[6]+0.1))*(aqi$co[i]-(polbp[6]+0.1))+abp[11]
    } else if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "very bad" ){
      temp[i] <- (abp[10]-abp[9])/(polbp[6]-(polbp[5]+0.1))*(aqi$co[i]-(polbp[5]+0.1))+abp[9]
    } else if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "bad" ){
      temp[i] <- (abp[8]-abp[7])/(polbp[5]-(polbp[4]+0.1))*(aqi$co[i]-(polbp[4]+0.1))+abp[7]
    } else if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "moderate" ){
      temp[i] <- (abp[6]-abp[5])/(polbp[4]-(polbp[3]+0.1))*(aqi$co[i]-(polbp[3]+0.1))+abp[5]
    } else if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "good" ){
      temp[i] <- (abp[4]-abp[3])/(polbp[3]-(polbp[2]+0.1))*(aqi$co[i]-(polbp[2]+0.1))+abp[3]
    } else if ( !is.na(aqi$co_cat[i]) & aqi$co_cat[i] == "very good" ){
      temp[i] <- (abp[2]-abp[1])/(polbp[2]-(polbp[1]+0.1))*(aqi$co[i]-(polbp[1]+0.1))+abp[1]
    }
  }
  return(temp)
}
aqi <- aqi %>% 
  mutate( aqi_co = co_aqi_convert(aqi_bp, co_bp) ) 

#ALL AQI SCORES HAVE BEEN ADDED NOW!!!! HOORAY



#Reorder columns to have same pollutants together
aqi <- aqi %>% 
  select(locationid, stationname, latitude, longitude, 
         contains("pm25"),
         contains("pm10"),
         contains("o3"),
         contains("no2"),       #grouping the same pollutants together
         contains("so2"),
         contains("co"),
         everything())  #looking so good!!! wow!



# Creating a column with highest AQI score for each row

#data object only containing the pollutants' AQI scores below
aqi_score <- aqi[c("aqi_pm25", "aqi_pm10", "aqi_o3", "aqi_so2", "aqi_no2", "aqi_co")]

colnames(aqi_score)  #giving us all the column names <- just to see what this does

#Now I need to pick the MAXIMUM AQI score on each ROW
#The MAXimum score on each row is "the" AQI score for that location and time
aqi_score <- aqi_score %>% 
  mutate(max_aqi = apply(aqi_score, 1, max)) %>%   # showing the maximum AQI for each ROW
  mutate(
    max_pollut = colnames(aqi_score)[max.col(aqi_score)] ) # showing which pollutant that maximum AQI is coming from
aqi_score   #notice two columns have been added on the right

max_aqi_score <- aqi_score[, (dim(aqi_score)[2]-1):dim(aqi_score)[2] ]
max_aqi_score  # just extracted the two right columns from aqi_score

# Add max_aqi_score to the beast "aqi"
aqi <- cbind(aqi, max_aqi_score) %>% as_tibble() #to make sure it is tibble
aqi <- aqi %>% 
  mutate(max_pollut = factor(max_pollut))  #factoring max pollutants

# just rewriting AQI BREAKPOINTS
aqi_bp1 <- c(0, 50, 100, 150, 200, 300, 400, 500)

# Now create a column that categorizes the MAXimum AQI scores!
aqi <- aqi %>% 
  mutate(final_cat = cut(max_aqi, aqi_bp1, labels = lab))  
#final category is the label that shows up as the final category on websites!

aqi <- aqi %>% 
  select(locationid, stationname, latitude, longitude, 
         contains("pm25"),
         contains("pm10"),
         contains("o3"),
         contains("no2"),       #grouping same pollutants together
         contains("so2"),
         contains("co"),
         contains("max"),
         final_cat,
         everything())

dim(aqi)  # 1590   31
 
# Lastly, some EDA using this most recent aqi data object!

aqi %>% 
  group_by(max_pollut) %>% 
  summarise(mean = mean(max_aqi),
            sd   = sd(max_aqi),
            max  = max(max_aqi),
            min  = min(max_aqi),
            freq = n()) %>% 
  mutate(prop = freq/sum(freq)) 
#o3's AQI score was used for final AQI score most of the time, followed by pm25


aqi %>% 
  ggplot(aes(reorder(max_pollut, max_aqi, median), max_aqi)) +
  geom_point(aes(col = final_cat)) + 
  geom_boxplot(size = 0.05, alpha = 0.3)+
  theme_bw()+
  theme(legend.position = "none") 

aqi %>% 
  group_by(final_cat) %>% 
  summarise(freq = n()) %>% 
  mutate(prop = freq / sum(freq))
summary(aqi$final_cat)
#not a single "very good" category

aqi %>% 
  ggplot(aes(final_cat))+
  geom_bar(aes(fill = max_pollut))+
  geom_text(stat = 'count' , aes(label = ..count..), vjust=0, hjust=0.7, col = "purple") +
  coord_flip() +
  theme_bw()

aqi %>% 
  ggplot(aes(final_cat))+
  geom_bar(aes(fill = max_pollut), , position = "dodge")+
  ylim(c(0, 600)) +
  # geom_text(stat = 'count' , aes(label = ..count..), 
  #           vjust=0, hjust=0.7, col = "purple") +
  coord_flip() +
  theme_bw()

#overall o3 seems to be the most problematic pollutant 
      # this contradicts our first few plots where we concluded pm25 to be most problematic one.
      #BUT for "bad" and "moderate" categories, pm25 seems to be the major pollutant



##Now that I have created the response variables (AQI scores) for each pollutant, I will 
##divide aqi dataset into two: training set and testing set. 

set.seed(3)
aqi_train <- aqi %>% 
  sample_frac(0.85)    #85% <- I will use 85% of the data as the training set
aqi_test <- aqi %>% 
  setdiff(aqi_train)

dim(aqi)       #1590   31
dim(aqi_train) #1352   31
dim(aqi_test)  #233    31

