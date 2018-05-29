### Stat Analysis


### Load EDA.R
source("EDA.R")


### Load additional library
library(tree)
library(leaps)
library(knitr)
library(kableExtra)
library(zoo)


#Table formatting function
table_nice <- function(x){
  kable(x, "html") %>%
    kable_styling(full_width = FALSE, position = "left")
}



## withOUT replacing the NA's


#-----------Training set
aqi_train

#Atmospheric Variables
atm_variables <- c("pressure", "wind", "hour", "temp", "dew", "humid")
aqi_train[atm_variables] #aqi with only atmospheric variables

# ----------Multicollinearity
# pollutant, pollutant_cat, aqi_pollutant are computed from one another
# therefore, they have very high correlation with one another; almost 1
aqi_train_na.rm <- aqi_train %>% 
  na.omit()
cor(aqi_train_na.rm$pm25, aqi_train_na.rm$aqi_pm25)  # 0.9784871
cor(aqi_train_na.rm$pm10, aqi_train_na.rm$aqi_pm10)  # 0.9831561
cor(aqi_train_na.rm$co, aqi_train_na.rm$aqi_co)      # 0.9857047 <- multicollinearity


# --------------Average values of atmospheric values 
aqi_train %>% 
  select(26:31) %>%   # selecting only atmospheric values
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = mean(value, na.rm = T))


# -----------------best subset selection

# Because the final AQI is determined by the maximum AQI score, perhaps there is no point in 
# doing best subset selection. However, I thought that this could also give us which pollutant
# is the most important, and if any of non-pollutant variables (atmospheric variables) could
# also give some information

regfit_full <- aqi_train %>% 
  regsubsets(max_aqi~aqi_pm10+aqi_pm25+aqi_no2+aqi_so2+aqi_o3+aqi_co+   #pollutants
               pressure+wind+hour+temp+dew+humid,                       #atm_var
             data = .,
             nvmax = 12)  #to return maximum 12 variables

reg_summary <- regfit_full %>%
  summary()

#into nice table format
reg_summary$outmat %>%
  data.frame() %>%
  rownames_to_column(var = "num_variables") %>%
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, font_size = 10)
#notice aqi_o3 is selected first
#notice no atmospheric variables are selected before all the pollutants are selected
#but humid is selected first among the atmospheric variables


#Take a look at the adj-R^2 only
reg_summary %>%
  .$adjr2 %>%
  {tibble(adjRsq = .)} %>%
  table_nice()

# Data for making plots
best_fit_data <- tibble(
  num_pred = 1:12,
  RSS = reg_summary$rss,
  R2 = reg_summary$rsq,
  Adj_R2 = reg_summary$adjr2,
  Cp = reg_summary$cp,
  BIC = reg_summary$bic
) %>%
  gather(key = "statistic", value = "value", 
         -num_pred)  #do NOT want to gather num_pred as well

# building plots
best_fit_data %>%
  filter(statistic != "R2") %>%
  ggplot(aes(x = num_pred, y = value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ statistic, ncol = 2, scales = "free") + 
  theme_bw()

# Judging model with lowest test error

# Identify min number of predictors (CP, BIC, RSS)
best_fit_data %>%
  group_by(statistic) %>%
  filter(value == min(value), 
         statistic %in% c("Cp", "BIC", "RSS"))  # want these to be small

# Identify min number of predictors (Adj_R2)
best_fit_data %>%
  group_by(statistic) %>%
  filter(value == max(value), statistic == "Adj_R2") 

# Adj_R2: 7 predictors
# Cp:     7 predictors
# BIC:    5 predictors
# RSS:    12 predictors <- not really useful though
# two measures uses 7 predictors, that is all the pollutants plus wind

#coefficient estimates associated with model with 7 predictors
coef(regfit_full, 7)
coef(regfit_full, 7) %>% names()





#Validation Set approach
set.seed(3)

test_mat <- model.matrix(max_aqi ~ ., data = na.omit(aqi_test))

test_mat[, names(coef(regfit_full, 7))]%*%coef(regfit_full, 7)

val_errors <- rep(NA, 12)

for (i in 1:12) {
  coefi <- coef(regfit_full, id = i)
  pred <-  test_mat[, names(coefi)] %*% coefi
  val_errors[i] <- mean((aqi_test$max_aqi - pred[,1])^2)
}

val_errors









#----------------Forward Selection

regfit_fwd <- aqi_train %>% 
  regsubsets(max_aqi~aqi_pm10+aqi_pm25+aqi_no2+aqi_so2+aqi_o3+aqi_co+   #pollutants
               pressure+wind+hour+temp+dew+humid,                       #atm_var
             data = .,
             nvmax = 12,
             method = "forward")    #forward selection

regfit_fwd %>%
  summary() %>%
  .$outmat %>%
  data.frame() %>%
  rownames_to_column(var = "num_variables") %>%
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, font_size = 10)
#Again all the pollutants first, and then humid


#----------------Backward Selection
regfit_bwd <- aqi_train %>% 
  regsubsets(max_aqi~aqi_pm10+aqi_pm25+aqi_no2+aqi_so2+aqi_o3+aqi_co+   #pollutants
               pressure+wind+hour+temp+dew+humid,                       #atm_var
             data = .,
             nvmax = 12,
             method = "backward")    #backward selection

regfit_bwd %>%
  summary() %>%
  .$outmat %>%
  data.frame() %>%
  rownames_to_column(var = "num_variables") %>%
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, font_size = 10)
#Again all the pollutants first, and then humid



#-----------------Classification tree
#By location and atmospheric variables
set.seed(3)
tree_aqi <- tree(max_pollut ~ longitude+latitude+         #location
                   pressure+wind+hour+temp+dew+humid,     #atmospheric variables
                 data = aqi_train)
summary(tree_aqi)

#plotting the tree
plot(tree_aqi)
text(tree_aqi, pretty=0)
#Classification error rate
pred_tree <- predict(tree_aqi, newdata = aqi_test, 
                     type = "class") #type="class" instructs R to return the actual class prediction
table(pred_tree, aqi_test$max_pollut)
mean(pred_tree == aqi_test$max_pollut)  # 0.5450644

#Cross validation
set.seed(3) #I wrote in EDA, that I will stick to using seed 3
tree_cv <- cv.tree(tree_aqi, FUN = prune.misclass)
tree_cv
plot(tree_cv$size, tree_cv$dev, type = "b")   

tree_aqi_pruned <- prune.misclass(tree_aqi, best = 10) #10 leaves
plot(tree_aqi_pruned)
text(tree_aqi_pruned, pretty = 0)

pred_tree_pruned <- predict(tree_aqi_pruned, newdata = aqi_test, 
                     type = "class") 
table(pred_tree_pruned, aqi_test$max_pollut)
mean(pred_tree_pruned == aqi_test$max_pollut)  #0.5450644


#----------------------average values of AQI scores
aqi_train %>% 
  select(contains("aqi")) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = mean(value, na.rm = T))


#---------------Principal Component Analysis

pca1 <- aqi_train_na.rm %>%  # not using any NA's
  select(latitude, longitude,                          #location
         max_aqi,                                      #most important pollutant info
         pressure, wind, hour, temp, dew, humid) %>%   #atmospheric variables
  prcomp(scale = T)

pca1$rotation 
biplot(pca1, scale = 0)  #not really helpful...
pca1_var <- pca1$sdev^2 #variance explained by each principal component
pve1 <- pca1_var/sum(pca1_var) #variance explained by all four principal components



#### REPLACE the NA's with column means and repeat above

# column means of numerical variables
aqi_train_numeric <- aqi_train %>% 
  select(-c(1:4), -contains("cat"), -max_pollut) 
aqi_train_numeric %>% 
  colMeans( na.rm = T)

foo <- na.aggregate(aqi_train_numeric)

#replace NA's by column means
aqi_train1 <- aqi_train %>% 
  mutate(pm25     = foo$pm25,
         aqi_pm25 = foo$aqi_pm25,
         
         pm10     = foo$pm10,
         aqi_pm10 = foo$aqi_pm10,
         
         o3       = foo$o3,
         aqi_o3   = foo$aqi_o3,
         
         no2      = foo$no2,
         aqi_no2  = foo$aqi_no2,
         
         so2      = foo$so2,
         aqi_so2  = foo$aqi_so2,
         
         co       = foo$co,
         aqi_co   = foo$aqi_co,
         
         max_aqi  = foo$max_aqi,
         
         pressure = foo$pressure,
         wind     = foo$wind,
         hour     = foo$hour,
         temp     = foo$temp,
         dew      = foo$dew,
         humid    = foo$humid)

#Ask




