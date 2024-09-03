#load necessary libraries
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(rgl)
library(sf)
library(tmap)

#load the data into csv
deprivation <- read.csv('Indices_of_Multiple_Deprivation_(IMD)_2019.csv')
View(deprivation)

#View summary statistics for each domain & check for missing data
summary(deprivation)


#Load the data for 2004- 2015 into csv
imd_04 <- read.csv('IMD 2004-2015/imd2004eng.csv')
imd_07 <- read.csv('IMD 2004-2015/imd2007eng.csv')
imd_10 <- read.csv('IMD 2004-2015/imd2010eng.csv')
imd_15 <- read.csv('IMD 2004-2015/imd2015eng.csv')


#renaming needed columns for uniformity with the 2019 dataset
imd_04 <- imd_04 %>% rename(IMDScore04= IMD.SCORE,
                                   HDDScore04= HEALTH.DEPRIVATION.AND.DISABILITY.SCORE,
                            lsoa11cd= SOA)

imd_07 <- imd_07 %>% rename(IMDScore07= IMD.SCORE,
                                   HDDScore07= HEALTH.DEPRIVATION.AND.DISABILITY.SCORE,
                            lsoa11cd= LSOA)

imd_10 <- imd_10 %>% rename(IMDScore10= imd_score,
                                   HDDScore10= healthdd_score,
                            lsoa11cd= lsoacode)
imd_15 <- imd_15 %>% rename(IMDScore15= Index.of.Multiple.Deprivation..IMD..Score,
                            HDDScore15= Health.Deprivation.and.Disability.Score,
                            lsoa11cd= LSOA.code..2011.)
imd_15 <- imd_15 %>% rename(TotPop15= Total.population..mid.2012..excluding.prisoners.,)

#joining selected columns to the 2019 dataset
imd_04_selected <- imd_04 %>% select(lsoa11cd, IMDScore04, HDDScore04, HDDDec04, HDDRank04)
view(imd_04_selected)
imd_07_selected <- imd_07 %>% select(lsoa11cd, IMDScore07, HDDScore07, HDDDec07, HDDRank07)
imd_10_selected <- imd_10 %>% select(lsoa11cd, IMDScore10, HDDScore10, HDDDec10, HDDRank10)
imd_15_selected <- imd_15 %>% select(lsoa11cd, IMDScore15, HDDScore15, HDDDec15,HDDRank15, TotPop15)

imd_04 <- imd_04 %>% rename(hdd_rank= RANK.OF.HEALTH.DEPRIVATION.AND.DISABILITY.SCORE..where.1.is.most.deprived.)
imd_07 <- imd_07 %>% rename(hdd_rank= RANK.OF.HEALTH.DEPRIVATION.AND.DISABILITY.SCORE..where.1.is.most.deprived.)
imd_10 <- imd_10 %>% rename(hdd_rank= healthdd_rank)
imd_15 <- imd_15 %>% rename(hdd_rank= Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.)
imd_15 <- imd_15 %>% rename(HDDDec= Health.Deprivation.and.Disability.Decile..where.1.is.most.deprived.10..of.LSOAs.)
rm(top_2004, top_2007, top_2010, top_2015)

imd_04 <- imd_04 %>%
  mutate(HDDDec = ntile(hdd_rank, 10))

imd_07 <- imd_07 %>%
  mutate(HDDDec = ntile(hdd_rank, 10))
imd_10 <- imd_10 %>%
  mutate(HDDDec = ntile(hdd_rank, 10))

#More renaming as needed
imd_04 <- imd_04 %>% rename(HDDRank04=hdd_rank)
imd_10 <- imd_10 %>% rename(HDDRank10=hdd_rank)
imd_07 <- imd_07 %>% rename(HDDRank07=hdd_rank)
imd_15 <- imd_15 %>% rename(HDDRank15= HDDRank015)

imd_04 <- imd_04 %>% rename(HDDDec04= HDDDec)
imd_10 <- imd_10 %>% rename(HDDDec10= HDDDec)
imd_07 <- imd_07 %>% rename(HDDDec07= HDDDec)
imd_15 <- imd_15 %>% rename(HDDDec15= HDDDec)


imd_harmonised <- deprivation %>% left_join(imd_04_selected, by= 'lsoa11cd')%>%
  left_join(imd_07_selected, by= 'lsoa11cd') %>%
  left_join(imd_10_selected, by= 'lsoa11cd')%>%
  left_join(imd_15_selected, by= 'lsoa11cd')
View(imd_harmonised)

#saving the harmonised imd dataframe as a csv
write.csv(imd_harmonised, 'imd_harmonised.csv', row.names = F)
summary(imd_harmonised)

imd_harmonised <- imd_harmonised %>% rename(IMDScore19= IMDScore,
                                            HDDScore19= HDDScore,
                                            TotPop19 = TotPop)

birmingham <- imd_harmonised %>% filter(LADnm=='Birmingham')
summary(birmingham)

#there seems to be 45 new LSOAs since 2015, as they do not appear in 2004-2010
summary(imd_04$lsoa11cd)
summary(imd_15$lsoa11cd)

#remove NA values in 2004- 2010
cleaned_birmingham <- birmingham %>% filter(!is.na(IMDScore04)&!is.na(IMDScore07) & !is.na(IMDScore10)
                                           &!is.na(HDDScore04)& !is.na(HDDScore07) &!is.na(HDDScore10))


#Trends
#calculate the average scores for each year
#Since there is no year column in the data, i converted the data from wide to long format
imdscore_long <- cleaned_birmingham %>%
  pivot_longer(cols = starts_with("IMDScore"),
                              names_to = 'Year', values_to = 'IMDScore') %>%
  mutate(Year= sub('IMDScore', " ", Year)) #creates a new column year from the string by removing 'IMDScore'

imdscore_long$Year <- ifelse(nchar(as.character(imdscore_long$Year))==2, 
                             paste0('20', imdscore_long$Year),imdscore_long$Year)
imdscore_long$Year <- ifelse(nchar(as.character(imdscore_long$Year))==1, 
                             paste0('200', imdscore_long$Year),imdscore_long$Year)
imdscore_long$Year <- as.numeric(imdscore_long$Year)

hdd_long <- cleaned_birmingham %>%
  pivot_longer(cols = starts_with("HDDScore"),
               names_to = 'Year', values_to = 'HDDScore') %>%
  mutate(Year= sub('HDDScore', " ", Year))

#changing the two digit years(YY) to 4 digits (YYYY)
hdd_long$Year <- ifelse(nchar(as.character(hdd_long$Year))==2, 
                             paste0('20', hdd_long$Year),hdd_long$Year)
hdd_long$Year <- ifelse(nchar(as.character(hdd_long$Year))==1, 
                        paste0('200', hdd_long$Year),hdd_long$Year)
hdd_long$Year <- as.numeric(hdd_long$Year)


#Aggregating the IMDScores and HDDScores

imd_avg <- imdscore_long %>% group_by(Year) %>%
  summarise(Avg_IMDScore = mean(IMDScore, na.rm = TRUE))

hdd_avg <- hdd_long %>% group_by(Year) %>%
  summarise(Avg_HDDScore = mean(HDDScore, na.rm = TRUE))



#Descriptive statistics of the data population for 2019

summary(imd_harmonised$lsoa11cd) #Number of LSOAs in England
sum(imd_harmonised$TotPop19) #Total Population of England  as at Mid 2015 excl. prisoners
sum(imd_harmonised$DepChi) #Total Population of Dependent children (0-15) in England  as at Mid 2015 excl. prisoners
sum(imd_harmonised$Pop16_59) #Total Population aged 16-59 in England  as at Mid 2015 excl. prisoners
sum(imd_harmonised$Pop60_) #Total Population aged 60+ in England  as at Mid 2015 excl. prisoners

sum(imd_harmonised$WorkPop) #Working age population 18-59/64 in England excl. prisoners

summary(birmingham$lsoa11cd) #Number of LSOAs in Birmingham
sum(birmingham$TotPop19) #Total Population of Birmingham as at Mid 2015 excl. prisoners
sum(birmingham$DepChi) #Total Population of Dependent children (0-15) in Birmingham as at Mid 2015 excl. prisoners
sum(birmingham$Pop16_59) #Total Population aged 16-59 in Birmingham as at Mid 2015 excl. prisoners
sum(birmingham$Pop60_)#Total Population aged 60+ in Birmingham as at Mid 2015 excl. prisoners
summary(birmingham$Pop60_)

round(sum(birmingham$WorkPop)) #Working age population 18-59/64 in Birmingham excl. prisoners

#Average scores for the 7 domains of deprivation in 2019 for Birmingham

domain_averages <- birmingham %>%
  summarise(avg_imd= mean(IMDScore19, na.rm = T),
            avg_inc= mean(IncScore, na.rm= T),
            avg_emp= mean(EmpScore, na.rm= T),
            avg_edu= mean(EduScore, na.rm= T),
            avg_HDD= mean(HDDScore19, na.rm= T),
            avg_cri= mean(CriScore, na.rm= T),
            avg_BHS= mean(BHSScore, na.rm= T),
            avg_env= mean(EnvScore, na.rm= T))

#View the domain averages in a kable
library(knitr)
kable(domain_averages)

domain_stats <- birmingham %>%
  summarise(Min_imd = min(IMDScore19, na.rm = T),
            Max_imd = max(IMDScore19, na.rm = T),
            Min_inc = min(IncScore, na.rm = T),
            Max_inc = max(IncScore, na.rm = T),
            Min_emp = min(EmpScore, na.rm = T),
            Max_emp = max(EmpScore, na.rm = T),
            Min_edu = min(EduScore, na.rm = T),
            Max_edu = max(EduScore, na.rm = T),
            Min_HDD = min(HDDScore19, na.rm = T),
            Max_HDD = max(HDDScore19, na.rm = T),
            Min_cri= min(CriScore, na.rm = T),
            Max_cri = max(CriScore, na.rm = T),
            Min_BHS = min(BHSScore, na.rm = T),
            Max_BHS = max(BHSScore, na.rm = T),
            Min_env = min(EnvScore, na.rm = T),
            Max_env = max(EnvScore, na.rm = T))

domain_sd <- birmingham %>%
  summarise(sd_imd= sd(IMDScore19, na.rm = T),
            sd_inc= sd(IncScore, na.rm= T),
            sd_emp= sd(EmpScore, na.rm= T),
            sd_edu= sd(EduScore, na.rm= T),
            sd_HDD= sd(HDDScore19, na.rm= T),
            sd_cri= sd(CriScore, na.rm= T),
            sd_BHS= sd(BHSScore, na.rm= T),
            sd_env= sd(EnvScore, na.rm= T))


#reshaping the data to three rows
domainStats <- domain_stats %>%
  pivot_longer(cols= everything(), names_to= 'Domain_Statistic',
               values_to = 'Value')%>%
  separate(Domain_Statistic, into= c('Statistic', 'Domain'), sep = '_') %>%
  pivot_wider(names_from = Domain, values_from = Value)%>%
  mutate(statistic= factor(Statistic, levels= c('Min', 'Max')))
domainStats
kable(domainStats)

#Reading the shape data into a dataframe
england_shape <- read_sf(dsn= 'Indices_of_Multiple_Deprivation_(IMD)_2019', layer= 'Indices_of_Multiple_Deprivation_(IMD)_2019')

#filtering the shape of birmingham specifically into a dataframe
birmingham_shape <- england_shape %>% filter(LADnm== 'Birmingham')
View(birmingham_shape)

#joining the shape data to the birmingham dataframe
library(dplyr)
birm_geometry <- birmingham_shape %>%
  select(lsoa11cd, geometry) #selects only the geometry and LSOA code as the common identifier

birmingham <- birmingham %>%
  left_join(birm_geometry, by= 'lsoa11cd')

#Make a copy of the datafram to convert to sf object, usable for spatial visualizations
birmingham_copy <- birmingham
birmingham_copy <- st_as_sf(birmingham_copy)
cleaned_birmingham_copy <- cleaned_birmingham

#The machine found some problems with some geometry values, so this is a fix for it
invalid_geoms <- which(!st_is_valid(birmingham_copy)) #finds the invalid geometries
print(invalid_geoms)

birmingham_copy$geometry <- st_make_valid(birmingham_copy$geometry) #fixes the invalid geomteries


#Plotting the deciles on the geometry of Birmingham
#Health related deprivation map, 2015
HDDDec15_map <- ggplot(birmingham_copy) +
  geom_sf(aes(fill = factor(HDDDec15)))+ 
  scale_fill_viridis_d(name = "HDD Decile where\n1 is the most deprived", option = 'D') +
  labs(subtitle = 2015,
       caption = "Source: English Index of Multiple Deprivation 2015") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size = 15, face = 'bold.italic'),
        plot.caption = element_text(size= 10),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank())

#health related deprivation map, 2019
HDDDec19_map <- ggplot(birmingham_copy) +
  geom_sf(aes(fill = factor(HDDDec)))+ 
  scale_fill_viridis_d(name = "HDD Deciles where\n1 is the most deprived", option = 'D') +
  labs(subtitle = 2019,
       caption = "Source: English Index of Multiple Deprivation 2019") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size = 15, face = 'bold.italic'),
        plot.caption = element_text(size= 10),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        axis.ticks = element_blank())

#combine the two maps into a composite figure for the purpose of comparison
combined_map <- HDDDec15_map + HDDDec19_map
combined_map <- combined_map +
  plot_annotation(
    title = "Health and Disability Deprivation in Birmingham: 2015 vs. 2019"
  ) &
  theme(
    plot.title = element_text(size = 20, face = "bold")
  )
combined_map
ggsave("combined_HDD_maps.png", combined_map, width = 16, height = 9) #save the plot

#Overall IMD 2019 map
IMD_Dec_plt <- ggplot(birmingham_copy) +
  geom_sf(aes(fill = factor(IMD_Decile))) + 
  scale_fill_viridis_d(name = "IMD 2019 Deciles where\n1 is the most deprived", option = 'D') +
  labs(caption = "Source: English Index of Multiple Deprivation 2019") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.caption = element_text(size= 15),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

    
ggsave("overall_IMDDec_map.png", IMD_Dec_plt, width = 15, height = 10) #save the plot
combined_map <- HDDDec15_map + HDDDec19_map


install.packages("patchwork")
library(patchwork)

#persistent deprivation


# Define the threshold (top 10%)
threshold <- quantile(birmingham$HDDRank, 0.1, na.rm = TRUE)

# Identify persistently most deprived LSOAs
persistently_deprived <- cleaned_birmingham %>%
  filter(HDDRank04 <= threshold, 
         HDDRank07 <= threshold, 
         HDDRank10 <= threshold, 
         HDDRank15 <= threshold, 
         HDDRank <= threshold)


count(persistently_deprived)
View(persistently_deprived)

persistently_deprived %>% select(lsoa11cd, HDDRank, HDDScore19, IMD_Rank, IMDScore19, TotPop19) %>% view()

#Correlation analysis + corresponding scatter plot
#Income domain and Health Deprivation
cor.test(birmingham$HDDScore19, birmingham$IncScore)
inc.plot <- ggplot(birmingham, aes(x= HDDScore19, y= IncScore))+
  geom_point()+
  geom_smooth(method = 'lm', se= F)+
  labs(x= 'Health Deprivation and Disability', y= 'Income')+
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#Employment deprivation domain and Health Deprivation
cor.test(birmingham$HDDScore19, birmingham$EmpScore)
emp.plot <- ggplot(birmingham, aes(x= HDDScore19, y= EmpScore))+
  geom_point()+
  geom_smooth(method = 'lm', se= F)+
  labs(x= 'Health Deprivation and Disability', y= 'Employment')+
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#Education deprivation domain and Health Deprivation
cor.test(birmingham$HDDScore19, birmingham$EduScore)
edu.plot<- ggplot(birmingham, aes(x= HDDScore19, y= EduScore))+
  geom_point()+
  geom_smooth(method = 'lm', se= F)+
  labs(x= 'Health Deprivation and Disability', y= 'Education, skills and training')+
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#Crime domain and Health Deprivation
cor.test(birmingham$HDDScore19, birmingham$CriScore)
cri.plot <- ggplot(birmingham, aes(x= HDDScore19, y= CriScore))+
  geom_point()+
  geom_smooth(method = 'lm', se= F)+
  labs(x= 'Health Deprivation and Disability', y= 'Crime')+
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#Barriers to housing and services domain and Health Deprivation
cor.test(birmingham$HDDScore19, birmingham$BHSScore)
bhs.plot <- ggplot(birmingham, aes(x= HDDScore19, y= BHSScore))+
  geom_point()+
  geom_smooth(method = 'lm', se= F)+
  labs(x= 'Health Deprivation and Disability', y= 'Barriers to housing and services')+
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#Living Environmentdomain and Health Deprivation
cor.test(birmingham$HDDScore19, birmingham$EnvScore)
env.plot <- ggplot(birmingham, aes(x= HDDScore19, y= EnvScore))+
  geom_point()+
  geom_smooth(method = 'lm', se= F)+
  labs(x= 'Health Deprivation and Disability', y= 'Living Environment')+
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

library(gridExtra)
#composite chart of scatter plots
scatter_plots <-grid.arrange(inc.plot, emp.plot, edu.plot, cri.plot, bhs.plot, env.plot, ncol= 2, nrow= 3)
ggsave("Scatter Plots.png", scatter_plots, width = 15, height = 15)


#Multivariate Regression
library(MASS)

#Test for multicollinearity
cor.test(birmingham$CriScore, birmingham$EmpScore)
cor.test(birmingham$IncScore, birmingham$EduScore)
cor.test(birmingham$IncScore, birmingham$CriScore)

#make a copy of the original birmingham dataset

birmingham2 <- birmingham[sample(1:nrow(birmingham)),] #randomize rows
train_size= 0.7 #training data contains 70% of the data
birm_train <- birmingham2[1:(train_size*nrow(birmingham2)),]
birm_test <- birmingham2[nrow(birm_train):nrow(birmingham2),]

#training model with stepwise suggestions
train_model <- lm(formula= HDDScore19 ~ IncScore + EmpScore + EduScore +CriScore,
                  data= birm_train)
summary(train_model)

#prediction
birm_test$predicted <- predict(train_model, newdata = birm_test)
birm_test$residuals <- birm_test$predicted- birm_test$HDDScore19
birm_test %>% select(HDDScore19, predicted, residuals)%>% view()

install.packages("car")
library(car)
vif(train_model2) #checks for multicollinearity

#sum of squared errors
sum(birm_test$residuals**2)

train_model3 <- lm(formula= HDDScore19 ~ EmpScore + EduScore +CriScore,
                  data= birm_train)

#residual mean squared error

rmse <- sqrt(mean((birm_test$HDDScore19 - birm_test$predicted2)**2))
rmse

sqrt(mean((birm_test$HDDScore19 - birm_test$predicted2)**2))

#training model to avoid multicollinearity
train_model2 <- lm(formula= HDDScore19 ~ EmpScore +CriScore,
                   data= birm_train)

summary(train_model3)
birm_test$predicted2 <- predict(train_model2, newdata = birm_test)
birm_test$residuals2 <- birm_test$predicted- birm_test$HDDScore19

sum(birm_test$residuals2**2)

birm_test %>% select(lsoa11cd, EmpScore, CriScore, HDDScore19, predicted, residuals) %>%
  head(10)%>% view()

