scores_data <- read.csv(file="pisa_scores.csv")
is.na(scores_data)
anyNA(scores_data)


library(dplyr)
scores_data_tibb <- as_tibble(scores_data)

#select
selected_scores <- scores_data_tibb %>% select(LOCATION,GENDER,TIME,MATH,SCIENCE,READING)
head(selected_scores)


#arrange
arrange(selected_scores, desc(MATH), desc(SCIENCE), desc(READING))


#mutate
selected_scores %>% mutate(AverageScore=(MATH+SCIENCE+READING)/3)


#group_by() & summarise() & top_n()
selected_scores %>%  
  group_by(GENDER) %>% 
  summarize(Average_Math=mean(MATH), Average_Science=mean(SCIENCE), Average_Reading=mean(READING) )%>%
  top_n(2,Average_Math)

selected_scores %>% 
  group_by(LOCATION,GENDER) %>% 
  summarise(Highest_Math=max(MATH),Highest_Science=max(SCIENCE),Highest_Reading=max(READING),
            Lowest_Math=min(MATH),Lowest_Science=min(SCIENCE),Lowest_Reading=min(READING))


################################# ggplot2 (1) #################################
library(ggplot2)

tr_data <- selected_scores[which(selected_scores[1] == 'TUR' & selected_scores[2] == 'BOY'),]
tr_data2 <- selected_scores[which(selected_scores[1] == 'TUR' & selected_scores[2] == 'GIRL'),]

tr_data$AverageScore <- (tr_data$MATH + tr_data$SCIENCE + tr_data$READING )/3
tr_data2$AverageScore <- (tr_data2$MATH + tr_data2$SCIENCE + tr_data2$READING )/3

specie <- c("2003", "2006", "2009", "2012", "2015", "2018")
cls <- rep(c("#3C3B6E", "red"), 2);

data <- rbind(tr_data, tr_data2)

ggplot(data, aes(fill=GENDER, y=AverageScore, x=TIME)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Turkey PISA Comparision by Gender", subtitle = "Age 15+ (2003-2018)") +
  xlab("Year") + ylab("PISA Score") + scale_fill_manual(values = cls) + labs(fill="") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5)
  ) 

################################# ggplot2 (2) #################################
library(ggplot2)

tr_data <- selected_scores[which(selected_scores[1] == 'TUR'),]
  tr_data$AverageScore <- (tr_data$MATH + tr_data$SCIENCE + tr_data$READING )/3
usa_data <- selected_scores[which(selected_scores[1] == 'USA'),]
  usa_data$AverageScore <- (usa_data$MATH + usa_data$SCIENCE + usa_data$READING )/3
gbr_data <- selected_scores[which(selected_scores[1] == 'GBR'),]
  gbr_data$AverageScore <- (gbr_data$MATH + gbr_data$SCIENCE + gbr_data$READING )/3
oecd_data <- selected_scores[which(selected_scores[1] == 'OECD'),]
  oecd_data$AverageScore <- (oecd_data$MATH + oecd_data$SCIENCE + oecd_data$READING )/3

data <- rbind(tr_data, usa_data, gbr_data, oecd_data)

ggplot(data=data,aes(x=TIME,y=AverageScore,col=LOCATION,shape=GENDER))+
  geom_point(size=3)+
  geom_line(size=1)+ 
  theme_light()+
  ggtitle("PISA Test Score Comparision")+
  theme( plot.title = element_text(color = "Black",size = 14,face = "bold",hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5))+
  xlab("Year")+
  ylab("Average PISA Score")

################################# ggplot2 (3) #################################
library(ggplot2)

recentScores <- selected_scores[which(selected_scores[3] == '2018'),]

ggplot(recentScores, aes(x = SCIENCE, y = MATH)) + 
  geom_point(aes(color = LOCATION, size=GENDER))+
  ggtitle("Math & Science Proficiency Comparision", subtitle = "Among OECD Countries in 2018 (recent scores)")+
  theme( plot.title = element_text(color = "Black",size = 14,face = "bold",hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5))+
  ylab("Math Score")+
  xlab("Science Score")
