library(plotly)
library(covid19.analytics)
install.packages("scatterplot3d")
install.packages("covid19.analytics")
install.packages("devtools")
devtools::install_github("mponce0/covid19.analytics")

library(ggplot2)
library("scatterplot3d")
setwd("/Users/rayanwali/Documents/Cornell University/Semester 3 (FA20)/ENGRD 2700 - Basic Engineering Probability and Statistics/Homeworks/Fall 2020/COVID-19 Data Analysis Project")
data <- read.csv(file = 'national-history.csv')
datatrimmed <- read.csv(file = 'national-history-trimmed.csv')
date <- data$date
deaths <- data$death
cases <- data$positive
states <- data$states
sd(cases, na.rm = FALSE)
hospitalizations <- data$hospitalizedCurrently
cor(data$hospitalizedCurrently, deaths, use = "complete.obs")
summary(lm(hospitalizations~deaths, data))

data("datatrimmed", package="datasets")
datetrimmed = datatrimmed$date
deathstrimmed = datatrimmed$death
model <- lm(deathstrimmed ~ datetrimmed, data = datatrimmed)
summary(model)
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(datatrimmed, pred.int)
ggplot(mydata, aes(date,death)) + geom_point() + stat_smooth(method = lm)

plot(hospitalizations,deaths, main="Deaths vs. Hospitalizations", xlab="Number of Hospitalizations", ylab="Number of Deaths")
plot(date,deaths, main="Deaths vs. Time (Year)", xlab="Time (Year)", ylab="Number of Deaths")
plot(date,cases, main="Cases vs. Time (Year)", xlab="Time (Year)", ylab="Number of Cases")
plot(date,hospitalizations,main="Hospitalizations vs. Time (Year)", xlab="Time (Year)", ylab="Number of Hospitalizations")

library(ggplot2)
library("scatterplot3d")
library(plotrix)
data2 <- read.csv(file = 'provisional-COVID-19-death-counts-by-sex-state-age.csv')
#data2.frame()
age <- data2$Age.group
gender <- data2$Sex
states <- data2$State
#lst <- lapply(unlist(data2$Start.week),as.integer)
#print(lst)
#match('3',lst)
time <- as.numeric(substring(lst[1], 1, 2))
deaths <- data2$COVID.19.Deaths
df <- data2[c("State","COVID.19.Deaths")]

df2 <- data2[c("Sex","COVID.19.Deaths")]
filter(df2, df2$Sex == "Male")
maleDeaths <- sum(df2[df2$Sex == "Male",]$COVID.19.Deaths,na.rm=TRUE)
filter(df2, df2$Sex == "Female")
femaleDeaths <- sum(df2[df2$Sex == "Female",]$COVID.19.Deaths,na.rm=TRUE)
maleDeathProportion = maleDeaths/(maleDeaths + femaleDeaths)
femaleDeathProportion = femaleDeaths/(maleDeaths + femaleDeaths)
slices <- c(maleDeathProportion, femaleDeathProportion)
lbls <- c("Male", "Female")
pie3D(slices, labels=lbls, explode=0.1, main="COVID-19 Deaths Across Gender")

pie = ggplot(df2, aes(x=Sex, y=COVID.19.Deaths)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(value*100), "%")), position = position_stack(vjust = 0.5))
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "COVID-19 Deaths Across Genders")
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie
slices <- c(0.6,0.4)
lbls <- c("Male", "Female")
pie3D(slices,labels=lbls,explode=0.1,main="COVID-19 Deaths Across Age Group")
#print(df)
#print(max(data2$maxDeaths))
#maxState <- 0
#maxDeaths <- 0
#for (row in 1:nrow(df)) {
  #state <- df[row, "State"]
  #numDeaths <- df[row, "Total Deaths"]
  #print(numDeaths)
  #if (numDeaths > maxDeaths) {
  #  maxDeaths <- numDeaths
  #  maxState <- state
  #}
#}
#print(maxState)
df = df[-c(1:64), ]
#head(df)
#print(df)
#ggplot(df, aes(x=State, y=COVID.19.Deaths, fill="red")) + geom_bar(stat="identity", width=0.1)
colors <- c("#999999", "#E69F00", "#56B4E9")
#scatterplot3d(x=age,y=data2$Start.week,z=deaths,main="Deaths Per Age Group and Time", xlab="Age (years)", ylab="Month", zlab="Number of Deaths", color=colors[as.factor(deaths)], pch=19)
attach(data2)
#plot(age,deaths,main="Deaths Per Age Group", xlab="Age (years)", ylab="Number of Deaths", color=colors[as.factor(deaths)], pch=19)
plot(gender,deaths,main="Deaths Per Gender", xlab="Gender", ylab="Number of Deaths", color=colors[as.factor(deaths)], pch=19)
cor(age,deaths,use = "complete.obs")
cor(state,deaths,use = "complete.obs")

# JHU COVID-19 Data Analysis Tools
covid19.data.ALLcases <- covid19.data()
report.summary()
