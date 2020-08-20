#install.packages("padr")

library(tmap)

# read data
data2= read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# create new variables
data2$RDate <- as.Date(data2$date, "%Y-%m-%d")
data2$testspercase = data2$new_tests/data2$new_cases
data2$casespertest = data2$new_cases/data2$new_tests
data2$rtc = data2$total_cases_per_million/1000/data2$total_tests_per_thousand
data2$dpc = data2$total_deaths/data2$total_cases

data("World")
map= World

# merge data with map
total2 <- merge(map,data2, by.x="iso_a3",by.y="iso_code",all=FALSE)
# Create subset for dygraphs
tab <- table(data2$location)
complete <- names(tab)[tab==max(tab) & names(tab)!="OWID_WRL"]

#complete <- names(tab)[tab==max(tab)]
completedata = subset(data2, data2$location %in% complete)
