#TASK 1
#1.1
csv_dataset=read.csv("/Users/Lyazzat/Desktop/R APU/11th March/covid_19_india.csv")
df=as.data.frame(csv_dataset)
colnames(df)=c('Sno',
               'Date',
               'Time',
               'StateUnionTerritory',#due to the "/" updated column name to avoid errors""
               'ConfirmedIndianNational', 
               'ConfirmedForeignNational',
               'Cured',
               'Deaths',
               'Confirmed') 
View(df)#show data as a grid

#1.2
df$Sno=as.numeric(df$Sno)
df$Date=as.Date(df$Date)
df$Time=as.character(df$Time)
df$StateUnionTerritory=as.character(df$StateUnionTerritory)
suppressWarnings(df$ConfirmedIndianNational<-as.numeric(df$ConfirmedIndianNational))
suppressWarnings(df$ConfirmedForeignNational<-as.numeric(df$ConfirmedForeignNational))
suppressWarnings(df$Cured<-as.numeric(df$Cured))
suppressWarnings(df$Deaths<-as.numeric(df$Deaths))
suppressWarnings(df$Confirmed<-as.numeric(df$Confirmed))

# Create a function to replace NA values with mean per matching territory
f_replace_na_with_mean=function(column) {
  territories=unique(df$StateUnionTerritory)
  for (territory in territories) {
    mean_value=mean(column[df$StateUnionTerritory == territory], na.rm = TRUE)
    column[df$StateUnionTerritory == territory & is.na(column)]=mean_value
  }
  return(column)
}

# Replace NA values in each column with mean per matching territory
if (anyNA(df)){
df$ConfirmedIndianNational=f_replace_na_with_mean(df$ConfirmedIndianNational)
df$ConfirmedForeignNational=f_replace_na_with_mean(df$ConfirmedForeignNational)
df$Cured=f_replace_na_with_mean(df$Cured)
df$Deaths=f_replace_na_with_mean(df$Deaths)
df$Confirmed=f_replace_na_with_mean(df$Confirmed)
}
"""
if (anyNA(df)){
  df$ConfirmedIndianNational[is.na(df$ConfirmedIndianNational)]=mean(df$ConfirmedIndianNational, na.rm = TRUE)
  df$ConfirmedForeignNational[is.na(df$ConfirmedForeignNational)]=mean(df$ConfirmedForeignNational, na.rm = TRUE)
  df$Cured[is.na(df$Cured)]=mean(df$Cured, na.rm = TRUE)
  df$Deaths[is.na(df$Deaths)]=mean(df$Deaths, na.rm = TRUE)
  df$Confirmed[is.na(df$Confirmed)]=mean(df$Confirmed, na.rm = TRUE)
}
"""
#1.3
result=all(
  class(df$Sno) == "numeric",
  class(df$Date) == "Date",
  class(df$Time) == "character",
  class(df$StateUnionTerritory) == "character",
  class(df$ConfirmedIndianNational) == "numeric",
  class(df$ConfirmedForeignNational) == "numeric",
  class(df$Cured) == "numeric",
  class(df$Deaths) == "numeric",
  class(df$Confirmed) == "numeric"
);result#should return TRUE in case of all data types are correct

View(df)

#TASK 2
#2.1
s=summary(df);s
#2.2
total_records=nrow(df);total_records
#2.3
max_confirmed_day=df$Date[which.max(df$Confirmed)]
max_cured_day=df$Date[which.max(df$Cured)]
output_df=data.frame(
  "Day with highest number of confirmed cases" = max_confirmed_day,
  "Day with highest number of cured cases" = max_cured_day
)
# View the data frame
View(output_df)
#2.4
install.packages("plotrix")
library(plotrix)

values=c(sum(df$Cured), sum(df$Deaths), sum(df$Confirmed))
labels=c("Cured", "Deaths", "Confirmed")
total_cases=sum(values)
percentages=round(values / total_cases * 100, 1)
labels_with_percentage=paste(labels, ": ", percentages, "%")

pie3D(values,
      labels = labels_with_percentage,
      main = "COVID-19 Cases Distribution",
      col = c("yellow", "red", "orange") ,
      explode = 0.2) 

#TASK 3
#3.1
aggregate_data_per_terr=aggregate(cbind(Cured, Deaths, Confirmed) ~ StateUnionTerritory, 
                                     data = df, FUN = sum);aggregate_data_per_terr
#3.2
df$Date=as.Date(df$Date)
df$Month=format(df$Date, "%B %Y")
average_cases_per_month=aggregate(cbind(Cured, Deaths, Confirmed) ~ Month, 
                                  data = df, 
                                  FUN = mean);average_cases_per_month

#TASK 4
#4.1
correlation=cor(average_cases_per_month$Confirmed, average_cases_per_month$Cured)
# Present the results
cat("Average number of confirmed cases per month:", mean(average_cases_per_month$Confirmed), "\n")
cat("Average number of cured cases per month:", mean(average_cases_per_month$Cured), "\n")
cat("Correlation between confirmed and cured cases:", correlation, "\n")

#4.2
options(digits = 7)#to numeric data on the axes

plot(average_cases_per_month$Confirmed, 
     average_cases_per_month$Cured,
     xlab = "Confirmed Cases", 
     ylab = "Cured Cases", 
     main = "Relationship between \nConfirmed and Cured Cases",
     col = "red",#points
     col.lab = "red",#labels                    
     col.main = "blue",#title
     grid(nx = NULL, ny = NULL)
     )

abline(a = 0, b = 1, col = "green", lty = 2) #the line

