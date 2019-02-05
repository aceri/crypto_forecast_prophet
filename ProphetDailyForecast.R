library(prophet)
library(plyr)

# Don't forget to uncompress the 7zip file.
dat <- read.csv("BF_BTC_USD.csv",header=TRUE)
df <- as.data.frame(dat)

df$Date <- gsub("/","-",df$Date)
df$Date<-strptime(x=df$Date,format="%d-%m-%Y %H:%M",tz="")
df$Date<-as.POSIXct(df$Date)
plot(df[,c("Date","Close")],type="l")
nrow(df)

# Daily average of Close.
df$Grupo<-as.factor(format((df$Date),"%Y-%m-%d"))
df_dia<-ddply(df, .(df$Grupo),summarise,m=mean(Close))

# We don't want so much data. Getting only the most recent half
df_dia<-tail(df_dia,round(nrow(df_dia)/2))

# Column names required by prophet package
names(df_dia)<-c("ds","y")

m<-prophet(df_dia,daily.seasonality = TRUE)
nrow(df_dia)

# We want to see a couple weeks in advance
desired_periods=14 
desired_periods

future_df_prophet=make_future_dataframe(m,periods=desired_periods,freq="day",include_history = TRUE)
forecast=predict(m,future_df_prophet)

# Plotting the forecast
plot(m,forecast)

# Plotting the forecast grouped by week, year, day, etc.
prophet_plot_components(m,forecast)



