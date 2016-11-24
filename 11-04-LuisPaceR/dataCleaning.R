#install.packages('devtools')
#devtools::install_github('fawda123/rStrava')
#devtools::install_github("ALShum/rwunderground")
#install.packages("httpuv")
#install.packages('weatherData')
library('rStrava'); library('httpuv');
library(weatherData); library('rwunderground');

### You will need API access from Strava and 
### Wunderground. No attempt was made to make this file 
### readable or reusable. 

app_name <- 'runPace'
# chosen by user

app_client_id  <- 'your client ID'
# an integer, assigned by Strava

app_secret <- 'your app secret'
# an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# df containing summaries of recent runs
# data = compile_activities(get_activity_list(stoken))
# head(data)

# sorting out the trash variables
# we keep AvgSpeed, Distance, ElapsedTime, MovingTime, ElevGain
# Units:::
# speed: km/hr, distance: km, 
# elapse/moving time: seconds, ElevGain: metres
#n = colnames(data)
#keeps = c(n[5],n[8],n[9],n[29], n[41])

# sorting out variables with location/time data
# we keep City/Country/State/StartTime
#keeps2 = c(n[21], n[22], n[23], n[34])

#dataNew = data[keeps]
#dataDates = data[keeps2]

# StartTime in dataDates is in the form "YYYY-MM-DDTHH:MM:SSZ"
# we want in the form "YYYMMDD_HHMMSS"
for (i in c(1:98)){
  S = dataDates[i, 4]
  S = sub("-", "", S); S = sub("-", "", S)
  S = sub(":", "", S); S = sub(":", "", S)
  S = sub("T", "_", S); S= sub("Z", "", S)
  dataDates[i,4]=S
}




# We are interested in the humidity (%) and temperature (deg F)
# The following sectio adds both quantities to our main df

# API key to get weather data from wunderground
# get your own. Limit: 10calls/min, 500calls/day
rwunderground::set_api_key("your api key")

# adding new columns to put our values in
# also adding a new column to separate date of runs from
# today's date. in number of months. 

dataNew$temp <- 0; dataNew$hum <- 0; dataNew$months <- 0;

todaysDate = strptime(Sys.Date(), format="%Y-%m-%d")

for (i in c(1:98)){
  # location = set_location(territory = dataDates[i,3], city = dataDates[i,1])
   day = substr(dataDates[i,4], 1, 8)
  # we only keep the hour. (hr < 12) = AM, PM otherwise
  # the variable will be converted to Int
  time = strtoi(substr(dataDates[i,4], 10, 11), 10L);
  if (time == 0){
    time = 1
  ## i went on a midnight run once, so i need this...
  }
  #
  # # df containing weather data.
  # # the i-th row contins info for the (i+1)-th hour of day
  weather = history(location, date = day);
  #
  # set temperature
  dataNew[i, 6] = weather[time, 2]
  #
  # set humidity
  dataNew[i, 7] = weather[time, 3]

  # compute difference in weeks
  dateOfRun = strptime(day, format="%Y%m%d")
  dataNew[i,8] = floor(difftime(todaysDate,dateOfRun)/12)
  
}



# exporting all df's used as .csv files
# you will have to load them as .csv files
# to mess with this code

write.csv(data, file = "dataRaw.csv")
write.csv(dataDates, file = "dataRawDates.csv")
write.csv(dataNew, file = "dataFinal.csv")

dataNew = read.csv("dataFinal.csv")

# i manually added two entries
# and recalculated months > days

data <- read.csv("dataFinal.csv")
dataDates <- read.csv("dataRawDates.csv")
data$days <- 0

for (i in c(1:100)){
  day = substr(dataDates[i,4], 1, 8)
  
  # compute difference in days
  dateOfRun = strptime(day, format="%Y%m%d")
  todaysDate = strptime("20161031", format="%Y%m%d")
  data[i,9] = difftime(todaysDate,dateOfRun)
  
}

write.csv(data, file = "dataFinalFinal.csv")
dta$months<- NULL

#
# had to flip the rows of data for
# a time series model

data <- read.csv("dataFinalFinal.csv")
data$moving_time<-NULL
data$months<-NULL

for(i in c(1:50)){
  topVec = data[i,]
  bottomVec = data[101-i,]
  data[i,] = bottomVec
  data[101-i,] = topVec
}


write.csv(data, file = "dataFinal.csv")



