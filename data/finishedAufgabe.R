

### Exercice 1 Gnaoui Salim and Gharbi Marwen
### 2.1 is done, 

###2.2 
### get the csv file to open, file path is given as argument
myread.cvsdata <- function (path) {
  ### transform the csv into data frame, 
  ###after some searches, I figured out that I should use '/t' as seperator as my local-configuration is in German, otherwise it will not work , 
  data=read.table((path),header = T,sep = "\t")
  ###  import the csv file, and prevent r fron transforming the columns (names are given in the vector c) to "FACTOR"
  data=read.table((path),header = T,sep = "\t",as.is=c("developer","tstamp","file"))
  ### YOU can run View(data) to visualize the dataframe (if you are using R studio)
  ### extract the column named file (first position) from data , 
  
  file=data[,1]
  ### extract the column named developer (3 rd position) from data , 
  developer=data[,3]
  ### transform the extracted sets  to vectors then to factor
  file=factor(c(file))
  developer=factor (c(developer))
  data <- cbind(data,data.frame(file))
  data <- cbind(data,data.frame(developer))
  ### rename the factors (position 7 and 8)
  names(data)[[(8)]] <- "filef"
  names(data)[[(9)]] <- "developerf"
  data <- subset(data, select = -c(7))
  
  return (data) ;
  
}

count.developer <- function (data) {
  developer=result[,"developer"]
  developer=factor(developer)
  return (nlevels(developer))
}
developer.busy <- function (data) {
  w = table(data$developer)
  sort((w)) 
  return (w)
}
### THIS WILL just RETURN A FREQUENCY table , developer by file. and should be sorted 
developer.changedFils <- function (data)
{
  
 resullt = ( data.frame ( table ( data$developer, data$file) ) )
 ### we got a table result with 3 columns , var1 is developer name, var2 file name, and Freq is frequency
 
 ### we count the total number of files , we will need it for the c 
 totalnumberoffile=nlevels(resullt[,2])

 return (result)
 
}
### this function will transform the tmstpf field to POSIXct
### argument : the dataset 
transformtmstp <- function  (dataset){
  ### get the tmstpf column
  tstamp2=dataset[,'tstamp'] 
  ### CONVERT IT TO POSTIXCT
  tstamp2=as.POSIXct(tstamp2)
  ### Append it to the dataframe
  dataset <- cbind(dataset,data.frame(tstamp2))
  ### return the result (the dataframe and the tstampf column)
  return (dataset)
   
}
### this funcction will transform the tmstmp to "unix format"
transformtounixFormat <- function  (dataset){
  ### get the tmstpf2 column
  tstamp3=dataset[,'tstamp2'] 
  ### CONVERT IT TO numeric
  tstamp3=as.numeric(tstamp3)
  ### Append it to the dataframe
  dataset <- cbind(dataset,data.frame(tstamp3))
  ### return the result (the dataframe and the tstampf column)
  return (dataset)
  
}
### will compute the diffrence between two dates in POSTformat
diffPOstin <- function  (dataset){
  ### get the tmstpf2 column
  tstamp2=dataset[,'tstamp2'] 
  ### print the diff between index 1 and 2 
  print (tstamp2[1]-tstamp2[2])
 
}
### will compute the diffrence between two dates in Unix
diffUnixFormat <- function  (dataset){
  ### get the tmstpf3 column
  tstamp3=dataset[,'tstamp3'] 
  ### print the diff between index 1 and 2 
  print.default (tstamp3[1]-tstamp3[2])

}

getWeekday <- function  (dataset){
  ### get the tmstpf2 column
  ### tstamp2=dataset[,'tstamp2'] 
  ###  tstamp2 = as.factor(tstamp2)
  
  k <- weekdays(as.Date(dataset$tstamp2))
  dataset$wday=as.factor(k)
  return (dataset)
}


setHours <- function  (dataset){

  k <- as.POSIXlt(dataset$tstamp2)$hour
  dataset$hour=as.factor(k)
  return (dataset)
}
###  
raw.hours <- function (dataset)
{ ### add levels ( to show empty values)
  levels(dataset$hour) <- c(levels(dataset$hour), c(0:23))
  
  hours<-table(unlist(dataset$hour))
  return(hours)
}

raw.weekdays <- function (dataset)
{ ### get weekdays and put them into a vector
  ### i choosed a random date
  xd <- as.Date("2012-07-27")
  daysvector= c(weekdays(xd + 0:6))
  ###
  
  levels(dataset$wday) <- c(levels(dataset$wday), daysvector)
  
  wdays<-table(unlist(dataset$wday))
  return(wdays)
}
myplot.hours.bars = function (hours){
  plot(hours)
}
myplot.wdays.bars = function(wday){
  plot(wdays)
}



result=myread.cvsdata(file.choose())
result =transformtmstp(result)
result =transformtounixFormat(result)
diffPOstin(result)
diffUnixFormat(result)
### append week day 
result=getWeekday(result)
### append hours day 
result=setHours(result)
hours =raw.hours(result)
max(hours)
min(hours)
weekdays = raw.weekdays(result)
max(weekdays)
min(weekdays)
plot(weekdays)
plot(hours)
plot(result)
