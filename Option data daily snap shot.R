library(WindR)
w.start()
library(data.table)
#set initial variables
StartDate = as.Date('01/17/2018',"%m/%d/%Y") 
EndDate =as.Date('11/16/2018',"%m/%d/%Y")
ACode = "M1901.DCE" #wind code of underlying asset

#get dataset of option contracts, including code, contract type, exercise price and listed_date
Contr.data = w.wset('optionfuturescontractbasicinfo','exchange=DCE;productcode=M;contract=M1901.DCE;field=wind_code,call_or_put,exercise_price,listed_date')
Contr.data #3-D list
CCode = paste0(Contr.data[[2]][2][Contr.data[[2]][3]=='认购'],'.dce' )#Call option code
PCode = paste0(Contr.data[[2]][2][Contr.data[[2]][3]=='认沽'],'.dce' )#Put option code
NumCon = length(CCode) # number of contracts call = put
Days = as.character(w.tdays(StartDate,EndDate)$Data$DATETIME)#get date sequence

#Create an empty data table
df = as.data.table(matrix(NA, nrow = length(Days)*NumCon))
setnames(df,c("V1"),c('Date'))
# set table frame
for (i in 1:length(Days)){
  df$Date[1+(i-1)*NumCon] <- Days[i]
  for (k in 1:NumCon){
    df$CCode[k+(i-1)*NumCon] <- as.character(CCode[k])
  }
}

# add one column indicating the strike price
df[,K:= as.numeric(sapply(df$Code,function(x) substr(x,9,12)))]
#Get underlying asset's daily close
df$AClose <- NA
for (i in 1:length(Days)){
  tdate = Days[i]
  df$AClose[1+(i-1)*NumCon] <- w.wsd(ACode,'close',tdate,tdate)[[2]][[2]]
}
#Using call option contracts as an example
#Get option contract's daily close,return and implied volatility
for (k in 1:NumCon){
  data = w.wsd(CCode[k],'close,pct_chg,us_impliedvol',StartDate,EndDate)
  for(i in 1:length(Days)){
    df$CClose[k+(i-1)*NumCon] = data[[2]][[2]][i]
    df$CRtn[k+(i-1)*NumCon] = data[[2]][[3]][i]/100
    df$CVolatility[k+(i-1)*NumCon] = data[[2]][[4]][i]
  }
}
#Convert obtained data to numeric form
df[[3]] <- as.numeric(df[[3]])
df[[4]] <- as.numeric(df[[4]])
df[[5]] <- as.numeric(df[[5]])
df[[6]] <- as.numeric(df[[6]])
df[[7]] <- as.numeric(df[[7]])

#Calculate daily instrict value (call option)
for(i in 1:length(Days)){
  S = df$AClose[1+(i-1)*NumCon] #Daily spot price
  for (k in 1:NumCon){
    instrvalue = S - df$K[k+(i-1)*NumCon]
    if (instrvalue > 0){
      df$InstrValue[k+(i-1)*NumCon] = instrvalue #in the money or at the money
    }
    else if(!is.na(df$CClose[k+(i-1)*NumCon])){
      df$InstrValue[k+(i-1)*NumCon] = 0 #out of the money
    }
    else
      df$InstrValue[k+(i-1)*NumCon] =NA
  }
}

#Get daily time value (call option)
df[InstrValue>0,TimeValue := CClose-InstrValue]
df[InstrValue<0,TimeValue := CClose]
#replace NA with empty value
df[is.na(df)] <- " "



