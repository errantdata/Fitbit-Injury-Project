#This script will read in the workout data (primary key = workout, features=date, category, etc)
#and reorganize it into daily data (primary key = date, features = workout information, injury information)

#Read in the available data
# assumes data is in a 'DATA' folder in the working directory
#datafile <- file.path(getwd(), "DATA", "FitNotes",                 #This method didn't work for me, so I'll switch to primative means
 #                     "FitNotes_Export_2016_09_22_14_49_18.csv")   #It is not because "DATA" is in all caps (debugged below)

dat=read.csv(file="../Data/FitNotes/FitNotes_Export_2016_09_22_14_49_18.csv", stringsAsFactors = F)
# convert dates to date - see if this helps with NAs by coercion
dat$Date=as.Date(dat$Date)
entries=unique(dat$Date) #Unique days recorded

# function to convert dat$Time to Minutes
mins <- function(x){
  # input: vector of strings in "hh:mm:ss" format representing duration,
  #   (not time of day)
  # output: vector with duration in minutes, numerical.
  # will return NA if x = ""

  # set up numeric vector to store results
  # (if store them in x they are converted back to character!)
  z <- numeric()

  for (i in 1:length(x)) {
    # Split the string at each ":"
    y <- strsplit(x, ":")
    # # [[1]]
    # # [1] "01" "02" "03"

    # assign the pieces of splitstring to appropriate variables and
    # convert to numeric
    hours <- as.numeric(y[[i]][1])
    minutes <- as.numeric(y[[i]][2])
    seconds <- as.numeric(y[[i]][3])

    #calculate the time in minutes
    z[i] <- ((hours*60) + (minutes) + (seconds/60))
    next
  }
  z
}
# use mins to convert dat$Time to mins
dat$Time <- mins(dat$Time)

#dat[is.na(dat)]=0

#Create names of features for daily data
names.workout=unique(dat$Exercise)
#convert names so can be used as column headings (get rid of spaces, "-", "(", etc)
names.workout=make.names(names.workout, unique = TRUE)
#Concatenate with features of exercises - this has to be done in steps because append(x,y)
#adds y to the end of x and takes no more input. Will include all possible features and remove excess later
#changed names to names.workout.full b/c 'names' is a function and can cause confusion
names.workout.full=append(paste(names.workout,"Weight", sep = "."),paste(names.workout,"Reps", sep = ".")) #will report sqrt of sum of squares
names.workout.full=append(names.workout.full,paste(names.workout,"ctheta_w", sep = ".")) #reports the cosine of the "angle" between reps and weight so "pseudo-work" can be computed
names.workout.full=append(names.workout.full,paste(names.workout,"Distance", sep = "."))
names.workout.full=sort(append(names.workout.full,paste(names.workout, "Time", sep = "."))) #Sort alphabetically because I'm OCD
names.workout.full=append("Day",names.workout.full)
names.workout.full=append(names.workout.full,"Injured") #0=no, 1=yes - always best to make categorical variables numerical if there's a natural mapping to do so
names.workout.full=append(names.workout.full,"Injured.Next.Week") #0=no, 1=yes - this makes it easier to see immediately if a workout may have contributed to injury

#Initialize the new data frame
days=data.frame(matrix(0,nrow=length(entries),ncol=length(names.workout.full)), stringsAsFactors = F)
# my attempt.. not working... there has to be an easier way!
#days <- as.data.frame(setNames(1:length(names.workout.full), names.workout.full), stringsAsFactors = F)

colnames(days)=names.workout.full
days$Day=as.Date(entries)

# all days column types are currently numeric.. double check that won't cause problems with code below

#Now populate each column for a given day and cycle through the days
for(x in seq(nrow(days)))    #Starting with looping over the active days
{       #Now loop over the workouts for that day
  daily.workout=split(split(dat, f = dat$Date)[[x]],split(dat, f = dat$Date)[[x]]$Exercise)
  #   daily.workout is a list of data frames.  each exercise done that day is a data frame
  #   with Date, Exercise, Category, Weight, Reps, Distance.Unit, Time, and Comment
  #   all the exercise names in the daily.workout list and daily.workout$(exercisename w/ spaces, etc)$Exercise still have spaces and punctuation
  for(y in seq(length(daily.workout)))
  {
    #       names.workout previously used for vector of unique values of exercise names - ok to overwrite?
    names.workout=make.names(unique(daily.workout[[y]]$Exercise)) #Get the name of the y-th workout for the x-th day, remove spaces and special chars
    category=unique(daily.workout[[y]]$Category)      #Get the category of the workout

    weight=sqrt(sum(daily.workout[[y]]$Weight..lbs.^2))                                      #Sqrt of sum of squares of weight - NAs will result where weight not entered
    reps=sqrt(sum(daily.workout[[y]]$Rep^2))                                                 #Sqrt of sum of squares of reps
    #       ctheta will produce error if NA in weight or reps... it can't evaluate if first part is true or false if one is NA
    ctheta= if((!is.na(weight) & !is.na(reps)) & (weight>0 & reps>0)){sum(daily.workout[[y]]$Weight..lbs. * daily.workout[[y]]$Reps )  /(weight*reps)}
    else {ctheta=0}                                                                        #Cosine of angle between reps and weight
    #       distance unit - sometimes meters, sometimes miles.
    distance=sqrt(sum(daily.workout[[y]]$Distance^2))                                        #Sqrt of sum of squares of distance
    #       Frequently distance is NA
    if(is.na(distance)){distance=0}
    time=sqrt(sum(as.numeric(daily.workout[[y]]$Time)^2))                                    #Sqrt of sum of squares of time
    #       time frequently NA
    if(is.na(time)){time=0}

    days[x, colnames(days)==paste(names.workout,"Weight", sep = ".")]=weight
    days[x, colnames(days)==paste(names.workout,"Reps", sep = ".")]=reps
    days[x, colnames(days)==paste(names.workout,"ctheta_w", sep = ".")]=ctheta              #For some reason, this keeps returning a dimensionality error...will debug soon
    days[x, colnames(days)==paste(names.workout,"Distance", sep = ".")]=distance
    days[x, colnames(days)==paste(names.workout,"Time", sep = ".")]=time
  }
}









##--------------------------------------THIS SECTION CONTAINS MedHelper/Injury Data-----------------------------------------------------------
#Now, we populate the injured column
#datafile <- file.path(getwd(), "DATA", "MedHelper",
#                      "schedulelog_2016-09-22.csv")
injury=read.csv(file="../Data/MedHelper/schedulelog_2016-09-22.csv", na.strings = "NA",
                stringsAsFactors = F)  #Reading in the file

# only count meds that are for pain.
# note to user - if add more meds in MedHelper this will need to be updated
#Could create a .csv or text file containing the names and read them into this list - might be more user friendly to update
pain.meds <- c("Ibuprofen", "Aleve", "Hydrocodon", "Acetaminophine",
               "Metaxalone", "Asprin", "Tramadol", "Tylenol")  # subset for meds that are for pain


# inspired by http://stackoverflow.com/questions/7597559/grep-in-r-with-a-list-of-patterns
injury <- injury[(grep(paste(pain.meds,collapse="|"),
                       injury$Prescription, ignore.case = TRUE, value = FALSE)), ]

# get rid of records where no medication actually taken
injury <- injury[(injury$Actual.Dosage>0),]
injury <- injury[is.na(injury$Actual.Dosage)== FALSE, ]

# Actual.Time is a character string.  Convert to POSIXlt
# (leaving time for now in case later want to use calcs for # of meds in day, etc)
injury$Actual.Time <- strptime(injury$Actual.Time, format = "%d %b %Y %H:%M")

# Pull unique dates from Actual.Time - dates where pain medication taken
injury.date <- unique(format(injury$Actual.Time, "%Y-%m-%d"))
injury.date=as.Date(injury.date)

#As would be expected, not all injured days are currently recorded (that would mean Becca worked out while injured, which is far less likely)
#So, let's create the rows for the days left out
injury.date.missing=injury.date[!(injury.date %in% days$Day)]
injury.date.missing.frame=data.frame(matrix(0,nrow=length(injury.date.missing),ncol=ncol(days)),stringsAsFactors = F)
colnames(injury.date.missing.frame)=colnames(days)
injury.date.missing.frame$Day=injury.date.missing
injury.date.missing.frame$Injured=1
days=rbind(days,injury.date.missing.frame)

#Actually populating the "Injured" column:
days$Injured[days$Day %in% injury.date] = 1 #At LAST! We know when Becca was injured!
days$Injured[!(days$Day %in% injury.date)] = 0
days=days[order(days$Day),]  #It's nice to order the columns by date - doesn't accomplish anything, but, again, OCD

#Now populate injured.next.week from the results
days$Injured.Next.Week = unlist(lapply(days$Day, function(x) {
  if(1 %in% days$Injured[days$Day %in% seq(from = as.numeric(x) + 1, to = as.numeric(x) + 7)]) {1}
  else {0}
}))

#Make sure these two columns are treated as factors, and not numeric
days$Injured=as.factor(days$Injured)
days$Injured.Next.Week=as.factor(days$Injured.Next.Week)






##---------------------------------------THIS SECTION INCLUDES FITBIT STEP DATA------------------------------------------------
#
#We should also include steps.  From "steps": Total steps, distance, calories burned, calories consumed (>1000, else assume some wasn't logged),
#Time very active.
#From "steps.mins": Max floors, maximum number of steps taken per 15 mins (indicates large amount of activity)

#datafile = file.path(getwd(), "Data", "Fitbit",
#                      "fitbitScraper_daily_20140121-20160921.csv")
steps=read.csv(file="../Data/Fitbit/fitbitScraper_daily_20140121-20160921.csv", stringsAsFactors = F)
steps=steps[,-1]
colnames(steps)=c("Day","Steps.Total","Steps.Distance","Steps.Floors","Steps.Active.Time","Calories.Burned","Calories.Consumed")
steps$Day=as.Date(steps$Day,format="%Y-%m-%d")

#datafile = file.path(getwd(), "Data", "Fitbit",
#                     "fitbitScraper_intraday_20140121-20160921.csv")
steps.mins.raw=read.csv(file="../Data/Fitbit/fitbitScraper_intraday_20140121-20160921.csv", stringsAsFactors = F)        #This data frame isn't usable as is, so we need to break it down into daily data
steps.mins.raw$time=as.Date(steps.mins.raw$time,format="%Y-%m-%d")  #This drops the timestamp pretty simply
steps.mins=data.frame(matrix(nrow=length(steps$Day),ncol=3))        #Initializing the daily data frame
colnames(steps.mins)=c("Day","Steps.Max","Steps.Floors.Max")
steps.mins$Day=steps$Day
#Now populate the daily data frame
#I want to combine the two data lapply functions and just have both lists populated element by element for each x, but for some reason R doesn't like setting variables inside _apply...Mathematica is way better lol
steps.mins$Steps.Max = unlist(lapply(steps.mins$Day, function(x) {
  max(steps.mins.raw$steps[steps.mins.raw$time %in% x])   #For some reason, using == instead of %in% creates nonexistent NA rows
}))
steps.mins$Steps.Floors.Max = unlist(lapply(steps.mins$Day, function(x) {
  max(steps.mins.raw$floors[steps.mins.raw$time %in% x])
}))

#Now we outer join the data frames
steps = merge(x=steps, y=steps.mins, by="Day", all = T)

#Now incorporate this data frame in with the days data frame
days = merge(x=days, y=steps, by="Day", all = T)
days[is.na(days)] = 0









##--------------PCA processing of daily activities as recorded by fitbit-------------------------------------
library(caret)
#I will explain this manipulation and analysis in a non-physics way.  The motivation was inspired by quantum physics
#descriptions of phenomena, but I think there is a more accessible way of explaining it without a lot of terms of art.
#Each day, there are various activities that are engaged in that generate particular signals as measured
#by fitbit (steps). It would be nice to isolate these activities, so that the steps each day are given by
#days.steps=c1*activity1.steps+c2*activity2.steps+... (the coefficients c1, c2, ... simply scale the activity -
#maybe a run is faster on some days for instance, so they need to be scaled).
#It doesn't matter *when* the activity in the day is done, just so that it's done.  So, covariance is a good
#measure for how similar two days are - even if the steps as a function of time of day aren't the same, if the
#distribution of steps is the same, the days are similar.
#A principal component analysis (PCA) on a data frame with 15 mins intervals as rows and days as columns
#can find exactly these activities/canonical distributions of steps throughout the day.
#prcomp(fitbit data,center=F)$rotation gives the set of coefficients mentioned earlier, which provides information
#both about what activities were done and how ham the activity was. Those coefficients then become
#features in the days data frame and will likely be by far the most useful predictors of injury.
#I'm keeping each section relatively self-contained in this script, and would like to use different notation
#for things used early, so please pardon the extra memory use.

fitbit=read.csv(file="../Data/Fitbit/fitbitScraper_intraday_20140121-20160921.csv", stringsAsFactors = F)

times=seq(from=as.POSIXct("00:00:00",format="%H:%M:%S"),to=as.POSIXct("23:45:00",format="%H:%M:%S"),by=15*60)
times=format(times,format="%H:%M:%S")
times.length=length(times)

#this is a bad way to generate the dates, but the NAs in the time column really muck up handling it.
dates=seq.Date(from=as.Date(fitbit$time[1],format="%Y-%m-%d"),to=as.Date(fitbit$time[length(fitbit$time)],format="%Y-%m-%d"),by=1)
dates.length=length(dates)

fitbit$time=rep(times,dates.length)
fitbit$Day=as.Date(unlist(lapply(dates,function(x) rep(x,times.length))),format="%Y-%m-%d",origin="1970-01-01")

activity=as.data.frame(matrix(fitbit$steps,nrow=times.length,ncol=dates.length,byrow=F))
colnames(activity)=dates

activity.pca=prcomp(activity,center=F)
activity.coef=as.data.frame(activity.pca$rotation)
activity.coef$Day=dates

days=merge(x=days,y=activity.coef,by="Day",all=T)

days[is.na(days)]=0

days$Injured=as.factor(days$Injured)
days$Injured.Next.Week=as.factor(days$Injured.Next.Week)

days=days[,colSums(days[,-1] != 0) > 0]  #Trim the fat - we don't care about features that have all 0s

##--------------Predicting state of injury with decision tree-----------------------------------------------------
states.variables=append(c("Day","Injured"),colnames(days)[grep("PC",colnames(days))])
states=days[,colnames(days) %in% states.variables]

states.tracked=states[states$Day >= min(unique(injury.date)),]

training.indices=createDataPartition(states.tracked$Injured,p=0.7,list=F)
training=states.tracked[training.indices,-(colnames(states) %in% "Day")]

mod.injured=train(Injured ~ .,data=training,method="rf")
confusionMatrix(data=predict(mod.injured,newdata=states.tracked[-training.indices,]),reference = states.tracked$Injured[-training.indices])
#Well, that didn't work as well as hoped...let's do some trend detection

##-------------I don't like available 2d histogram and density packages. Let's make heat maps and our own decision tree--------

#Adding an injury column to fitbit for easier manipulation in this section
fitbit$Injured=0 #Initialize to 0 for now
fitbit$Injured[fitbit$Day %in% injury.date] = 1 
fitbit$Injured=as.factor(fitbit$Injured)

#Make a matrix of probabilities
times=unique(fitbit$time)
breaknum=40
stepbreaks=seq(from=0,to=max(fitbit$steps),by=max(fitbit$steps)/breaknum)
stepmids=(stepbreaks[1:(length(stepbreaks)-1)] + stepbreaks[2:length(stepbreaks)])/2

heatmatrix0=matrix(unlist(lapply(times,function(m){
  temp=hist(fitbit$steps[(fitbit$Injured == 0) & (fitbit$Day >= min(injury.date)) & (fitbit$time == m)],breaks=stepbreaks,plot=F)
  temp$counts/sum(temp$counts)
})),ncol=length(times))

heatmatrix1=matrix(unlist(lapply(times,function(m){
  temp=hist(fitbit$steps[(fitbit$Injured == 1) & (fitbit$Day >= min(injury.date)) & (fitbit$time == m)],breaks=stepbreaks,plot=F)
  temp$counts/sum(temp$counts)
})),ncol=length(times))

heatmatrix.all=matrix(unlist(lapply(times,function(m){
  temp=hist(fitbit$steps[(fitbit$Day >= min(injury.date)) & (fitbit$time == m)],breaks=stepbreaks,plot=F)
  temp$counts/sum(temp$counts)
})),ncol=length(times))

#Plotting the probability (density)
par(mfrow=c(2,1))
image(x=times,y=stepmids,z=t(heatmatrix0),xlab="Time",ylab="Steps",main="Step probability when healthy",col=topo.colors(length(unique(fitbit$steps))))
image(x=times,y=stepmids,z=t(heatmatrix1),xlab="Time",ylab="Steps",main="Step probability when injured",col=topo.colors(length(unique(fitbit$steps))))

#Let's determine the relative probabilities of a particular day being injured vs healthy
#We'll need to get all Bayes up in here: P(A|B)=P(B|A)*P(A)/P(B)

#Probability of being injured
p.injured=length(which(fitbit$Injured[fitbit$Day>=min(injury.date)]==1))/length(fitbit$Injured[fitbit$Day>=min(injury.date)])

injured.pred=unlist(lapply(unique(fitbit$Day),function(w){
#Probability of having those steps given being injured
p1=prod(unlist(lapply(lapply(1:length(times),function(z){
  c(z, .bincode(x=fitbit$steps[fitbit$Day == w][z],breaks=stepbreaks,right=F))
  }),function(y){
    heatmatrix1[y[2],y[1]]
    })))

#Probability of having those steps given being healthy
p0=prod(unlist(lapply(lapply(1:length(times),function(z){
  c(z, .bincode(x=fitbit$steps[fitbit$Day == w][z],breaks=stepbreaks,right=F))
}),function(y){
  heatmatrix0[y[2],y[1]]
})))

#Ratio of probabilityes of being injured given those steps to being healthy
p1*p.injured/(p0*(1-p.injured))
}))
injured.pred[injured.pred==Inf | is.na(injured.pred)]=0

#Now, let's predict which days are injured
fitbit$Predicted=0
fitbit$Predicted[fitbit$Day %in% unique(fitbit$Day)[which(injured.pred>1)]]=1
fitbit$Predicted=as.factor(fitbit$Predicted)
#And now test the prediction
confusionMatrix(fitbit$Predicted[fitbit$Day>=min(injury.date)],fitbit$Injured[fitbit$Day>=min(injury.date)])

#This does pretty well, if I do say so myself!  Much better than the previous classifiers I tried implementing.
#But, maybe this can be improved.  Assuming the probabilities at t_i depend on the previous steps taken, maybe we can get
#a better estimate of the probability of some step sequence throughout the day
#For now, let's just assume a modified Markov process since I'm not sure there's enough data to populate a histogram with too
#many prior conditionals in any statistically meaningful way right now. 





##-------------Improving the probability estimates using modified Markov Chains/Mixture Models--------
#At each point in time, we need to compute a transition matrix from steps binned by activity level to breaknum of bins.
times=unique(fitbit$time)
breaknum=40
stepbreaks=seq(from=0,to=max(fitbit$steps),by=max(fitbit$steps)/breaknum)
stepmids=(stepbreaks[1:(length(stepbreaks)-1)] + stepbreaks[2:length(stepbreaks)])/2

#A list over time in which each element is a list over activity levels in which each element is the probability of some number
#of steps at time t given the activity level of the previous time for uninjured days
trans.mat0=lapply(2:length(times),function(m){
  lapply(unique(fitbit$activityLevel),function(n){
    temp=hist(fitbit$steps[(fitbit$Injured == 0) & (fitbit$Day >= min(injury.date)) & (fitbit$time == times[m]) & (fitbit$activityLevel[fitbit$time == times[m-1]]==n)],breaks=stepbreaks,plot=F)
    temp2=temp$counts/sum(temp$counts)
    temp2[is.na(temp2)]=0
    temp2
  })
})
#Same as before for injured days
trans.mat1=lapply(2:length(times),function(m){
  lapply(unique(fitbit$activityLevel),function(n){
    temp=hist(fitbit$steps[(fitbit$Injured == 1) & (fitbit$Day >= min(injury.date)) & (fitbit$time == times[m]) & (fitbit$activityLevel[fitbit$time == times[m-1]]==n)],breaks=stepbreaks,plot=F)
    temp2=temp$counts/sum(temp$counts)
    temp2[is.na(temp2)]=0
    temp2
  })
})
#Total probability not considering injury status
trans.mat=lapply(2:length(times),function(m){
  lapply(unique(fitbit$activityLevel),function(n){
    temp=hist(fitbit$steps[ (fitbit$Day >= min(injury.date)) & (fitbit$time == times[m]) & (fitbit$activityLevel[fitbit$time == times[m-1]]==n)],breaks=stepbreaks,plot=F)
    temp2=temp$counts/sum(temp$counts)
    temp2[is.na(temp2)]=0
    temp2
  })
})

#Probability distribution over steps for the initial time being injured
temp=hist(fitbit$steps[(fitbit$Day >= min(injury.date)) & (fitbit$time == times[1]) & (fitbit$Injured == 0)],breaks=stepbreaks,plot=F)
init.prob0=temp$counts/sum(temp$counts)
#"" uninjured
temp=hist(fitbit$steps[(fitbit$Day >= min(injury.date)) & (fitbit$time == times[1]) & (fitbit$Injured == 1)],breaks=stepbreaks,plot=F)
init.prob1=temp$counts/sum(temp$counts)

temp=hist(fitbit$steps[(fitbit$Day >= min(injury.date)) & (fitbit$time == times[1])],breaks=stepbreaks,plot=F)
init.prob=temp$counts/sum(temp$counts)

#The set of activity levels
act.l=unique(fitbit$activityLevel)

#The probability for each day of that activity level being maintained if uninjured
prob0=unlist(lapply(unique(fitbit$Day),function(day){
q=unlist(lapply(fitbit$activityLevel[fitbit$Day==day],function(x){
  which(act.l==x)
}))
prod(unlist(lapply(1:length(times),function(t){
  if(t==1){init.prob0[.bincode(fitbit$steps[fitbit$Day==day][t],breaks=stepbreaks,right=F)]}
  else{trans.mat0[[t-1]][[q[t-1]]][.bincode(fitbit$steps[fitbit$Day==day][t],breaks=stepbreaks,right=F)]}
})))
}))
#"" if injured
prob1=unlist(lapply(unique(fitbit$Day),function(day){
  q=unlist(lapply(fitbit$activityLevel[fitbit$Day==day],function(x){
    which(act.l==x)
  }))
  prod(unlist(lapply(1:length(times),function(t){
    if(t==1){init.prob1[.bincode(fitbit$steps[fitbit$Day==day][t],breaks=stepbreaks,right=F)]}
    else{trans.mat1[[t-1]][[q[t-1]]][.bincode(fitbit$steps[fitbit$Day==day][t],breaks=stepbreaks,right=F)]}
  })))
}))

#Probability of being injured
p.injured=length(which(fitbit$Injured[fitbit$Day>=min(injury.date)]==1))/length(fitbit$Injured[fitbit$Day>=min(injury.date)])

injured.pred2=prob1*p.injured/(prob1*(1-p.injured))
injured.pred2[injured.pred2 == Inf | is.na(injured.pred2)]=0

fitbit$Predicted2=0
fitbit$Predicted2[fitbit$Day %in% unique(fitbit$Day)[which(injured.pred>1)]]=1
fitbit$Predicted2=as.factor(fitbit$Predicted2)

confusionMatrix(fitbit$Predicted2[fitbit$Day>=min(injury.date)],fitbit$Injured[fitbit$Day>=min(injury.date)])