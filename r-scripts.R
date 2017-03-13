#==========================================================================
#==========================================================================
#==========================================================================
#Descriptive statistics for Categorical Data
#==========================================================================


#Using this data set unless mentioned otherwise

x <− c( "A" , "A" , "K" , "U" , "K" , "K" , "A" , "U" , "A" , "K" , "A" ,
"K" , "K" , "A" , "U" , "A" , "U" , "K" , "K" , "A" , "A" , "A" ,
"U" , "K" , "A")

#==========================================================================
#Building a contingency table of frequencies/counts.

table(x)

#==========================================================================
#Return the length of a column (no. of variables)
length(column)

#==========================================================================
#Relative frequency

table(x)/length(x) #Length returns the length of the vector

sum(survey$Sex=="Male" & survey$W.Hand=="Left", na.rm=TRUE)/length(survey$Sex=="Male")

#==========================================================================
#Mode

data <- table(x)
names(data)[data == max(data)] #Name(s) of data where data is max

#==========================================================================
#Barcharts

#Standard
barplot(table(x)) 

#Save graph as png
png(filename="barchart_standard.png") 
barplot(table(x))
dev.off()

#Save graph as pdf
barplot(table(x))
dev.print(pdf, 'barchart_standard.pdf')

#Pimping it up!

colors <- c("thistle", "powderblue", "khaki1", " wheat", "lightgreen", "plum2", "paleturquoise", "tan")

freq <- table(x)
barplot(freq, main="Title of barplot", 
		xlim=c(0,50), ylim=c(0,30),
		xlab="X values", ylab="Y values",
		col=colors, border="red")
legend("topright", names(freq), fill=colors)
#Parameter "density = c(90,50,10) can also be added"

#==========================================================================
#Pie Charts

colors <- c("thistle", "powderblue", "khaki1", " wheat", "lightgreen", "plum2", "paleturquoise", "tan")

freq <- table(x)
labels <- paste(names(freq), " ", "\n", freq, sep=" ")

pie(freq, main="My pie chart title", labels=labels, col=colors)

legend("topright", labels, fill=colors)

#==========================================================================
#Omit missing values from data, where survey is the dataset

newData <- na.omit(survey)

#==========================================================================
#Extracting a column from a dataset

survey$Sex

survey$Sex[survey$Sex == 'Male'] #Filtering column

#==========================================================================
#Like table command, but includes missing values

summary(survey$Sex)

#==========================================================================
#Count number of missing values

sum(survey$Sex) #Used to get the sum or number of elements in column

sum(is.na(survey$Sex)) #Count the number of missing values

#==========================================================================
#Print format

sprintf("Text %d text", 12)

#==========================================================================
#Relationship between two categorical data

tmp <- na.omit(survey[c("Sex", "W.Hnd")])
freq <- table(tmp$Sex, tmp$W.Hnd)

barplot(freq, main="title", col=colors)

#==========================================================================
#==========================================================================
#==========================================================================
#Graphical methods for Quantitative Data
#==========================================================================

#Generate a sequence of values

seq(15, 75, by=5)

#==========================================================================
#Divide variable into interval or classes

breaks <- seq(15,75, by=5)
cut(x, breaks, right=FALSE) #right=FALSE allows for semi-open [....)

#==========================================================================
#Combines vector, matrix or data frame by column into a nice table

cbind(x.freq)

#==========================================================================
#Histogram

breaks = seq(15,75,by=20)

hist(survey$Age, right=FALSE, col=colors, breaks=breaks,
	 main="Histogram title", 
	 xlim=c(0,40), ylim=c(0,35),
	 xlab="Age", ylab="Frequency")

#==========================================================================
#Filtering data using two different methods

pulseRight <- na.omit(survey$Pulse[survey$W.Hnd == 'Right'])
pulseLeft <- na.omit(survey$Pulse[survey$W.Hnd == 'Left'])

#OR

tmp <- na.omit(survey[c("Pulse", "W.Hnd")])
pulseRight <- subset(tmp, W.Hnd == 'Right')$Pulse
pulseLeft <- subset(tmp, W.Hnd == 'Left')$Pulse

#==========================================================================
#Obtaining range (max,min) of a dataset

range(x)

#==========================================================================
#Plotting cumulative frequency using an ogive

x <- na.omit(survey$Age)

breaks <- seq(15,75,by=5)
x.cut <- cut(x, breaks, right=FALSE)
x.freq <- table(x.cut)

x.cumfreq <- c(0, cumsum(x.freq))

cbind(breaks, x.cumfreq) #Shows summary

#The ogive itself..

plot(breaks, x.cumfreq, main="Ogive title",
	 xlab="Age", ylab="Cumulative Frequency")
lines(breaks, x.cumfreq) #Draw lines through the points
grid() #Add small grids to the graph

#For cumulative relative frequency, divide the cumulative frequency by the number/length

x.cumrelfreq <- x.cumfreq / length(x)

#==========================================================================
#==========================================================================
#==========================================================================
#Numerical methods for Quantitative Data
#==========================================================================

#Sum of x over n observations, where x = x1,x2,...,xn

ans <- sum(x)

#==========================================================================
#Mean of data

mean(x)

mean(survey$Age)
mean(survey$Age[survey$Age<70])
mean(survey$Age, trim=0.1)

#==========================================================================
#Median

median(x)
median(survey$Age[survey$Age<70])

#==========================================================================
#Variance

var(survey$Age, na.rm=TRUE) #na.rm used to drop missing values when set to true

#==========================================================================
#Standard Deviation

sd(surey$Age)

#==========================================================================
#==========================================================================
#==========================================================================
#Lab material
#==========================================================================

#How many missing values are in variable representing preferred writing hand?
sum(is.na(survey$W.Hnd))

#How many students prefer that their left hand is on top when clapping?
sum(survey$Clap=="Left", na.rm=TRUE)

#Code to calculate number of students that do not have a preference regarding which hand is on top when clapping or did not answer this question?
sum(survey$Clap=='Neither') + sum(is.na(survey$Fold))

#What percentage of students (excluding missing values) are left handed (answer to 2 decimal places)?
freq <- table(survey$W.Hnd)
sprintf("%.2f%%", 100*freq['Left'] / sum(freq))

#How many students are left handed and their left hand is the preferred hand on top when folding?
table(survey$W.Hnd, survey$Fold)
sum(survey$W.Hnd=="Left" & survey$Fold=="L on R", na.rm=TRUE)

#What percentage of students who prefer their right hand on top when folding are also right handed (answer to 2 decimal places)?
table(survey$W.Hnd, survey$Fold)

#Function to get the mean/median etc of a group of data together
tapply(CO2$uptake, CO2$Type, mean)

#Using relative frequency to get probability (Student survey dataset)
sum(survey$Age<=18 & survey$Sex=="Male", na.rm=TRUE)/length(na.omit(survey$W.Hnd))

#Creating sample data
sample(1:6, 10, replace = T)
#sample(outcomes where 1,2,..,6 rolls of a die, 10 times roll, replace = True)













