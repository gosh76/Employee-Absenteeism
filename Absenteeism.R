# Reading data

absent = read.csv('F:/EdwisorProject2/Absenteeism_at_work_Project.csv')

str(absent)

# 'data.frame':	740 obs. of  21 variables:
# $ ID                             : int  11 36 3 7 11 3 10 20 14 1 ...
# $ Reason.for.absence             : int  26 0 23 7 23 23 22 23 19 22 ...
# $ Month.of.absence               : int  7 7 7 7 7 7 7 7 7 7 ...
# $ Day.of.the.week                : int  3 3 4 5 5 6 6 6 2 2 ...
# $ Seasons                        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Transportation.expense         : int  289 118 179 279 289 179 NA 260 155 235 ...
# $ Distance.from.Residence.to.Work: int  36 13 51 5 36 51 52 50 12 11 ...
# $ Service.time                   : int  13 18 18 14 13 18 3 11 14 14 ...
# $ Age                            : int  33 50 38 39 33 38 28 36 34 37 ...
# $ Work.load.Average.day          : Factor w/ 39 levels "","205,917","222,196",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ Hit.target                     : int  97 97 97 97 97 97 97 97 97 97 ...
# $ Disciplinary.failure           : int  0 1 0 0 0 0 0 0 0 0 ...
# $ Education                      : int  1 1 1 1 1 1 1 1 1 3 ...
# $ Son                            : int  2 1 0 2 2 0 1 4 2 1 ...
# $ Social.drinker                 : int  1 1 1 1 1 1 1 1 1 0 ...
# $ Social.smoker                  : int  0 0 0 1 0 0 0 0 0 0 ...
# $ Pet                            : int  1 0 0 0 1 0 4 0 0 1 ...
# $ Weight                         : int  90 98 89 68 90 89 80 65 95 88 ...
# $ Height                         : int  172 178 170 168 172 170 172 168 196 172 ...
# $ Body.mass.index                : int  30 31 31 24 30 31 27 23 25 29 ...
# $ Absenteeism.time.in.hours      : int  4 0 2 4 2 NA 8 4 40 8 ...

# Work.load.Average.day variable values have commas which need to be removed.

absent$Work.load.Average.day = gsub(',','',absent$Work.load.Average.day)

head(absent$Work.load.Average.day)

# [1] "239554" "239554" "239554" "239554" "239554" "239554"

# So, commas have been removed.

# Missing Values Analysis

as.data.frame(colSums(is.na(absent)))

# Missing values are:

# ID                                                   0
# Reason.for.absence                                   3
# Month.of.absence                                     1
# Day.of.the.week                                      0
# Seasons                                              0
# Transportation.expense                               7
# Distance.from.Residence.to.Work                      3
# Service.time                                         3
# Age                                                  3
# Work.load.Average.day                                0
# Hit.target                                           6
# Disciplinary.failure                                 6
# Education                                           10
# Son                                                  6
# Social.drinker                                       3
# Social.smoker                                        4
# Pet                                                  2
# Weight                                               1
# Height                                              14
# Body.mass.index                                     31
# Absenteeism.time.in.hours                           22

#plotting boxplot of Reason.for.absence Vs. Absenteeism.time.in.hours

library(ggplot2)

ggplot(absent,aes_string(y=absent$Absenteeism.time.in.hours,x=as.factor(absent$Reason.for.absence)))+geom_boxplot()+xlab('Reason.for.absence')+ylab('Absenteeism.time.in.hours')

# Category 27 of Reason.for.absence is taking < 10 hrs of Absenteeism.time.in.hours. So, null values of Reason.for.absence are put equal to 27 since Absenteeism.time.in.hours for the observations having null values is < 10 hrs.

absent$Reason.for.absence[is.na(absent$Reason.for.absence)] = 27

#Zero category of 'Reason for absence' column has been put equal to category 26(i.e. unjustified absence).

absent$Reason.for.absence[absent$Reason.for.absence==0] = 26

as.data.frame(colSums(is.na(absent)))

#Putting Month.of.absence null value equal to 10.

absent$Month.of.absence[is.na(absent$Month.of.absence)] = 10

#Finding ID column values for which there are missing values in Transportation.expense

absent$ID[is.na(absent$Transportation.expense)]

# [1] 10  3  1 15 20 22 20

for (i in c(1,3,10,15,20,22)){
  absent$Transportation.expense[is.na(absent$Transportation.expense) & absent$ID==i] = mean(absent$Transportation.expense[absent$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(absent)))

#Finding ID column values for which there are missing values in Distance.from.Residence.to.Work 

absent$ID[is.na(absent$Distance.from.Residence.to.Work)]

# [1] 34 22 28

for (i in c(34,22,28)){
  absent$Distance.from.Residence.to.Work[is.na(absent$Distance.from.Residence.to.Work) & absent$ID==i] = mean(absent$Distance.from.Residence.to.Work[absent$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(absent)))

#Finding ID column values for which there are missing values in Service.time

absent$ID[is.na(absent$Service.time)]

# [1] 28 34 34

for (i in c(34,28)){
  absent$Service.time[is.na(absent$Service.time) & absent$ID==i] = mean(absent$Service.time[absent$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(absent)))

#Finding ID column values for which there are missing values in Age

absent$ID[is.na(absent$Age)]

# [1] 28 24 24

for (i in c(24,28)){
  absent$Age[is.na(absent$Age) & absent$ID==i] = mean(absent$Age[absent$ID==i],na.rm = T)
}

as.data.frame(colSums(is.na(absent)))

absent$Work.load.Average.day = as.numeric(absent$Work.load.Average.day)

#Work.load.Average.day missing values are imputed using Month.of.absence and Hit.target

absent$Month.of.absence[is.na(absent$Work.load.Average.day)]

# [1]  9 10 11 11 12 12  1  1  1  5

absent$Hit.target[is.na(absent$Work.load.Average.day)]

# [1] 92 93 93 93 97 97 95 95 95 92

#making a dataframe having above two series

df = data.frame(m=c(9,10,11,11,12,12,1,1,1,5),h=c(92,93,93,93,97,97,95,95,95,92))


for (i in 1:10){
  absent$Work.load.Average.day[(is.na(absent$Work.load.Average.day) & absent$Month.of.absence==df[i,1]) & absent$Hit.target==df[i,2]] = mean(absent$Work.load.Average.day[absent$Month.of.absence==df[i,1] & absent$Hit.target==df[i,2]],na.rm = T)
}

as.data.frame(colSums(is.na(absent)))

#Hit.target missing values are imputed using Month.of.absence and Work.load.Average.day

absent$Month.of.absence[is.na(absent$Hit.target)]

# [1] 11 11 12  1  1  1

absent$Work.load.Average.day[is.na(absent$Hit.target)]

# [1] 306345 306345 261306 308593 308593 308593

df1 = data.frame(m1=c(11,12,1),w1=c(306345,261306,308593))

for (i in 1:3){
  absent$Hit.target[(is.na(absent$Hit.target) & absent$Month.of.absence==df1[i,1]) & absent$Work.load.Average.day==df1[i,2]] = mean(absent$Hit.target[absent$Month.of.absence==df1[i,1] & absent$Work.load.Average.day==df1[i,2]],na.rm = T)
}

as.data.frame(colSums(is.na(absent)))

#Disciplinary.failure missing values put to 0

absent$Disciplinary.failure[is.na(absent$Disciplinary.failure)] = 0

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Education

absent$ID[is.na(absent$Education)]

# [1] 11 10 34 34 14 34 34 34 10 24

for (i in c(10,11,14,24,34)){
  absent$Education[is.na(absent$Education) & absent$ID==i] = mean(absent$Education[absent$ID==i],na.rm=T)
 }

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Son

absent$ID[is.na(absent$Son)]

# [1] 20 14 34 34 27  1

for (i in c(1,14,20,27,34)){
  absent$Son[is.na(absent$Son) & absent$ID==i] = mean(absent$Son[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Social.drinker

absent$ID[is.na(absent$Social.drinker)]

# [1] 10 14 17

for (i in c(10,14,17)){
  absent$Social.drinker[is.na(absent$Social.drinker) & absent$ID==i] = mean(absent$Social.drinker[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Social.smoker

absent$ID[is.na(absent$Social.smoker)]

# [1] 34  1 11 15

for (i in c(34,1,11,15)){
  absent$Social.smoker[is.na(absent$Social.smoker) & absent$ID==i] = mean(absent$Social.smoker[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Pet

absent$ID[is.na(absent$Pet)]

# [1] 1 13

for (i in c(1,13)){
  absent$Pet[is.na(absent$Pet) & absent$ID==i] = mean(absent$Pet[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Weight

absent$ID[is.na(absent$Weight)]

# [1] 27

for (i in c(27)){
  absent$Weight[is.na(absent$Weight) & absent$ID==i] = mean(absent$Weight[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Height

absent$ID[is.na(absent$Height)]

# [1] 20 10 28 34 34 27 10 11  5 22 13 24 32 28

for (i in c(20,10,28,34,27,11,5,22,13,24,32)){
  absent$Height[is.na(absent$Height) & absent$ID==i] = mean(absent$Height[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#ID column has been used to impute missing value for Body.mass.index 

absent$ID[is.na(absent$Body.mass.index)]

# [1]  3 24 11 30  2 19 34  3 28 34 28  3 13 36 11 14 34 36 28 20 28 28 18 28 17 15 20 22 24 11  5

for (i in c(3,24,11,30,2,19,34,28,13,36,14,20,18,17,15,22,5)){
  absent$Body.mass.index[is.na(absent$Body.mass.index) & absent$ID==i] = mean(absent$Body.mass.index[absent$ID==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

#Reason.for.absence column has been used to impute missing value for Absenteeism.time.in.hours 

absent$Reason.for.absence[is.na(absent$Absenteeism.time.in.hours)]

# [1]  23 14 10 22 26 26 23 26 26 22 10  6 28 26 11 22 22 26 26 23 26 13

for (i in c(23,14,10,22,26,6,28,11,13)){
  absent$Absenteeism.time.in.hours[is.na(absent$Absenteeism.time.in.hours) & absent$Reason.for.absence==i] = mean(absent$Absenteeism.time.in.hours[absent$Reason.for.absence==i],na.rm=T)
}

as.data.frame(colSums(is.na(absent)))

# All missing values have been imputed.

# ID                                                   0
# Reason.for.absence                                   0
# Month.of.absence                                     0
# Day.of.the.week                                      0
# Seasons                                              0
# Transportation.expense                               0
# Distance.from.Residence.to.Work                      0
# Service.time                                         0
# Age                                                  0
# Work.load.Average.day                                0
# Hit.target                                           0
# Disciplinary.failure                                 0
# Education                                            0
# Son                                                  0
# Social.drinker                                       0
# Social.smoker                                        0
# Pet                                                  0
# Weight                                               0
# Height                                               0
# Body.mass.index                                      0
# Absenteeism.time.in.hours                            0

# Continuous Variables Distributions

#Transportation.expense 

hist(absent$Transportation.expense,prob = TRUE,xlab = 'Transportation.expense')
lines(density(absent$Transportation.expense))

#Distance.from.Residence.to.Work

hist(absent$Distance.from.Residence.to.Work,prob = TRUE,xlab = 'Distance.from.Residence.to.Work')
lines(density(absent$Distance.from.Residence.to.Work))

#Service.time

hist(absent$Service.time,prob = TRUE,xlab = 'Service.time')
lines(density(absent$Service.time))

#Age

hist(absent$Age,prob = TRUE,xlab = 'Age')
lines(density(absent$Age))

#Work.load.Average.day

hist(absent$Work.load.Average.day,prob = TRUE,xlab = 'Work.load.Average.day')
lines(density(absent$Work.load.Average.day))

#Hit.target

hist(absent$Hit.target,prob = TRUE,xlab = 'Hit.target')
lines(density(absent$Hit.target))

#Weight

hist(absent$Weight,prob = TRUE,xlab = 'Weight')
lines(density(absent$Weight))

#Height

hist(absent$Height,prob = TRUE,xlab = 'Height')
lines(density(absent$Height))

#Body.mass.index

hist(absent$Body.mass.index,prob = TRUE,xlab = 'Body.mass.index')
lines(density(absent$Body.mass.index))

#All continuous variables have skewed distribution.

#Outlier Analysis

#boxplot for Transportation.expense, Distance.from.Residence.to.Work, Service.time, Age, Hit.target

boxplot(absent[,c('Transportation.expense','Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target')])

#boxplot for Weight,Height,Body.mass.index,Absenteeism.time.in.hours

boxplot(absent[,c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours')])

#boxplot for Work.load.Average.day 

boxplot(absent[,c('Work.load.Average.day')])

#Capping outliers

for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  q = quantile(absent[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  absent[,i][absent[,i]<min1] = min1
  absent[,i][absent[,i]>max1] = max1
}

#Checking for outliers again

boxplot(absent[,c('Transportation.expense','Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target')])

boxplot(absent[,c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours')])

boxplot(absent[,c('Work.load.Average.day')])

#Correlation Analysis

#Converting catcols to factor as they are categorical

catcols = c('Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')

for (i in catcols){
  absent[,i] = as.factor(absent[,i])
}

str(absent)

# 'data.frame':	740 obs. of  21 variables:
#   $ ID                             : int  11 36 3 7 11 3 10 20 14 1 ...
# $ Reason.for.absence             : Factor w/ 27 levels "1","2","3","4",..: 25 25 22 7 22 22 21 22 19 21 ...
# $ Month.of.absence               : Factor w/ 13 levels "0","1","2","3",..: 8 8 8 8 8 8 8 8 8 8 ...
# $ Day.of.the.week                : Factor w/ 5 levels "2","3","4","5",..: 2 2 3 4 4 5 5 5 1 1 ...
# $ Seasons                        : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
# $ Transportation.expense         : num  289 118 179 279 289 179 361 260 155 235 ...
# $ Distance.from.Residence.to.Work: num  36 13 51 5 36 51 52 50 12 11 ...
# $ Service.time                   : num  13 18 18 14 13 18 3 11 14 14 ...
# $ Age                            : num  33 50 38 39 33 38 28 36 34 37 ...
# $ Work.load.Average.day          : num  239554 239554 239554 239554 239554 ...
# $ Hit.target                     : num  97 97 97 97 97 97 97 97 97 97 ...
# $ Disciplinary.failure           : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 1 1 1 1 ...
# $ Education                      : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 3 ...
# $ Son                            : Factor w/ 5 levels "0","1","2","3",..: 3 2 1 3 3 1 2 5 3 2 ...
# $ Social.drinker                 : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 1 ...
# $ Social.smoker                  : Factor w/ 2 levels "0","1": 1 1 1 2 1 1 1 1 1 1 ...
# $ Pet                            : Factor w/ 6 levels "0","1","2","4",..: 2 1 1 1 2 1 4 1 1 2 ...
# $ Weight                         : num  90 98 89 68 90 89 80 65 95 88 ...
# $ Height                         : num  172 176 170 168 172 ...
# $ Body.mass.index                : num  30 31 31 24 30 31 27 23 25 29 ...
# $ Absenteeism.time.in.hours      : num  4 0 2 4 2 ...

# Chi-square test for correlation between factors

pval = c()

#Calculating & storing p-values in vector pval from chisquare test

for(i in catcols){
  for(j in catcols){
    chi2 = chisq.test(absent[,i],absent[,j])
    pval = c(pval,chi2$p.value)
  }
}

length(pval)#100

#converting pval to matrix m1

m1 = matrix(pval,ncol=10)
m1

#Converting m1 to dataframe chi_df

chi_df = data.frame(m1)

#Setting row names to catcols

row.names(chi_df) = catcols

#Setting column names to catcols

colnames(chi_df) = catcols

chi_df

#p-values are <0.05 for chi-square test of all categorical variables with Reason.for.absence except Day.of.the.week.
#This means that categorical variables having p-values<0.05 have dependence on Reason.for.absence.
#So, all categorical variables except Reason.for.absence and Day.of.the.week will be dropped.

absent[,c('Month.of.absence','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')] = list(NULL)

#Correlation between continuous independent variables

cor(absent[,4:13])

#                                 Transportation.expense Distance.from.Residence.to.Work Service.time          Age
# Transportation.expense                     1.000000000                     0.262824738  -0.35863815 -0.234662747
# Distance.from.Residence.to.Work            0.262824738                     1.000000000   0.12868741 -0.141167058
# Service.time                              -0.358638154                     0.128687405   1.00000000  0.682015793
# Age                                       -0.234662747                    -0.141167058   0.68201579  1.000000000
# Work.load.Average.day                     -0.003327286                    -0.076821665  -0.01547591 -0.048468759
# Hit.target                                -0.082950500                     0.002352948   0.01524642 -0.025772801
# Weight                                    -0.207541855                    -0.047859094   0.45340789  0.436831191
# Height                                    -0.153731047                    -0.333209857  -0.08885700  0.007841315
# Body.mass.index                           -0.136401667                     0.113771638   0.49803269  0.490009823
# Absenteeism.time.in.hours                  0.146104343                    -0.044854777  -0.03232262 -0.028932356
#                                 Work.load.Average.day   Hit.target        Weight       Height Body.mass.index
# Transportation.expense                   -0.003327286 -0.082950500 -0.2075418555 -0.153731047     -0.13640167
# Distance.from.Residence.to.Work          -0.076821665  0.002352948 -0.0478590935 -0.333209857      0.11377164
# Service.time                             -0.015475905  0.015246418  0.4534078882 -0.088857004      0.49803269
# Age                                      -0.048468759 -0.025772801  0.4368311908  0.007841315      0.49000982
# Work.load.Average.day                     1.000000000 -0.060163508 -0.0524068352  0.026593404     -0.10000231
# Hit.target                               -0.060163508  1.000000000 -0.0312133528  0.085830102     -0.07273108
# Weight                                   -0.052406835 -0.031213353  1.0000000000  0.263057442      0.90411690
# Height                                    0.026593404  0.085830102  0.2630574419  1.000000000     -0.11015602
# Body.mass.index                          -0.100002308 -0.072731078  0.9041169006 -0.110156024      1.00000000
# Absenteeism.time.in.hours                 0.097056235  0.015652302 -0.0001233551  0.087612481     -0.05710553
#                                 Absenteeism.time.in.hours
# Transportation.expense                       0.1461043432
# Distance.from.Residence.to.Work             -0.0448547773
# Service.time                                -0.0323226201
# Age                                         -0.0289323558
# Work.load.Average.day                        0.0970562348
# Hit.target                                   0.0156523017
# Weight                                      -0.0001233551
# Height                                       0.0876124811
# Body.mass.index                             -0.0571055317
# Absenteeism.time.in.hours                    1.0000000000

# Correlation amongst continuous independent variables < 0.95
# Correlation between every independent variable & dependent variable < 0.2
# This means that there is no relationship between any independent variable and dependent variable.

# Relationship between Reason.for.absence and Absenteeism.time.in.hours

# Aggregating Absenteeism.time.in.hours by Reason.for.absence

Reasons = aggregate(absent$Absenteeism.time.in.hours, by=list(Category=absent$Reason.for.absence), FUN=sum)
Reasons

#   Category        x
# 1         1 135.0000
# 2         2  17.0000
# 3         3   8.0000
# 4         4   9.0000
# 5         5  19.0000
# 6         6  77.0000
# 7         7 113.0000
# 8         8  32.0000
# 9         9  58.0000
# 10       10 222.6087
# 11       11 161.0800
# 12       12  78.0000
# 13       13 485.0000
# 14       14 129.8333
# 15       15  16.0000
# 16       16   6.0000
# 17       17   8.0000
# 18       18 154.0000
# 19       19 403.0000
# 20       21  35.0000
# 21       22 283.6667
# 22       23 425.5685
# 23       24  24.0000
# 24       25 108.0000
# 25       26 290.4118
# 26       27 169.0000
# 27       28 324.0092


#Calculating absenteeism time by category as percent of total time in column Absence

Reasons$Absence = (Reasons$x/sum(absent$Absenteeism.time.in.hours))*100

Reasons = Reasons[order(Reasons$Absence),]

Reasons

#    Category        x    Absence
# 16       16   6.0000  0.1582621
# 3         3   8.0000  0.2110162
# 17       17   8.0000  0.2110162
# 4         4   9.0000  0.2373932
# 15       15  16.0000  0.4220324
# 2         2  17.0000  0.4484094
# 5         5  19.0000  0.5011635
# 23       24  24.0000  0.6330486
# 8         8  32.0000  0.8440648
# 20       21  35.0000  0.9231959
# 9         9  58.0000  1.5298674
# 6         6  77.0000  2.0310309
# 12       12  78.0000  2.0574079
# 24       25 108.0000  2.8487187
# 7         7 113.0000  2.9806038
# 14       14 129.8333  3.4246171
# 1         1 135.0000  3.5608984
# 18       18 154.0000  4.0620618
# 11       11 161.0800  4.2488112
# 26       27 169.0000  4.4577172
# 10       10 222.6087  5.8717551
# 21       22 283.6667  7.4822827
# 25       26 290.4118  7.6601984
# 27       28 324.0092  8.5463981
# 19       19 403.0000 10.6299410
# 22       23 425.5685 11.2252308
# 13       13 485.0000 12.7928571

barplot(Reasons$Absence,names.arg=Reasons$Category,xlab="Reason.for.absence",ylab="Absence",col="blue")


#Top 3 categories in terms of Absence time are:
# 1. Category-13:Diseases of the musculoskeletal system and connective tissue - 12.79 % of total time
# 2. Category-23:medical consultation - 11.22 % of total time
# 3. Category-19:Injury, poisoning and certain other consequences of external causes - 10.63 % of total time
# 4. Category 28:dental consultation - 8.54 % 0f total time
# 5. Category 26:unjustified absence - 7.66 % of total time

#Forecasting absenteeism time in hours per month for 2011



#Reading original data again

absent1 = read.csv('F:/EdwisorProject2/Absenteeism_at_work_Project.csv')

#Imputing missing values for Month.of.absence,Reason.for.absence,Absenteeism.time.in.hours

absent1$Month.of.absence[is.na(absent1$Month.of.absence)] = 10

absent1$Reason.for.absence[is.na(absent1$Reason.for.absence)] = 27

absent1$Reason.for.absence[absent1$Reason.for.absence==0] = 26

for (i in c(23,14,10,22,26,6,28,11,13)){
  absent1$Absenteeism.time.in.hours[is.na(absent1$Absenteeism.time.in.hours) & absent1$Reason.for.absence==i] = median(absent1$Absenteeism.time.in.hours[absent1$Reason.for.absence==i],na.rm=T)
}

#Converting Month.of.absence to factor

absent1$Month.of.absence = as.factor(absent1$Month.of.absence)

#Making a timeseries aggregating Absenteeism.time.in.hours by Month.of.absence

monthly_absence = aggregate(absent1$Absenteeism.time.in.hours,by=list(Category=absent1$Month.of.absence),FUN=sum)

monthly_absence = monthly_absence[2:13,]

monthly_absence

#Calculating absenteeism time as percent of total time in column absenthours

monthly_absence$absenthours = monthly_absence$x/3

row.names(monthly_absence) = monthly_absence$Category

monthly_absence

# Modelling time series using arima

tsdata = ts(monthly_absence$absenthours)

class(tsdata)

tsdata

#Visualizing timeseries

plot(tsdata)

#Checking stationarity - Augmented Dickey-Fuller Test

library(tseries)

adf.test(tsdata, alternative="stationary", k=0)

# Augmented Dickey-Fuller Test
# data:  tsdata
# Dickey-Fuller = -3.3984, Lag order = 0, p-value = 0.078
# alternative hypothesis: stationary

#time series tsdata is not stationary as determined by p-value of 0.078(>0.05).

#We will be subtracting shifted(single lag) time series from original time series.

tsdata2 = tsdata - lag((tsdata),1)

plot(tsdata2)

#Doing Augmented Dickey-Fuller Test again

adf.test(tsdata2, alternative="stationary", k=0)

#ACF plot

acf(tsdata2)

#value of p should be 0.

#PACF plot

pacf(tsdata2)

#value of q should be 0.

library(forecast)

model = arima(tsdata2,c(4,0,9))

fit1 = fitted(model)

residuals1 = tsdata2 - fit1
 
sum(residuals1**2)

#RSS(residual sum of squares) for order=(1,0,0):61679.74
#RSS(residual sum of squares) for order=(2,0,0):47290.44
#RSS(residual sum of squares) for order=(3,0,0):20928.83
#RSS(residual sum of squares) for order=(4,0,0):20649.88
#RSS(residual sum of squares) for order=(4,0,1):20653.29
#RSS(residual sum of squares) for order=(4,0,2):16526.79
#RSS(residual sum of squares) for order=(4,0,3):10442.2
#RSS(residual sum of squares) for order=(4,0,4):8476.191
#RSS(residual sum of squares) for order=(4,0,5):8104.803
#RSS(residual sum of squares) for order=(4,0,7):6743.328
#RSS(residual sum of squares) for order=(4,0,9):2222.32

#Arima with order=(4,0,9) gives us lowest RSS of 2222.32 so we will order=(4,0,9)

plot(tsdata2)
lines(fit1)

absent2011 = predict(model,n.ahead = 12)

#Scaling absent2011 back to original

absence_2011 = cumsum(absent2011$pred)

absence_2011_2 = absence_2011 + rep(tsdata[4],12)

as.data.frame(absence_2011_2)

ts_2011 = ts(absence_2011_2)

df1 = as.data.frame(absence_2011_2)

row.names(df1) = c(13:24)

ts_2011 = ts(df1$absence_2011_2,start=13)

#Plotting original timeseries & forecast values

plot(tsdata,xlim=c(1,24))
lines(ts_2011)

