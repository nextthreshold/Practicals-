data=read.csv(file.choose())
data1<-subset(data,select=c("X","A","B","C","Block","Obs"))

colnames(data1)<-c("Sr.No","A","B","C","Block","Obs")

data1$A<- as.factor(data1$A)
data1$B<- as.factor(data1$B)
data1$C<- as.factor(data1$C)
data1$Block<- as.factor(data1$Block)

str(data1)



# Wihtout block effect

model1<-lm(Obs~A+B+C+A:B+B:C+A:C,data1)
print(model1)

anova(model1)
par(mfrow=c(2,2))
plot(model1)


library(carData)
library(car)
leveneTest(Obs~A,data1) # to test for homogenety of varaince  
#H0= variance is not significantly diffrent Result -  non Significant

leveneTest(Obs~B,data1) # to test for homogenety of varaince
#H0= variance is not significantly diffrent Result - non Significant

leveneTest(Obs~C,data1) # to test for homogenety of varaince
#H0= variance is not significantly diffrent Result - non Significant

# Shapiro wilks normailty test
shapiro.test(model1$residuals)  
# residuals dont foll noraml H0= Reiduals follows normal 

shapiro.test(data1$Obs) 
#Obs follows noraml H0= DATA fOLLOWS NORmal


# With block effect
model2<-lm(Obs~A+B+C+A:B+B:C+A:C+Block,data1)
print(model2)

anova  # block is not significant for the model 
shapiro.test((model2$residuals))  # Null hyptothesis rejected that the residuals dont foll normal
# h0= Residuals follws normal 


# que2 

library("doebioresearch")
data21<-read.csv(file.choose())

data21$replication<-as.factor(data21$replication)
data21$nitrogen_levels<-as.factor(data21$nitrogen_levels)
data21$levels<-as.factor(data21$levels)


splitplot(data21[4],data21$replication,data21$levels,data21$nitrogen_levels,1)

# sub plot is significant
#

#que 3

data31<- read.csv(file.choose())
