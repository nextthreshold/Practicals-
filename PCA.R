rm(list=ls())
#Q.1
R=matrix(c(1,0.676,0.875,0.676,1,0.699,0.875,0.699,1),nrow=3,byrow=TRUE);R
a=eigen(R);a 
eigval<-a$values  
a$vectors

#Proportional Variance explained by each component 

prop1=eigval[1]/sum(eigval);prop1  
prop2=eigval[2]/sum(eigval);prop2 
prop3=eigval[3]/sum(eigval);prop3 
# correlation of original variable 
ry1x1=sqrt(a$values[1])*a$vectors[1,1];ry1x1  
ry1x2=sqrt(a$values[1])*a$vectors[2,1];ry1x2  
ry1x3=sqrt(a$values[1])*a$vectors[3,1];ry1x3  


ry2x1=sqrt(a$values[2])*a$vectors[1,2];ry2x1  
ry2x2=sqrt(a$values[2])*a$vectors[2,2];ry2x2  
ry2x3=sqrt(a$values[2])*a$vectors[3,2];ry2x3  

summary(princomp(R,cor = TRUE))

#Q.2
sigma=matrix(c(1,0.25,0.25,0.25,1,0.25,0.25,0.25,1),nrow=3,byrow=TRUE);sigma
p<-prcomp(sigma,center=TRUE,scale.=TRUE)
summary(p)
b<-cor(sigma);b
c=eigen(b);c
eigval<-c$values 
c$vectors

#Proportion of variance explained by each component 
prop1=eigval[1]/sum(eigval);prop1  
prop2=eigval[2]/sum(eigval);prop2 
prop3=eigval[3]/sum(eigval);prop3 

# Correlation between the original variables 
ry1x1=sqrt(c$values[1])*c$vectors[1,1];ry1x1  #-0.7071068
ry1x2=sqrt(c$values[1])*c$vectors[2,1];ry1x2  #-0.7071068
ry1x3=sqrt(c$values[1])*c$vectors[3,1];ry1x3  #-0.7071068


ry2x1=sqrt(c$values[2])*c$vectors[1,2];ry2x1   #0.7071068
ry2x2=sqrt(c$values[2])*c$vectors[2,2];ry2x2  #-0.3535534
ry2x3=sqrt(c$values[2])*c$vectors[3,2];ry2x3  #-0.3535534



#Q.3
R=matrix(c(1,0.920,0.875,0.625,0.920,1,0.889,0.750,0.875,0.889,1,0.425,0.625,0.750,0.425,1),nrow=4,byrow=TRUE);R
a=eigen(R);a 
eigval<-a$values  
a$vectors
princomp(R)$scores
#Proportional Variance 
prop1=eigval[1]/sum(eigval);prop1  
prop2=eigval[2]/sum(eigval);prop2 
prop3=eigval[3]/sum(eigval);prop3 
prop4=eigval[4]/sum(eigval);prop4 

summary(princomp(R),center=TRUE,scale.=TRUE,cor=T)

#Q.4
data <- read.csv("C:/Users/Owner/Desktop/data.csv", header=TRUE)
d1 <- data[-1]
head(d1)
corr_matrix<- cor(d1)
corr_matrix
A<- eigen(corr_matrix)
A$vectors
A$values
princomp(d1)
princomp(d1)$scores
