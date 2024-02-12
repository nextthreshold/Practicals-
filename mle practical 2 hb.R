rm(list=ls())
library(psych)
#Question 1


cor.mat<-matrix(c(1,0.676,0.875,0.676,1,0.699,0.875,0.699,1),nrow=3,byrow=T)
pca<-princomp(cor.mat,cor = T)
summary(pca)
pca$scores

f_1<-principal(cor.mat,nfactors = 1,rotate = 'none')
f_1
summary(f_1)
f_1$loadings

f_2<-principal(cor.mat,nfactors = 2,rotate = 'none')
f_1
summary(f_2)
f_2$loadings

#Question 2
mat<-matrix(c(1,0,0,0,0,0,
              0.505,1,0,0,0,0,
              0.569,0.422,1,0,0,0,
              0.602,0.467,0.926,1,0,0,
              0.621,0.482,0.877,0.874,1,0,
              0.603,0.45,0.878,0.894,0.937,1),nrow=6,byrow=T)
mat2<-mat
mat2[upper.tri(mat2)]<-t(mat2)[upper.tri(mat2)]
mat2

pca2<-princomp(mat2,cor = T)
summary(pca2)
pca2$loadings
f_1<-principal(mat2,nfactors = 2,rotate = 'none')
f_1$uniquenesses #specific variances
a1<-diag(f_1$uniquenesses)
f_1$communality #communality 
l_1<-f_1$loadings
summary(pca2) #prop of variance 
b1<-l_1%*%t(l_1)
resi<-mat2-b1-a1
resi
#by MLE 

f_4<-factanal(covmat = mat2,factors=2,rotation='none')
f_4$loadings
f_4$uniquenesses #specificity
1-f_4$uniquenesses  #communality
a_2<-diag(f_4$uniquenesses )
l_2<-f_4$loadings
b2<-l_2%*%t(l_2)
resi2<-mat2-b2-a_2
diag(resi2)<-0
#by varimax rotation 
f_5<-principal(mat2,nfactors = 2,rotate = 'varimax')
f_5$uniquenesses #specific variances
f_5$communality #communality 
f_5$loadings

#question 3
mat3<-matrix(c(1,0.63,0.45,0.63,1,0.35,0.45,0.35,1),nrow=3,byrow=T)
mat3<-cov2cor(mat3)
f_6<-principal(mat3,nfactors=1,rotate='none',residuals = T)
f_6$communality
?principal

#question 4 
indi<-(1:16)
maths<-(1:16)
phy<-c(6,5,3,2,1,4,8,7,9,10,11,13,12,15,14,16)
length(cr)
lit<-c(9,11,14,10,12,13,15,16,6,8,7,4,5,1,3,2)
mu<-c(8,10,16,9,11,15,14,13,12,7,5,2,6,4,3,1)
tt<-c(8,14,16,15,1,2,6,7,9,10,11,13,12,4,5,3)
cr<-c(7,11,13,14,1,6,5,16,9,8,10,15,12,3,4,2)
df<-data.frame(indi,maths,phy,lit,mu,tt,cr)
head(df)
df<-df[-1]
cor<-cor(df)
pca4<-princomp(df,cor=T)
scree<-plot(pca4,type='l',main="Scree Plot")

f_7<-principal(df,nfactors=3,rotate="varimax")
f_7$loadings
f_7$scores
