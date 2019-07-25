# THIS IS BFTUTORIAL.R

#***************
#TUTORIAL STEP 1
#***************
# Load libraries. These libraries contain functions such as lm and bain that will be used in 
# this R code. Each time you reopen BFTutorial.R you have to execute this step.

library(MASS)
library(bain)
library(psych)


#***************
#TUTORIAL STEP 2
#***************
# Read Data. If you open monin.txt and holbar.txt in a text editor, you can see
# that variable labels have been inserted between " " in the first line of the file
# (this is called a header).

monin<-read.table("monin.txt",header=TRUE)
monin$group<-factor(monin$group)       # this command tells R that group is a factor and 
# not a continuous variable like attract
holubar<-read.table("holubar.txt",header=TRUE)
holubar$gr <- factor(holubar$gr)       # this command tells R that gr is a factor and 
# not a continuous variable like at


#***************
#TUTORIAL STEP 3
#***************
# Compute descriptives for the Monin data. The dependent variable name is attract, 
# the factor name is group (see also TutorialStep 2).

descrip <- describeBy(monin$attract,monin$group,mat=TRUE)
print(descrip)


#***************
#TUTORIAL STEP 4
#***************
# Compute the Bayes factor for the Monin data. 
# If you return to BFTutorial.R after closing it, you always have to rerun 
# Steps 1 and 2, and the lines between ====== below

#========================================================

# Using the R package lm the means of attract in each of the groups and the residual variance are estimated
# for the Monin data. Note that, attract~group-1 intstructs lm to regress attract on group. The -1 instructs
# lm to estimate the means in each group. Without the -1 regular dummy coding would have been applied. The
# results are collected in, what is called, the R object, prepmonin
prepmonin <-  lm(attract~group-1, data=monin)

# Inspect the estimats and collect the estimates of the group means from prepmonin and store them in the vector estm
estm<-coef(prepmonin)
print(estm)

#assign names to the estimates - seen the bain help file for the requirements that names
#have to adhere to.
names(estm) <- c("g1","g2","g3")

# Collect the residual variance from prepmonin and store it in varm
varm <- (summary(prepmonin)$sigma)**2

# Collect the sample size of each group from the data matrix and store them in the vector sampm
sampm<-table(monin$group)

# Compute the variance of the sample means in each group (divide the residual variance by the sample
# size) and store them in cov1m, cov2m, cov3m
cov1m <- varm/sampm[1]
cov2m <- varm/sampm[2]
cov3m <- varm/sampm[3]

# Tell R that cov1m, cov2m and cov3m are 1x1 matrices
cov1m <- matrix(cov1m,1,1)
cov2m <- matrix(cov2m,1,1)
cov3m <- matrix(cov3m,1,1)

# Collect cov1m, cov2m and cov3m in a list
covm<-list(cov1m,cov2m,cov3m)

#=========================================================

# Run Bain with the information specified thusfar in Tutorial Step 4. Note that, group_parameters=1 states that 
# there is 1 group specific parameter (the mean in each group) and that joint_parameters=0 states that there are
# no parameters that apply to each of the groups (e.g. the regression coefficient of a covariate).
#
# The null-hypothesis that the three means are equal is provided via "g1 = g2 = g3". See the bain help file
# for the requirements that hypotheses have to adhere to.
#
# To be able to repeat an analysis exactly, always set the seed before running bain.
#
# The results of the analysis are stored in the R object resmonin.

set.seed(100)
resmonin<-bain(estm,"g1 = g2 = g3",n=sampm,Sigma=covm,group_parameters=1,joint_parameters = 0)

# print the results of the analysis with bain

print(resmonin)

#***************
#TUTORIAL STEP 5
#***************
# Compute the Bayes factor for "all" the hypotheses for the Monin data
# Note that estm, covm and sampm have already been computed in Step 4.

set.seed(100)
resmonin<-bain(estm,"g1 = g2 = g3; g1 = g2; g1 = g3; g2 = g3",n=sampm,Sigma=covm,group_parameters=1,joint_parameters = 0)

# print the results of the analysis with bain

print(resmonin)

#***************
#TUTORIAL STEP 6
#***************
# Bayesian updating. Stepwise increasing the sample size for the Monin data

# WARNING: INCREASE THE SIZE OF THE PLOT WINDOW IN THE LOWER RIGHT HAND SCREEN
# OTHERWISE YOU ARE BOUND TO RECEIVE AN ERROR MESSAGE

# create an R object BFmonin in which the Bayes factor for different sample sizes 
# will be collected.
BFmonin<-1

# start with an initial sample size of 2+0=2 persons per group an incease by 1 
# until 2+17=19 persons per group

for (i in 0:17){

# collect the subset of persons selected in a matrix  
subdata <- matrix(0, nrow = 6+3*i, ncol = 2)  # in total 6 initial persons in 3 groups
                                              # for which 2 variables have been recorded
                                              # when i=0, each group contains 2 persons
                                              # when i=1, 1 additional person is added to each of the 3 groups
                                              # when 1=17, 17 additional persons are added to each group
subdata <- monin[c(1:(2+i),20:(21+i),39:(40+i)),1:2] # the first person in the first group is 1
                                                     # the first person in the second group is 20
                                                     # the first person in the third group is 39
                                                     # when i=0 each group contains 2 persons
                                                     # when i=5 each group contains 2+5=7 persons

submonin <-  lm(attract~group-1, data=subdata)
subvarm <- (summary(submonin)$sigma)**2
subestm<-coef(submonin)
names(subestm)<-c("g1", "g2", "g3")

# When i=0 the sample size in each group is 2. When i=9, the sample size in each group is 11.
subsampm<-c(2+i,2+i,2+i)

subcov1m <- subvarm/subsampm[1]
subcov2m <- subvarm/subsampm[2]
subcov3m <- subvarm/subsampm[3]
subcov1m <- matrix(subcov1m,1,1)
subcov2m <- matrix(subcov2m,1,1)
subcov3m <- matrix(subcov3m,1,1)

subcovm<-list(subcov1m,subcov2m,subcov3m)

set.seed(100)
resmonin<-bain(subestm,"g1 = g2 = g3",n=subsampm,Sigma=subcovm,group_parameters=1,joint_parameters = 0)

# collect BF0a from the object resmonin and store in BFmonin. This is done for i=1 to 17.
BFmonin[1+i]<-resmonin$fit$BF[1]
}

# create a vector containing the sample size per group
NperGroup<-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
# plot NperGroup vs BFmonin, add labels for the y-axis and the x-axis
plot(NperGroup,BFmonin,ylab="BF0a",xlab="N per Group")
# and connect the point by a line
lines(NperGroup,BFmonin)

#***************
#TUTORIAL STEP 7
#***************
# Bayesian updating. Posterior Model Probabilities for the Monin Data

# Create a vector in which the 5 PMPs resulting from Bain can be stored
pall<-1
# Create a vector in which each of the 5 PMPs resulting from Bain analysis with increasing sample
# sizes can be stored
p1<-1
p2<-1
p3<-1
p4<-1
p5<-1

for (i in 0:17){
  
  subdata <- matrix(0, nrow = 6+3*i, ncol = 2)
  subdata <- monin[c(1:(2+i),20:(21+i),39:(40+i)),1:2]
  
  submonin <-  lm(attract~group-1, data=subdata)
  subvar  <- (summary(submonin)$sigma)**2
  subest <-coef(submonin)
  names(subest)<-c("g1", "g2", "g3")
  
  subsamp<-c(2+i,2+i,2+i)
  
  subcov1 <- subvar/subsamp[1]
  subcov2 <- subvar/subsamp[2]
  subcov3 <- subvar/subsamp[3]
  subcov1 <- matrix(subcov1,1,1)
  subcov2 <- matrix(subcov2,1,1)
  subcov3 <- matrix(subcov3,1,1)
  
  subcov<-list(subcov1,subcov2,subcov3)
  
  set.seed(100)
  resmonin<-bain(subest,"g1 = g2 = g3; g1 = g2; g1 = g3; g2 = g3",n=sampm,Sigma=covm,group_parameters=1,joint_parameters = 0)
  
# The posterior model probabilities obtained for the sample size at hand are collected from the 
# R object resmonin which contains the results of the Bain analysis
  pall <- resmonin$fit$PMPb
# # R thinks that the numbers stored in pall are labels. The command below tells it otherwise.
#   pall <- as.numeric(levels(pall))[pall]

# Story the PMPs for each hypothesis in a vector, that is, collect the PMPs obtained for the different
# sample sizes in a vector
  p1[i+1]<-pall[1]
  p2[i+1]<-pall[2]
  p3[i+1]<-pall[3]
  p4[i+1]<-pall[4]
  p5[i+1]<-pall[5]
}
NperGroup<-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)

# create a legenda for the plot
leg <- c("H0","Ha1","Ha2","Ha3","Ha")

# give each line a different appearance
lty <- c(1,2,3,4,5)

# plot NperGRoup versus p1 using a line plot (type = "l") and a y-axis bounded between 0 and 1 (ylim=(0:1.0))
plot(NperGroup,p1,type = "l",ylab = "posterior probabilities",xlab="N per Group",ylim=(0:1.0))
# add a line for p2, p3, p4, and p5 to the plot
lines(NperGroup,p2,lty=2)
lines(NperGroup,p3,lty=3)
lines(NperGroup,p4,lty=4)
lines(NperGroup,p5,lty=5)
# insert the legenda in the plot at x=17 and y=.95 
legend(x = 17, y = 0.95,legend = leg, lty =lty)

#****************
#TUTORIAL STEP 8A
#****************
# Sensitivity Analysis Monin Data

# compute BF0a using b-g
set.seed(100)
resmonin<-bain(estm,"g1 = g2 = g3",n=sampm,Sigma=covm,group_parameters=1,joint_parameters = 0)
print(resmonin)

# compute BF0a replacing b-g by 2 x b-g
set.seed(100)
sampm2<- sampm/2 # dividing each sample size by 2 is equivalent to using 2 x b-g
resmonin<-bain(estm,"g1 = g2 = g3",n=sampm2,Sigma=covm,group_parameters=1,joint_parameters = 0)
print(resmonin)

# compute BF0a replacing b-g by 3 x b-g
set.seed(100)
sampm3<- sampm/3 # dividing each sample size by 3 is equivalent to using 3 x b-g
resmonin<-bain(estm,"g1 = g2 = g3",n=sampm3,Sigma=covm,group_parameters=1,joint_parameters = 0)
print(resmonin)

#****************
#TUTORIAL STEP 8B
#****************
# Sensitivity Analysis Holubar Data

prepholubar <-  lm(at~gr-1, data=holubar)
varh <- (summary(prepholubar)$sigma)**2
esth<-coef(prepholubar)
names(esth)<-c("g1", "g2", "g3")

samph<-table(holubar$gr)

cov1h <- varh/samph[1]
cov2h <- varh/samph[2]
cov3h <- varh/samph[3]
cov1h <- matrix(cov1h,1,1)
cov2h <- matrix(cov2h,1,1)
cov3h <- matrix(cov3h,1,1)

covh<-list(cov1h,cov2h,cov3h)

# compute BF0a using b-g
set.seed(100)
resholubar<-bain(esth,"g1 = g2 = g3",n=samph,Sigma=covh,group_parameters=1,joint_parameters = 0)
print(resholubar)

# compute BF0a replacing b-g by 2 x b-g
samph2<- samph/2
set.seed(100)
resholubar<-bain(esth,"g1 = g2 = g3",n=samph2,Sigma=covh,group_parameters=1,joint_parameters = 0)
print(resholubar)

# compute BF0a replacing b-g by 3 x b-g
samph3<- samph/3
set.seed(100)
resholubar<-bain(esth,"g1 = g2 = g3",n=samph3,Sigma=covh,group_parameters=1,joint_parameters = 0)
print(resholubar)

#***************
#TUTORIAL STEP 9
#***************
# Compute the Bayes factor for the Monin data with two outliers

# Created for the third group are two persons with scores of 9 and 10 on attract.
# These have been added to monin.txt rendering the file moninoutlier.txt

monoutl<-read.table("moninoutlier.txt",header=TRUE)
monoutl$group <- factor(monoutl$group)

prepoutlier <-  lm(attract~group-1, data=monoutl)
varoutlier <- (summary(prepoutlier)$sigma)**2
estoutlier<-coef(prepoutlier)
names(estoutlier)<-c("g1", "g2", "g3")


sampoutlier<-table(monoutl$group)

cov1outlier <- varoutlier/sampoutlier[1]
cov2outlier <- varoutlier/sampoutlier[2]
cov3outlier <- varoutlier/sampoutlier[3]
cov1outlier <- matrix(cov1outlier,1,1)
cov2outlier <- matrix(cov2outlier,1,1)
cov3outlier <- matrix(cov3outlier,1,1)

covoutlier<-list(cov1outlier,cov2outlier,cov3outlier)

set.seed(100)
resbain<-bain(estoutlier,"g1 = g2 = g3",n=sampoutlier,Sigma=covoutlier,group_parameters=1,joint_parameters = 0)

# print the results of the analysis with bain

print(resbain)

#****************
#TUTORIAL STEP 10
#****************

# Evaluating informative hypotheses using teh Monin data
set.seed(100)
resmonin<-bain(estm,"g1 > g2 > g3; g1 > g2 = g3; g1 = g2 & g1 > g3",n=sampm,Sigma=covm,group_parameters=1,joint_parameters = 0)

# print the results of the analysis with bain
print(resmonin)
# use the command below to obtain the BFmatrix
print(resmonin$BFmatrix)

#****************
#TUTORIAL STEP 11
#****************
# Sensitivity Analyis for the  Evaluation of Inequality Constrained Hypotheses
set.seed(100)
resmonin<-bain(estm,"g1 > g2 > g3",n=sampm,Sigma=covm,group_parameters=1,joint_parameters = 0)
print(resmonin)

sampj2<- sampm/2
set.seed(100)
resmonin<-bain(estm,"g1 > g2 > g3",n=sampj2,Sigma=covm,group_parameters=1,joint_parameters = 0)
print(resmonin)

sampj3<- sampm/3
set.seed(100)
resmonin<-bain(estm,"g1 > g2 > g3",n=sampj3,Sigma=covm,group_parameters=1,joint_parameters = 0)
print(resmonin) 

#*****************
#TUTORIAL STEP 12A
#*****************
# Replication of Monin using the Holubar data

# Compute descriptives the Holubar data. The dependent variable name is at,
# the factor name is gr (see also TutorialStep 2)

descrip <- describeBy(holubar$at,holubar$gr,mat=TRUE)
print(descrip)

#*****************
#TUTORIAL STEP 12B
#*****************
# Evaluate H0 and Ha using the Holubar data - if you interrupted the tutorial
# rerun Tutorial step 8B

set.seed(100)
resholubar<-bain(esth,"g1 = g2 = g3",n=samph,Sigma=covh,group_parameters=1,joint_parameters = 0)
print(resholubar)

#*****************
#TUTORIAL STEP 12C
#*****************
# Evaluate Horiginal as derived from the Monin analysis Using the Holubar data

set.seed(100)
resholubar<-bain(esth,"g1 = g2 = g3; g1 = g2 & g1 > g3",n=samph,Sigma=covh,group_parameters=1,joint_parameters = 0)
print(resholubar)

# To obtain the sample sizes, estimates and covariance matrix run the lines below
print(resholubar$n)
print(resholubar$estimates)
print(resholubar$posterior)

# ********************************************************************
# Below you find the codes used to recreate the monin and holubar data 
# ********************************************************************

# set the seed of the random number generator

# set.seed(123) # note that the seed used to recreate the monin and holubar data 
# used in the tutorial has been lost. This is irrelevant because the sufficient
# statistics of each data set (N, mean, sd) will be the same.

# Create the monin data

# Create variable attract
attract <- 1

# Generate normal random deviation for 67 persons (the Monin sample size)
attract[1:67]<-rnorm(67,mean=0,sd=1)

# For each of the three groups standardize the sampled values
attract[1:19]<-(attract[1:19]-mean(attract[1:19]))/sd(attract[1:19])
attract[20:38]<-(attract[20:38]-mean(attract[20:38]))/sd(attract[20:38])
attract[39:67]<-(attract[39:67]-mean(attract[39:67]))/sd(attract[39:67])

# For each group multiply with the standard deviation and add the mean as reported by Monin
attract[1:19]<-attract[1:19]*1.38+1.88
attract[20:38]<-attract[20:38]*1.95+2.54
attract[39:67]<-attract[39:67]*2.38+.02
group <- 1

# Create a factor group with the numbers 1, 2 and 3
group[1:19]<-1 #"ob"
group[20:38]<-2 #"rsa"
group[39:67]<-3 #"rbog"

# Create a data frame containing a header, attract and group and write it to a file monin.txt
monin<-matrix(c(attract,group),nrow=67,ncol=2)
monin<-as.data.frame(monin)
colnames(monin) <- c("attract", "group")
write.table(monin,"monin.txt",sep=" ",col.names=TRUE,row.names=FALSE)

# create the holubar data

at <- 1
at[1:75]<-rnorm(75,mean=0,sd=1)
at[1:20]<-(at[1:20]-mean(at[1:20]))/sd(at[1:20])
at[21:47]<-(at[21:47]-mean(at[21:47]))/sd(at[21:47])
at[48:75]<-(at[48:75]-mean(at[48:75]))/sd(at[48:75])
at[1:20]<-at[1:20]*1.20+.98
at[21:47]<-at[21:47]*1.88+.02
at[48:75]<-at[48:75]*1.72+.27
gr <- 1
gr[1:20]<-1 #"ob"
gr[21:47]<-2 #"rsa"
gr[48:75]<-3 #"rbog"
holubar<-matrix(c(at,gr),nrow=75,ncol=2)
holubar<-as.data.frame(holubar)
colnames(holubar) <- c("at","gr")
write.table(holubar,"holubar.txt",sep=" ",col.names=TRUE,row.names=FALSE)

