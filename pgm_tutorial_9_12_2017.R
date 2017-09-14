################################
###Basic Baysian Rule Example###
###date: 12 Sept 2017         
################################

#We need prior and liklihood (and data ofcourse) 
#to calculate posterior

###Case1
#Prior probability of bulb, with the assumption that
#it is working.(pr = 0.99)
#prior <- c(working = 0.99, broken = 0.01)
###Case2
#Prior probability of bulb, with the NO assumption 
prior <- c(working = 0.5, broken = 0.5)

#Liklihood probabilites of being good or bad given the state of machine
likelihood <- rbind(
      working = c(good=0.99, bad=0.01), broken = c(good=0.6, bad=0.4))
#Data or observations
data <- c("bad", "bad", "bad", "bad")

#Bayesian update network
bayes <- function(prior, liklihood, data){
  posterior <- matrix(0, nrow=length(data), ncol=length(prior))
  dimnames(posterior) <- list(data, names(prior))
  
  initial_prior <- prior
  for(i in 1:length(data)){
    #Bayes Rule
    posterior[i, ] <- prior*liklihood[ , data[i]]/sum(prior * liklihood[ , data[i]])
    #update the prior
    prior <- posterior[i, ]
    
  }
  return(rbind(initial_prior, posterior))
}

#Plotting the data
matplot( bayes(prior,likelihood,data), t='b', lty=1, pch=20, col=c(3,2))

#with differnt input, here algorithm hesitates first, but then it converge back
prior=c(working=0.99,broken=0.01)
data=c("bad","good","good","good","good","good","good","good","good","good")
matplot(bayes(prior,likelihood,data),t='b',pch=20,col=c(3,2))

########################
###PGM Simple Example###
########################

#To install packages in RStudio
#source("http://bioconductor.org/biocLite.R")
#biocLite()
#install.packages("gRain")

library("gRbase")
#if "RBGL" related error accours try below two commands
#source("https://bioconductor.org/biocLite.R")
#biocLite("RBGL")
#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")

#define simple undirected graph with five variables A, B,C , D, E
graph <- ug("A:B:E + C:E:D")
plot(graph)
dag <- dag("A + B:A + C:B + D:B + E:C:D")
dag
plot(dag)

#Define graph for machine bulb problem
machine_val <- c("working", "broken")
light_bulb_val <- c("good","bad")

machine_prob <- c(99,1)
light_bulb_prob <- c(99, 1, 60, 40)

#define random variable with gRain (?)
library("gRain")
#cptable means conditional probability table:
M <- cptable(~machine, values=machine_prob, levels=machine_val)
L <- cptable(~light_bulb|machine, values=light_bulb_prob, levels=light_bulb_val)

plist <- compileCPT(list(M,L))
#plist
#plist$machine
#plist$light_bulb
net  <- grain(plist)
net2 <- setEvidence(net, evidence=list(light_bulb="bad"))
querygrain(net2, nodes=c("machine"))


#####################
###Margenalisation###
###date: 13 Sept 2017         
#####################

A = matrix(c(0.8, 0.2), 2, 1)
B = matrix(c(0.6, 0.4, 0.3, 0.7), 2, 2)
C = matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2)
D = matrix(c(0.3, 0.7, 0.4, 0.6), 2, 2)

#SumProductEliminateVar Algorithm
#Step 1, eliminate A in order to obtain P(B, C, D)
#so we need to marginalize A out
Bs = t(A) %*% t(B)
#Step 2, eliminate B
Cs = Bs %*% t(C)
#Step 3, eliminate C
Ds = Cs %*% t(D)


#####################
###JuntionTree_Alg###
#####################

library(gRain)
val = c("true", "false")
#cptable = creates conditional probability table
F = cptable(~F,  values=c(10,90), levels=val)
C = cptable(~C|F,values=c(10,90,20,80),levels=val)
E = cptable(~E|F,values=c(50,50,30,70),levels=val)
A = cptable(~A|C,values=c(50,50,70,30),levels=val)
D = cptable(~D|E,values=c(60,40,70,30),levels=val)
B = cptable(~B|A:D,values=c(60,40,70,30,20,80,10,90),levels=val)

#facotrization of joint probability
plist = compileCPT(list(F,E,C,A,D,B))
plist
print(plist$F)
print(plist$B)

#create graph cand run jucntion tree
jtree = grain(plist)

#that is it, we have the junction tree with us
#we can now perform the queries
#marginal distributions
querygrain(jtree, nodes=c("F"), type="marginal")
querygrain(jtree, nodes=c("C"), type="marginal")
querygrain(jtree, nodes=c("A"), type="marginal")
querygrain(jtree, nodes=c("B"), type="marginal")
querygrain(jtree, nodes=c("B","A"), type="joint")
querygrain(jtree, nodes=c("A","B","C"), type="joint")

#now we want to observe variable and compute posterior 
#distribution, lets sat F=true, we want to propogate 
#this distribution down to the network
jtree2 = setEvidence(jtree, evidence=list(F="true"))
querygrain(jtree2, nodes=c("A"), type="marginal")
querygrain(jtree2, nodes=c("A","B"), type="joint")

#you can further give more evidence and see the effect
jtree3 = setEvidence(jtree, evidence=list(F="true",A="false"))

########################
###Learning Parameter###
###date: 14 Sept 2017         
########################

#load iris dataset
x=read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",col.names=c("sepal_length","sepal_width","petal_length","petal_width","class"))
head(x)

#assuming sepal_length is gaussina distribution, mean and variance 
#of this distribution can be calculated as
mean(x$sepal_length)
var(x$sepal_length)

#plyr package to use random variable
library(plyr)
y = daply(x, .(class),nrow) / nrow(x)

#compute conditional distribution
daply(x,.(class), function(n) mean(n$sepal_length))
#discretising sepal_length
q <- quantile(x$sepal_width,seq(0,1,.33))
x$dsw[ x$sepal_width < q['33%']] = "small"
x$dsw[ x$sepal_width >= q['33%'] & x$sepal_width < q['66%'] ] = "medium"
x$dsw[ x$sepal_width >= q['66%'] ] = "large"
p1 <- daply(x,.(dsw,class), function(n) nrow(n))

#baysean learning inference
posterior <- function(prob, nh, nt, Theta = c(0.2, 0.5, 0.8)){
  x = numeric(3)
  for(i in 1:3)
    #x[i] = prob[i] * (Theta[i]^nh) * ((1-Theta[i])^nt)
    #new algo:use log when dealing with lot of multiplications like these
    x[i] = exp ( log(prob[i]) + nh*log(Theta[i]) + nt*log(1-Theta[i]) )
  
  norm = sum(x)
  return(x/norm)
}

case1 = posterior(c(0.2,0.75,0.05),2,8)
case2 = posterior(c(0.2,0.75,0.05),8,2)
case3 = posterior(c(0.2,0.75,0.05),5,5)
case4 = posterior(c(0.2,0.75,0.05),10,10)
case5 = posterior(c(0.2,0.75,0.05),50,50)

