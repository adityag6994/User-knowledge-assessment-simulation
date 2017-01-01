##############################################################
#########Simulation Model for User Knowledge Assesment########
##############################################################

###Dec-28-2016###

rm(list=ls())

#Part-1
#Knowledge Structure Graph
#--------------------------------------------------------------x
#Constants for part1:
K <- (1:1000) #K:Knowledge Base (1000 Knowledge Unit)
K.column <- 100 #Number of Knowledge Unit per chapter
K.row <- length(K)/K.column #Number of chapter
K.chapter <- matrix(K,nrow = K.row,ncol = K.column,  byrow = TRUE) #Matrix with 'K.column' knowledge unit in 'K.row' chapters
dependency.factor <- 2 #Previous levels to which present unit is directly connected.
K.graph <- array(0, c(K.column, dependency.factor*K.column, K.row))
#K.graph :: Stores the dependencies of current chapter 'i' with previous chapter, where number of previous chapter is
#decided by 'dependency.factor'. For eg , in our case, for chapter 'i' it tells the relation between chapter 'i-1' and 'i-2'
#It is a [100,200,10] ~ [m,n,i] matrix, where first value 'm' indicate the respective knowledge unit of chapter 'i', 
#n = | 1 : 100 {for knowledge unit of chapter i-2}|
#    |101: 200 {for knowledge unit of chapter i-1}|

#function to randomly decide weather a knowledge unit is dependent on other.( binary )
p <- 0.5                                                         
random.seed.toss <- function(x){
  set.seed(x)
  return(rbinom(K.column,1,p))
}

#for ch1   : |all dependencies are assigned Ture|
#    ch2   : |ch.(i-2) dependencies are assigned 1 & ch.(i-1) is found by function='random.seed.toss'|
#    ch2 < : |ch.(i-1) dependencies is found by function='random.seed.toss', ch.(i-2) dependencies are assigned ..
#             .. on the basis of ch.(i)'s relation with ch.(i-1). All the chapter required by TRUE ch.(i-1) ..
#             .. are given TRUE and rest chapters dependencies is found found by function='random.seed.toss'| 

#ch1
K.graph[,,1] <- 1

#ch2
K.graph[,1:K.column,2] <- 1 # relation with ch.(i-2)
for(i in 1:K.column){       # realtion with ch.(i-1)
      temp <- random.seed.toss(i %% 10)*K.chapter[1,]
      temp[temp > 0] <- 1
      K.graph[i,(K.column+1):(dependency.factor*K.column),2] <- temp #putting them in K.graph
}


#ch2. <
for (i in 3:K.row){ #for chapter.(i) 
  for (j in 1:K.column){#for knowledge unit(j) in ch.(i)
    
    #for ch.(i-1)
    
    random.temp <- random.seed.toss(j %% 10) #choosing seed according to chapter
    temp <- random.temp*K.chapter[i-1,]
    temp[temp > 0] <- 1 #assiging TRUE for selected dependencies
    K.graph[j,(K.column+1):(dependency.factor*K.column),i] <- temp #putting them in K.graph
    
    #for ch.(i-2)
    
    temp2 <- 0 
    #assigning TRUE values in ch.(i-2) for consistency of ch.(i) with ch.(i-1) 
    for(k in (K.column+1):(dependency.factor*K.column)){
      if(K.graph[j,k,i] > 0){
        #checking for ch.(i-1) realtion with ch.(i-2)
        temp1 <- K.graph[(k - K.column),((K.column+1):(dependency.factor*K.column)),i-1]
        #if a knowledge unit(KU) from ch.(i-2) is not required for TRUE ch.(i-1) KU in ch.(i) then it should be 0/FALSE in temp2 
        temp2 <- temp2 + temp1
      }
    }
    
    #making all the other entries 1
    temp2[temp2 > 0 ] <- 1
    #for the reamining KU in ch.(i-2) deciding randomly if they are dependent on ch.(i) using function
    temp3 <- temp2 + random.temp
    temp4 <- temp3
    temp3[temp3 > 0 ] <- 1
    K.graph[j,1:(K.column),i] <- temp3 #putting them in K.graph
  }
}

###Dec-29-2016###
#--------------------------------------------------------------x
#Part-2
#Index for Learners

L <- 20 #L: Total number of learners
N.learner <- 1:L
L.Index <- array(0,c(L,K.row)) #Index for learners knowledge state (KS) for each chapter
unit.per.chapter <- 100 #number of KU taken from each chapter to be tested in learner
chapter.threshold <- 70 #minimum number of KU solvable by user in a chapter to have that ch. in its KS
user.knowledge.array <- array(0, c(K.row, K.column, L)) #saves value of lambda_k_l
#<not using this function now>
#functions for randomly selecting KU from chapter
random.unit.selection <- function(x){
  set.seed(x)
  return(sample(K.chapter[x,],unit.per.chapter))
}

#Influencing the seed with present (K,L) and returning response for..
#each knowoledge unit based on mean of lambda(K,L) for a particular chapter 

#function to generate lambda_k_l
lambda.calculator <- function(k,l) {
  set.seed(k*l)
  samp <- rnorm(200, .7, .01)
  samp <- samp[samp >= 0 & samp <= 1]
  if (length(samp) >= 100) {
    return(sample(samp, 100))
  }  
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}
#function to generate learners ability to solve for particular chapter
random.lambda.kl <- function(x,y){   #x:chapter y:learner            
  lambda.kl <- lambda.calculator(x,y) 
  return(rbinom(unit.per.chapter,1,mean(lambda.kl))) 
}

#for every learner we generate lambda depending on (K,L), using function lambda.calculator..
#these value will be used in part:4. Also, mean of these values is used to generate response 
#response, for that particular chapter. If the response has more TRUE values than a threshold
#it is assumed that learner knows that chapter. All response are such that they are consistent 
#with hierarchy.
for (l in 1:L){ #for learner l
  set.seed(l) #used if 'random.unit.selection <- function(x)' is used <not now>
  
  for(i in 1:K.row){ #for each ch.(i) 
    
    #chapter.units <- random.unit.selection(i) #selection depend on learner   
    chapter.units <- K.chapter[i,]
    #chapter.unit.realation: randomly decided weather selected units from above is solvable or not
    #learners ability to solve a KU in chapter based on (K,L)
    chapter.unit.relation <- chapter.units*random.lambda.kl(i,l) 
    #chapter.unit.relation.matrix
    user.knowledge.array[i,1:unit.per.chapter,l] <- lambda.calculator(i,l)
    #saves the above realtion as binary
    chapter.unit.relation[chapter.unit.relation > 0] <- 1
    #if there are more than 'chapter.threshold' TRUE values in leaerner response to a chapter than
    #it is assume it can solve it.
    if(sum(chapter.unit.relation) > chapter.threshold){#condition for solvability of chapter by learner
      L.Index[l,i] <- 1
    }else{
      break
    }
  }
}

#--------------------------------------------------------------x
#Part-3
#Generation of Question Object

#Question to be associated with each knowledge unit in K, with 10 question to each unit
#X:Sucess Rate : beta:lucky guess

#Question.base : source for questions, having 10 in each unit, <has to come from some database> 
#Class for question object 
Question.Unit <- function(Question.base)
{
  me1 <- list( q1=Question.base[1] , q2=Question.base[2] , q3=Question.base[3] ,
              q4=Question.base[4] , q5=Question.base[5] , q6=Question.base[6] , 
              q7=Question.base[7] , q8=Question.base[8] , q9=Question.base[9] , 
              q10=Question.base[10]
            )

  ## Set the name for the class
  class(me1) <- append(class(me1),"Question.Unit")
  return(me1)
}

###Dec-30-2016###

#q.df :  dataframe for questions for each knowledge unit
q.df <- data.frame()
#q.df.guess: dataframe for status of question base after lucky guesses
q.df.guess <- data.frame()

#number of question per unit
n.ques.unit <- 10

#function for selection of beta and x probability
lucky.probability <- function(i,p){
  set.seed(i)
  return(rbinom(10,1,p))
}

#cases for different type of questions
beta.x.input <- function(x,k){
  set.seed(k)
  if(x < 0){#true or false
    beta <- 0.5 ;sucess.rate.x <- 0.8 
  }else if(x == 0){#multiple question
    beta <- 0.25 ;sucess.rate.x <- 0.7 
  }else{
    sucess.rate.x <- rnorm(1,0.4,.1)
    beta <- runif(1,0,sucess.rate.x)
    #print(beta)
  }
  rates <- list(beta,sucess.rate.x)
  return(rates)
}

#for each knowledge unit in K
for(i in 1:length(K)){
  set.seed(i)
  #creating unique random questions for each knowledge unit out of 50 intigers
  question.sample <- t(cbind(Question.Unit(sample(1:50,n.ques.unit))))
  #appending it to dataframe
  q.df <- rbind(q.df,question.sample)
  #q.df contains all the questions for each knoeldge unit [1000 10]
  
  #adding 'beta' and 'x' factor to them
  #selecting questions with beta prob and asssigning them 'TRUE'
  #while reamining are left
  
  #for case 1:true or false || put negitive value
  #    case 2:multple choice || put '0' value
  #    case 3: || put positive value
  case <- 1
  
  rates <- beta.x.input(case,i)
  p <- as.double(rates[1])
  guess.result <- lucky.probability(i,p)
  question.sample.guess <- question.sample
  
  for(q in 1:length(guess.result)){
    if(guess.result[q] > 0){
      question.sample.guess[q] <- TRUE
    }
  }
  #q.df.guess will have question bank status after lucky guesses
  q.df.guess <- rbind(q.df.guess,question.sample.guess)

  ##This will later store the response from user as well
}

###Dec-31-2016###
############################################################################
#Part-4
#Proabbility of getting a question correct

#Pr(learner l got question j correct, where question j is associated with knowledge unit k)
#(1-lambda_{kl}) beta_j + lambda_{kl} X_j

prob.list <- array(0, c(K.row, K.column, L)) #stores the above prob. for learner L

for(l in 1:L){#for learner L
  for(j in 1:length(K)-1){
    #define type of quesition by selecting case
    case <- 0
    #getting values of beta_j and x_j
    rates <- beta.x.input(case,j)
    beta.j <- as.double(rates[1])
    x.j <- as.double(rates[2])
    
    #calculate P(l.j.correct)
    term1 <- (1- user.knowledge.array[(j/100+1),(j%%100),l])*beta.j
    term2 <- (user.knowledge.array[(j/100+1),(j%%100),l])*x.j
    P.lj.correct <- term1 +  term2  
    prob.list[(j/100+1),(j%%100),l] <- P.lj.correct
    #print(j)
    #print(P.lj.correct)
  }
} 

