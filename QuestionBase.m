%6-Jan-2017

%###################################################################
%#########Maximum Likelihood Estimation for learners ability########
%###################################################################

%###Jan-6-2017###

%#Estimate the parameter - how well learners knows the topic.
%# k : knowledge unit  l : learner
% # MLE (lambda_kl | Q_kl) = {pi(R_kl)[(1 - lambda_kl)*beta_j + lambda_kl*x_j] * 
% #                           pi(W_kl)[1 - (1 - lambda_kl)*beta_j - lambda_kl*x_j]}  
% Question.Unit <- function(Question.base)
% {
%   me1 <- list( q1=Question.base[1] , q2=Question.base[2] , q3=Question.base[3] ,
%                q4=Question.base[4] , q5=Question.base[5] , q6=Question.base[6] , 
%                q7=Question.base[7] , q8=Question.base[8] , q9=Question.base[9] , 
%                q10=Question.base[10]
%   )
%   
%   ## Set the name for the class
%   class(me1) <- append(class(me1),"Question.Unit")
%   return(me1)
% }

%class for questions for knowledge unit k also gives beta and x values
classdef QuestionBase
   properties
      Value
   end
   methods
      function [x beta] = roundOff(obj)
          for i = 1:10;
              j = obj.Value(i);
              rng(j);
              a = 0.6;
              b = 0.7;
              c = 0;
              x    = (b-a).*rand(10,1) + a;
              beta = (x-c).*rand(10,1) + c;
              %r = round([obj.Value],2);
          end
      end
   end
end

