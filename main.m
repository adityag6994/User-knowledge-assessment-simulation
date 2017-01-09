%6-Jan-2017
%rng(1)
%a = QuestionBase;
%a.Value = randi([0 50],1,10);
%roundOff(a);
format short
syms lambda_kl 

%total_unit:total knowledge unit
%question_k = total questions in each knowledge unit
%total_learner = total learner
total_unit = 1000; question_k = 10; total_learner = 5;
%stores all the questions given to user (same for all users)
question_database = zeros(total_unit,question_k);
%stores the MLE value
MLE_database = zeros(total_learner,total_unit);
%stores user response (for each knowledge unit)
Q_kl = zeros(question_k, total_unit ,total_learner);
%stores value for x and beta
x_database = zeros(total_unit, question_k);
beta_database = zeros(total_unit, question_k);

%Generating the knowlede base to be given to learner
display('Generating the knowlede base to be given to learner ....')
%ith knowledge unit
%for (learner = 1:total_learner)
for(i = 1:total_unit)
    %i'th knowledge unit
    rng(i);%random seed
    %getting the questions randomly
    a_i = QuestionBase;
    a_i.Value = randi([0 50],1,10);
    [x beta] = roundOff(a_i);%values of beat and x
    x_database(i,:) = x;
    beta_database(i,:) = beta;
    question_database(i,:) = a_i.Value;
end
display('done')
%%
display('Generating learners knowledge base now ......');
%Q_kl = zeros(question_k, total_unit ,total_learner);
%for learner = 1:1
for learner = 1:total_learner
    for i = 1:total_unit
        syms lambda_kl
        %with probability 'p' of getting correct answer 
        p = 0.75;
        
        %Getting users response
        
        rng(i*learner);%random seed
        %display(i)
        Q_kl(:,i,learner) = rand(1,10) > (p);
        %display(i)
        %display(Q_kl(:,i,learner)')
         beta_j = beta_database(i,:);
         x_j = x_database(i,:);
         mle_kl = 1;
         
         %calculating the the lambda equation, for the a knowledge unit unit
        
         for j = 1:question_k
         if (Q_kl(j,i,learner) == 1)
             mle_kl =  mle_kl * (   (1 - lambda_kl)*beta_j(j) + lambda_kl*x_j(j));
         else 
             mle_kl =  mle_kl * (1 -(1 - lambda_kl)*beta_j(j) - lambda_kl*x_j(j));
         end
        end
       
         %diffrentiating it for finding stationary points
         
         d_mle_kl = diff(mle_kl);
          eqn = d_mle_kl == 0;
          %eqn
          
         %finding roots for maxima
         
         roots = solve(eqn, lambda_kl);
          %only roots which lie in [0 1]
          valid_roots = roots((roots>=0&roots<=1));
     
         %saving the maximum value of equation with valid roots
         %roots are lambda value which should be between 0 and 1
         
         maximum_liklihood = 0;
         for m = 1:length(valid_roots)
              lambda_kl = valid_roots(m);
              temp = subs(mle_kl);

              if temp > maximum_liklihood
                  maximum_liklihood = temp;
              end
         end
         
      %MLE_database = zeros(total_learner,total_unit);
      %maximum_liklihood
      
      MLE_database(learner,i) = maximum_liklihood;
      
      
%      %Condition for answer to be a probability.
%      %NOTE : some values are coming negitive 
%      %because the second term for which Q_kl == 0 can 
%      %take up negitive value. For it to be in [0 1]
%      %           [1 - beta]
%      % lambda >  ----------    
%      %           [x - beta]
   
    end
end
%Doubt :: average value of MLE seem to grow uptil p=0.75 and then dicrese ,
%it should increase linearly with p ? Of course this happens because there
%exist no valid roots for that particular case, is there something I am
%getting wrong ?

display('----------');
display('--done--')
display('--');
% q=MLE_database(1,1:50);
% a=q(find(q>0));
% length(a)
 
