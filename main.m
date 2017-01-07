%6-Jan-2017
%rng(1)
%a = QuestionBase;
%a.Value = randi([0 50],1,10);
%roundOff(a);
format short
syms lambda_kl 

total_unit = 1000; question_k = 10; total_learner = 1;
%stores all the questions given to user
question_database = zeros(total_unit,question_k);
%stores the MLE value
MLE_database = zeros(total_unit,total_learner);
%ith knowledge unit
for(i = 1:1)
    %i'th knowledge unit
    rng(i);
    %getting the questions randomly
    a_i = QuestionBase;
    a_i.Value = randi([0 50],1,10);
    [x beta] = roundOff(a_i);
    
    question_database = a_i.Value;
    %with probability of getting correct answer as 0.6
    p = 0.6;
    %getting users response
    Q_kl = rand(1,10) > (1-p);
    beta_j = beta;
    x_j = x;
    mle_kl = 1;
    %calculating the the lambda equation, for the whole unit
    for j = 1:10
        if (Q_kl(j) == 1)
            mle_kl =  mle_kl * (   (1 - lambda_kl)*beta_j(j) + lambda_kl*x_j(j));
        else 
            mle_kl =  mle_kl * (1 -(1 - lambda_kl)*beta_j(j) - lambda_kl*x_j(j));
        end
    end
    
    %diffrentiating it for finding stationary points
    d_mle_kl = diff(mle_kl);
    eqn = d_mle_kl == 0;
    %finding roots for maxima
    roots = solve(eqn, lambda_kl);
    
    %saving the maximum value of equation
    maximum_liklihood = -9999;
    for m = 1:length(roots)
        lambda_kl = roots(m);
        temp = subs(mle_kl);
%         if temp > 1 || temp < 0
%             display('ERROR')
%         else
%             display(temp)
%         end
        if temp > maximum_liklihood
            maximum_liklihood = temp;
        end
    end
     
    MLE_database(i,1) = maximum_liklihood;
    disp(' ');disp('Maximum Liklihood Est. :::');disp(maximum_liklihood);
    
    %Condition for answer to be a probability.
    %NOTE : some values are coming negitive 
    %because the second term for which Q_kl == 0 can 
    %take up negitive value. For it to be in [0 1]
    %           [1 - beta]
    % lambda >  ----------    
    %           [x - beta]
    
end

