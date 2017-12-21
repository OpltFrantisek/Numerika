%M = -(D+L)^-1*U
function x0 = Gauss_Seidel_method(A,b)
    n = length(b); % poèet neznámých
    
    x0 = b; % staré x
    x1 = b; %nové x
    for k = 1:50
    for i = 1:n
        sum1 = 0;
        for j = 1:(i-1) % prvni suma  (k+1)
           sum1 = sum1 + A(i,j)*x1(j);
        end
        sum2 = 0;
        for j = (i+1):n % druhá suma   (k)
           sum2 = sum2 + A(i,j)*x0(j); 
        end
        x1(i) = (b(i)-sum1-sum2)/A(i,i);  
    end
        x0 = x1;
    end
end
