%M = -(D+L)^-1*U
function x0 = Gauss_Seidel_method(A,b)
    n = length(b); % po�et nezn�m�ch
    
    x0 = b; % star� x
    x1 = b; %nov� x
    for k = 1:50
    for i = 1:n
        sum1 = 0;
        for j = 1:(i-1) % prvni suma  (k+1)
           sum1 = sum1 + A(i,j)*x1(j);
        end
        sum2 = 0;
        for j = (i+1):n % druh� suma   (k)
           sum2 = sum2 + A(i,j)*x0(j); 
        end
        x1(i) = (b(i)-sum1-sum2)/A(i,i);  
    end
        x0 = x1;
    end
end
