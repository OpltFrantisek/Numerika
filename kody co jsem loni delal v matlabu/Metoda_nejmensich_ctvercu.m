function [a,b] = Metoda_nejmensich_ctvercu(x,y,p)
    n = length(x);
    A = zeros(p+1,p+1);
    b = zeros(1,p+1);
    for k = 1:n
       for i = 1:(p+1)
          for j = 1:(p+1)
             A(i,j) = A(i,j) + x(k)^((2*p+2)-(i+j)); 
          end                
          b(i) = b(i)+y(k)*x(k)^(p+1-i);
       end
    end
    ab = Gauss_Seidel_method(A,b);
    xplot = min(x):(max(x)-min(x))/1000:max(x);
    yplot = zeros(1,length(x));
    k = 1;
    for i = min(x):(max(x)-min(x))/1000:max(x)
        yplot(k) = Horner(ab,i);
        k = k +1;
    end 
    plot(x,y,'*');hold on;
    plot(xplot,yplot,'color','r');
end
