function newtonuv_interpolacni_polynom(x,y)
    n = length(x);
    c = y;
    for k = 2:n
       f = 1;
       P = c(1);
       for i = 1:(k-1)
          f = f*(x(k)-x(i));
          if i < (k-1)
             P = P + c(i+1)*f; 
          end
       end
       c(k) = (c(k)-P)/f;
    end
    koef = fliplr(c)
    
 % vykresleni
    xplot = 0:0.001:7;
    yplot = zeros(1,length(x));
    k = 1;
    
    for i = 0:0.001:7
        yplot(k) = Horner(koef,i);
        k = k +1;
    end 
    plot(x,y,'*');hold on;
    plot(xplot,yplot,'color','r'); 
    % konec vykresleni
end