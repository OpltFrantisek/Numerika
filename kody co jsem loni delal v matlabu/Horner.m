function y = Horner(a,x)
    n = length(a);
    y = a(1);
    for i = 2:n  
       y = y*x + a(i); 
    end
end