function [a,b] = Metoda_nejmensich_ctvercu2(x,y)
    n = length(x);
    nx = 0; % suma x(i) od 1 do n
    nxx = 0;% suma x(i)^2 od 1 do n
    nxy = 0;% suma x(i)*y(i) od 1 do n
    ny = 0;% suma y(i) od 1 do n
    for i = 1:n
       nx = nx + x(i);
       nxx = nxx + x(i)*x(i);
       nxy = nxy + x(i)*y(i);
       ny = ny + y(i);
    end
    A = [nxx nx;nx n];
    b = [nxy;ny];
    ab = Gauss_Seidel_method(A,b);
    xplot = 0:0.01:6;
    yplot = LinearFunction(0:0.01:6,ab(1),ab(2));
    plot(x,y,'*');
    line(xplot,yplot,'color','r');
end
function y = LinearFunction(x,a,b)
    y = a*x+b;
end