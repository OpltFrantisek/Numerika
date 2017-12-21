function Newtonova_metoda_tecen(a,b,presnost)
    x = zeros(1,50); % vytvorime si nulovy vector s hodnotami x
    x(1) = a;
    x(2) = b;
    k = 2;
    while abs(x(k)-x(k-1)) > presnost
       x(k+1) = Metoda_Secen(x,k);
       k = k+1;
    end
    disp(x(k));
end
function x_new = Metoda_Secen(x,k) % výpoèe derivace v newtonove metode nahradime aproxomaci derivace 
    x_new = x(k) - (x(k)-x(k-1))/(Func(x(k))-Func(k-1));
end

function y = Func(x)
    y = x*x*x-x*x-2*x+2; 
end
% testováno na funkci x^3-*x^2-2*x+2, 