function Puleni_Intervalu(a,b,presnost)
    
while abs(b-a)> presnost
    s = (a+b)/2; % spoèítáme s(k+1) jako støen intervalu <a,b>
    fs = Func(s); % funkèní hodnota v bodì s(k+1)
    if Func(a)*fs<0 % f(a(k))*f(s(k+1)) < 0 pak a(k+1) = a(k), b(k+1) = s(k+1)
        b = s;
    elseif fs*Func(b)<0 % f(b(k))*f(s(k+1)) < 0 pak a(k+1) = a(k+1), b(k+1) = b(k)
        a = s;
    else
        disp('reseni nenalezeno');
        break;
    end
end
disp(a);
disp(b);
end

function y = Func(x)
    y = x*x*x-x*x-2*x+2; 
end
% testováno na funkci x^3-*x^2-2*x+2, 
%Puleni_Intervalu(((1-sqrt(7))/3)-1,((1-sqrt(7))/3)-0.8,0.0001) 