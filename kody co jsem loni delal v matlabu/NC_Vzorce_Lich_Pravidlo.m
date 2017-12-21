function I = NC_Vzorce_Lich_Pravidlo(a,b)
    x = a:(b-a)/50:b ;% rozdělení intervalu <a,b> na více podintervalů
    n = length(x); % pocet uzlů
    I = 0;
    fx0 = Func(a); % funkcni hodnota v bode a
    fxn = Func(b); % funkcni hodnota v bode b
    sum =0;
    for i = 2:(n-1) % výpočet suma f(x(i)) 
      sum = sum + Func(x(i));
    end
    I = ((b-a)/n)*(fx0/2+sum+fxn/2); % výpocet integrálu
    disp(I);
end

function y = Func(x)
 y = x^3;
end