function Jacobi_method(A,b)
    x = b; % odhad prvn�ch x je vektor b (m��ou tu b�t libovoln� ��sla)
    n = length(b); % po�et nezn�m�ch
    for i = 1:100 % pocet iterac        i
       x = Vzorec(n,x,A,b); 
    end
    disp(x);
end
function x = Vzorec(n,x,A,b)
    for i = 1:n
       x(i) = b(i);
        sum = 0;
        for j = 1:n % v�po�et sumy
           if j ~= i
              sum = sum + A(i,j)*x(j);
           end
        end
        x(i) = (b(i)-sum)/A(i,i); % x(i) = 1/A(i,i) * (b(i) - suma)
    end
end
