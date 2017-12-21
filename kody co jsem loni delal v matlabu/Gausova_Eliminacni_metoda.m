function x = Gausova_Eliminacni_metoda(A,b)
%A = [1 -2 3 1; 2 -3 1 -2;1 1 1 1;3 2 5 -1];
%b = [-5;-7;3;1];
n = length(A(1,:)); % poèet sloupcù matice
m = length(A); % poèet øádkù matice
for i = 1:(m-1)
    for j = (i+1):m
        koef = -A(j,i)/A(i,i); %výpoèet koeficientu
        A(j,i:n) = A(j,i:n) + koef*A(i,i:n);
        b(j) = b(j)+koef*b(i);
    end
end
x = zeros(1,m); % vytvoøí nulový vektor do kterého se budou ukládat výsledky
x(m) = b(m)/A(m,m); % spoèítání poslední neznámé
for i = (m-1):-1:1 % zpìtný chod (dopoèítáváme x)
    x(i) = (b(i)-sum(A(i,(i+1):m).*x((i+1):m)))/A(i,i);
end
end
