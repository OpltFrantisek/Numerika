function x = Gausova_Eliminacni_metoda(A,b)
%A = [1 -2 3 1; 2 -3 1 -2;1 1 1 1;3 2 5 -1];
%b = [-5;-7;3;1];
n = length(A(1,:)); % po�et sloupc� matice
m = length(A); % po�et ��dk� matice
for i = 1:(m-1)
    for j = (i+1):m
        koef = -A(j,i)/A(i,i); %v�po�et koeficientu
        A(j,i:n) = A(j,i:n) + koef*A(i,i:n);
        b(j) = b(j)+koef*b(i);
    end
end
x = zeros(1,m); % vytvo�� nulov� vektor do kter�ho se budou ukl�dat v�sledky
x(m) = b(m)/A(m,m); % spo��t�n� posledn� nezn�m�
for i = (m-1):-1:1 % zp�tn� chod (dopo��t�v�me x)
    x(i) = (b(i)-sum(A(i,(i+1):m).*x((i+1):m)))/A(i,i);
end
end
