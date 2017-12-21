function neco = Lagrange(x,y)
  n = length(x);
  A = zeros(n,n);
  A(:,1) = ones(1,n);
  for i = 1:n
    for j = 2:n
      A(i,j) = A(i,j-1)*x(i);
    end
  end
  xplot = min(x):(max(x)-min(x))/1000:max(x);
  a = Gausova_Eliminacni_metoda(A,y);
  ya = Horner2(a,x);
  A
end
function y = Horner2(a,x)
  n = length(a);
  y = a(n);
  for i = (n-1):-1:1
    y = y*x+a(i);
  end
end