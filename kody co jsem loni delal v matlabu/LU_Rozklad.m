function [D l u] = LU_Rozklad(A)
  n = length(A(:,1));
  m = length(A(1,:));
  l = eye(n,m);
  u = zeros(n,m);
  D = zeros(n,m);
  %u(1,:) = A(1,:);
  for j = 1:m
    sum = 0;
    for i = 1:j
      for k = 1:(i-1)
        sum = sum + l(i,k)*u(k,j);
      end
      u(i,j) = A(i,j)-sum;
    end
    for i = (j+1):n
    sum = 0;
      for p = 1:(j-1)
        sum =sum + l(i,p)*u(p,j);
      end
      l(i,j) = (1/u(j,j))*(A(i,j)-sum);
    end
  end
  for i = 1:n
      D(i,i) = A(i,i);
  end
  L = triu(A)
  u
   U = tril(A)
  l
  
 D;
 A
 L*U
 l*u
end