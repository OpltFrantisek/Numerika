function [x,i]=gauss_seidel_LU(U,L,D,b,tolerance,iter)
    x=b;
    for i = 1:iter
         xn = (L+D)\ (-U*x+b);
         if (xn-x)<tolerance
            break;
         end
         x =xn;
    end
    x;
end