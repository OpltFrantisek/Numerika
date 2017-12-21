function Interpolace_Polynomem(x,y) 
    n = length(x); 
    A = zeros(n,n); % init matice A
    for i = 1:n % cyklus pres vsechny radky matice
       for j = 1:n %cyklus pres vsechny sloupce matice
          A(i,j) = x(i)^(j-1);  % vypocet 
       end
    end
    
    koef = Gausova_Eliminacni_metoda(A,y); % reseni soustavy rovnic
   % koef2 = Gauss_Seidel_method(A,y)
    %[D l u] = LU_Rozklad(A);
   % koef2 = gauss_seidel_LU(u,l,D,y,0.001,100)
    koef = fliplr(koef) % otoci vyslekdy
    % vykresleni
    xplot =  min(x):(max(x)-min(x))/1000:max(x); % navzorkovani osy x
    yplot = zeros(1,length(x));
    k = 1;
    
    for i = min(x):(max(x)-min(x))/1000:max(x)  % dopocitani funkcnich hodnout v podech xplot
        yplot(k) = Horner(koef,i);
        k = k +1;
    end 
    plot(x,y,'*');hold on;
    plot(xplot,yplot,'color','r'); 
    % konec vykresleni
end
