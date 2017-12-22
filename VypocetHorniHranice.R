# Title     : TODO
# Objective : TODO
# Created by: Fafik
# Created on: 20.12.2017
# integrál se počítá Simpsonovo metodou, odhad horni hranice intervalu je pomocí půlení intervalu

funkce <- function(x){
    return(sin(x))
}

integruj <- function(dolniInterval, horniInterval, f, n){
    h = (horniInterval - dolniInterval)/n

    x = seq(dolniInterval,horniInterval,h)
    y = f(x)
    a = 0
    b = 0
    for(i in 2:n){
        if(i%%2 > 0){
            a = a+y[i]
        }
        else{
            b = b +y[i]
        }
    }
    a = 4*a
    b = 2*b

    I = (1/3)*((horniInterval - dolniInterval)/n)*(y[1]+a+b+y[n+1])
    return(I)
}


dolnihranice <- 0
stred <- dolnihranice
odhadHorniHranice <- 3
presnost <- 0.000001
hodnotaIntegralu <- 1
i <- integruj(dolnihranice,odhadHorniHranice,funkce,200000)
while(abs(i - hodnotaIntegralu) > presnost ){
    print(i)
    zmena <- abs((odhadHorniHranice - stred)/2)

    print(zmena)
    if(i - hodnotaIntegralu > 0 ){
        odhadHorniHranice <- odhadHorniHranice - zmena

    }
    else{
        stred <- odhadHorniHranice
        odhadHorniHranice <- odhadHorniHranice+zmena
    }

    i <- integruj(dolnihranice,odhadHorniHranice,funkce,200000)
    i
}
