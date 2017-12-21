plot(cos,0,pi/2,col=4)
abline(a=0,b=1)
x0<-1
y0<-x0
for(i in 1:10){
  y1<-cos(x0)
  segments(x0,y0,x0,y1,col=2)
  x1<-y1
  segments(x0,y1,x1,y1,col=2)
  x0<-x1
  y0<-y1
}