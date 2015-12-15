## ------------------------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

a<-c(1,1,3,3,3,4,5,5,1,1)
b<-c(1:10,1,2,2,3,1,1)
c<-c(1,2,3,1,2,3,5,5,1,2,1,1:20, 1,0,2,5)
d<-c(1:10,10,NA)
e<-c(1:10,1:100,NA,1:50)

## ------------------------------------------------------------------------
system.time( replicate(10000, home6::mode(a) ) )
system.time( replicate(10000, Mode(a) ) )

system.time( replicate(10000, home6::mode(b) ) )
system.time( replicate(10000, Mode(b) ) )

system.time( replicate(10000, home6::mode(c) ) )
system.time( replicate(10000, Mode(c) ) )

system.time( replicate(10000, home6::mode(d) ) )
system.time( replicate(10000, Mode(d) ) )

system.time( replicate(10000, home6::mode(e) ) )
system.time( replicate(10000, Mode(e) ) )

## ------------------------------------------------------------------------
a<-list(c(1,2,3), c(1,2,8))
b<-list(c(1:100), c(1:100))
c<-list(c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8),c(1,2,3), c(1,2,8) )
d<-lapply(c(1:10), function(x) {c(1:10)} )


## ------------------------------------------------------------------------
system.time( replicate(10000, home6::simplify2array(a) ) )
system.time( replicate(10000, base::simplify2array(a) ) )

system.time( replicate(10000, home6::simplify2array(b) ) )
system.time( replicate(10000, base::simplify2array(b) ) ) 

system.time( replicate(10000, home6::simplify2array( c)))
system.time( replicate(10000, base::simplify2array(c)))

system.time( replicate(10000, home6::simplify2array(d) ) )
system.time( replicate(10000, base::simplify2array(d) ) )

## ------------------------------------------------------------------------
library(combinat)
Ass<-function(x){
  m<-t(combn(2*x,2))
  size<-length(m[,1])
  out<-matrix(0,nrow = size,ncol=2*x);
  for(y in c(1:size)){
    out[y,][m[y,]]<-c(1,1)
  }
  return(out)
}
a<-2
b<-5
c<-7


## ------------------------------------------------------------------------
system.time( replicate(1000, home6::ass(a) ) )
system.time( replicate(1000, Ass(a) ) )

system.time( replicate(1000, home6::ass(b) ) )
system.time( replicate(1000, Ass(b) ) ) 

system.time( replicate(1000, home6::ass( c)))
system.time( replicate(1000, Ass(c)))

