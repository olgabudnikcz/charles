library(geomorph)
library(corrplot)
library(viridis)
library(abind)
library(Morpho)


# custom functions
## calculating distinctiveness (distance from the mean)
DIST <- function (x) {
  output<-rep(0,dim (x) [3])
  for (i in 1:dim (x) [3])
  {
    input<-(x)[,,i]
    output[i]<-(sum(rowSums((input-mshape(x))^2)))^0.5
  }
  output
}

## calculating the level of asymmetry for faces 

ASYM <- function (A) {
  output<-rep(0,dim (A) [3])
  for (i in 1:dim (A) [3])
  {
    X<-as.data.frame(two.d.array(A))
    Xr <- X*matrix (rep(c(-1,1), each=nrow(X),times=ncol(X)/2), nrow = nrow(X), ncol = ncol (X))
    
    Xr <- as.data.frame (Xr)
    names(Xr)
    coor<-Xr
    
    # extract numbers from column names
    cisla <- as.numeric(unlist(regmatches(names(coor), gregexpr("[[:digit:]]+", names(coor)))))
    cisla2 <- cisla # copy
    
    # re-label
    left <- c(50,51,52,53,54,55,56,57,58,5,7,12,10,70,15,17,18,20,26,23,21,22,21,25,24,71,35,46,45,44,34,49,48,47)
    right <- c(59,60,61,62,63,64,65,66,67,4,6,11,9,69,14,16,19,27,33,29,28,30,28,32,31,72,37,38,39,40,36,41,42,43)
    
    # extract numbers from column names
    cisla <- as.numeric(unlist(regmatches(names(coor), gregexpr("[[:digit:]]+", names(coor)))))
    cisla2 <- cisla # copy
    
    # exchange
    pozice <- match(cisla,left)[-which(is.na(match(cisla,left)))]
    pozice2 <- match(cisla,right)[-which(is.na(match(cisla,right)))]
    
    cisla[-which(is.na(match(cisla,left)))] <- right[pozice]
    cisla[-which(is.na(match(cisla2,right)))] <- left[pozice2]
    cisla
    
    # ordering
    poradi <- order(cisla)
    
    sloupec <- paste(cisla,c(".X",".Y"),sep="")
    names(coor) <- sloupec
    coor<-coor[,poradi]
    
    
    origos <- (X) [i,]
    reflect <- (coor) [i,]
    
    output [i]<-sqrt(sum((origos-reflect)^2))
  }
  output
}


##  calculate group-relevant shapescores 

giveGroupVec <- function(coord,groupvar,iter = 99){
  
  dframe <- geomorph.data.frame(coords = coord, group = groupvar)
  reg1 <- procD.lm(coords ~ group, iter = iter, data = dframe)
  
  print(summary(reg1))
  
  coefficients<-coef(reg1, test = F)
  
  scores <- two.d.array (coord) %*% t(coefficients)
  sc <- scores [,2]
  
  groupvar <- as.numeric(as.factor(groupvar))
  
  g1sc <- subset (sc, groupvar == 1)
  g2sc <- subset (sc, groupvar == 2)
  
  gcoef<-t(coefficients)[,2]
  
  return(list(sc=sc, g1sc=g1sc, g2sc=g2sc, gcoef=gcoef))
}


# gridpar-graphics specifications

GP1<-gridPar(pt.bg = "red", pt.size = 0.2, link.col = "blue", link.lwd = 1.5, link.lty = 1, out.col = "gray", out.cex = 0.1, tar.pt.bg = "black", tar.pt.size = 0.2, tar.link.col = "black", tar.link.lwd = 2, tar.link.lty = 1, tar.out.col = "black", tar.out.cex = 0.1, n.col.cell = 15, grid.col = "grey65", grid.lwd = 1, grid.lty = 1, txt.adj = 0.5, txt.pos = 1, txt.cex = 0.8, txt.col = "black")

GP2<-gridPar(pt.bg = "red", pt.size = 0.5, link.col = "blue", link.lwd = 3, link.lty = 1, out.col = "gray", out.cex = 0.1, tar.pt.bg = "black", tar.pt.size = 0.5, tar.link.col = "black", tar.link.lwd = 3, tar.link.lty = 1, tar.out.col = "black", tar.out.cex = 0.1, n.col.cell = 20, grid.col = "grey65", grid.lwd = 1.5, grid.lty = 1, txt.adj = 0.5, txt.pos = 1, txt.cex = 0.8, txt.col = "black")



# extracting p-values from the models

pval <- function(B) {
  f <- (B)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


