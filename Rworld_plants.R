##RWorld Plants
##Programming for biologists
##11/07/2016
##use an array/matrix (wrapper for psecies properties)
#for loop involved in survival steps.
#survive or die 
#reproduce->disperse 

#repro(based on number spp), suvive (0-1), comp.mat(matrix if you have 3 spp, then 3 rows and 3 col)

#survive: is probability (0-1)
#repro: vector that is the same length of the matrix (each one needs a reproduction prob 0-1)
#comp.mat:matrix of spp. number of rows or columns reflects the number of species

##making a comp.matrix
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  return(mat)
}
c_mat<- mat_func(3,3)
c_mat[1,1]<-.5
c_mat[1,2]<-.2
c_mat[1,3]<-.4
c_mat[2,1]<-.6
c_mat[2,2]<-.5
c_mat[2,3]<-.3
c_mat[3,3]<-.5
c_mat[3,2]<-.4
c_mat[3,1]<-.8
c_mat

#repro
repr<- c(.3,.6,.8)
repr

#survival
surv<-c(.2,.5,.6)

###checking life history parameters
setup.plants <- function(repro, survive, comp.mat, names=NULL){
  if(is.null(names))
    names <- letters[seq_along(repro)]
  if(length(repro) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
  if(length(repro) != ncol(comp.mat))
    stop("Reproduction and competition matrix needed for all species")
  if(ncol(comp.mat) != nrow(comp.mat))
    stop("number of col needs to match number of rows")
  repro <- setNames(repro, names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              names=names))
}
setup.plants(repr, surv, c_mat)

###Suvival
survive <- function(cell, info){
  #...some code to check whether cell is empty or has water...
  if(runif(1) <= info$survive[plant])
    #$The plant survived! so do something...
}


