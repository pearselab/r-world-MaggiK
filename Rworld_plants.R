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

##terrain matrix
terr<- mat_func(3,3)
terr[1,1]<-NA
terr[1,2]<-2.2
terr[1,3]<-3.4
terr[2,1]<-.6
terr[2,2]<-.5
terr[2,3]<-.3
terr[3,3]<-NA
terr[3,2]<-4.4
terr[3,1]<-1.8
terr

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

#names
nm<-c("a","b","c")
###checking life history parameters
setup.plants <- function(repro, survive, comp.mat, names){
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
info<-setup.plants(repr, surv, c_mat, nm)
info

###Suvival
survive <- function(cell, info){
  if(is.na(cell)) 
    return(NA)
  if (cell=='')
    return('')
  if(runif(1) <= info$survive[plant])}
    return(plants)
    #$The plant survived! so do something...
}

survive(terr[2,1], info)
ifelse(is.na(terr[2,1]), NA, info$survive)
  
is.na(terr[1,1])


    #$The plant survived! so do something...

###plant.timestep
plant.timestep <- function(plants, terrain, info){
  survive <- function(cell, info){
      if(is.na(cell)) 
        return(NA)
      if (cell=='')
        return('')
      if(runif(1) <= info$survive[plant])} #that plant survival probability
       return(info$survive[plant])
    #$The plant survived! so do something...
  for (i in ncol(survive.matrix)){
    new.plants.matrix<- survive(cell, info)
  }
  return(new.plants.matrix)
}

###run.plant.ecosystem
#timesteps:assign a number
timesteps<-

plants <- array("", dim=c(dim(terrain),timesteps+1))
#...why timesteps+1, do you think?...
for(i in seq_len(dim(plants)[3]))
  plants[,,i][is.na(terrain)] <- NA


