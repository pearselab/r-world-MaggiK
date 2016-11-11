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
terrain<- mat_func(3,3)
terrain[1,1]<-NA
terrain[1,2]<-2.2
terrain[1,3]<-3.4
terrain[2,1]<-.6
terrain[2,2]<-.5
terrain[2,3]<-.3
terrain[3,3]<-NA
terrain[3,2]<-4.4
terrain[3,1]<-1.8
terrain

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
setup.plants <- function(repro, survive, comp.mat, name=NULL){
  if(is.null(name))
    name <- letters[seq_along(repro)]
  if(length(repro) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
  if(length(repro) != ncol(comp.mat))
    stop("Reproduction and competition matrix needed for all species")
  if(ncol(comp.mat) != nrow(comp.mat))
    stop("number of col needs to match number of rows")
  repro <- setNames(repro, name)
  survive<- setNames(survive, name)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat,
              name=name))
}
info<-setup.plants(repr, surv, c_mat)
info
info$survive[]
info$repro
###Suvival
survive <- function(cell, info){
  if(is.na(cell)) 
    return(NA)
  if (cell=='')
    return('')
  if(runif(1) <= info$survive[name])
    return(name)
  if(runif(1) >= info$survive[name])
    return('')
}

plants[3,3,1]<-survive(plants[3,3,1], info, "c")
plants[,,1]

#plant can be whatever you want


#how does it know survival is attached to that specific plant? Am I supposed to store the survival rate of the plant or the plant name?
#this works if I define which survival rate to put into the function. So suvival needs to be attached to the plant?

###plant.timestep
plant.timestep <- function(plants, info){
  survive <- function(cell, info){
    if(is.na(cell))
      return(NA)
    if (cell=='')
      return('')
    if(runif(1) <= info$survive[name])
      return(name)
    if(runif(1) >= info$survive[name])
      return('')
  }
  for (i in 1:nrow(plants)){
    for (j in 1:ncol(plants)){
      plants.matrix<- survive(plants[i,j], info)
    }
    return(plants.matrix)
  }
}

plant.timestep(plants, info)
  
#how am I supposed to define the criteria in the second function? is new.plants.matrix the array? 

#timesteps:assign a number
timesteps<-3

###run.plant.ecosystem
run.plant.ecosystem<-function(terrain, timesteps, info){
plants <- array("", dim=c(dim(terrain),timesteps+1))  
  for(k in seq_len(dim(plants)[3]))
    plants[,,k][is.na(terrain)] <- NA
    plants[,,k]<- plant.timestep(plants[,,k], info)
}
run.plant.ecosystem(terrain, timesteps, info)
plants

pt <- array("", dim=c(dim(terrain),timesteps+1)) 
for(k in seq_len(dim(pt)[3]))
  pt[,,k][is.na(terrain)] <- NA
  pt[,,k]<- plant.timestep(pt[,,k], info)
pt
#so the array of plants is storing the name of the plants?

###Reproduction
plant <- reproduce(row, column, plants, info)

reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  for(k in seq_len(dim(plants)[3]))
    plants[,,k][is.na(terrain)] <- NA
  for (k in seq_len(dim(plants)[3]))
    plants[,,k]<- plant.timestep(plants[i,j,], info)
  #...now filter out which ones are not water-logged and reproduce there...
  #...being careful to check you do have somewhere to reproduce to!...
  return(plants)
}