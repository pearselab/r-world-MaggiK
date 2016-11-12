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

##making a matrix
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  return(mat)
}
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
names<-c("a","b","c")
rownames(c_mat)<-names
colnames(c_mat)<-names

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
              names=names))
}
info<-setup.plants(repr, surv, c_mat)
info

###Suvival
survive <- function(cell, info){
  if(is.na(cell)) 
    return(NA)
  if (cell=='')
    return('')
  if(runif(1) <= info$survive[cell])
    return(cell)
  if(runif(1) >= info$survive[cell])
    return('')
}

#timesteps:assign a number
timesteps<-3
#setupmy array
plants <- array("", dim=c(dim(terrain),timesteps+1))
  #seeding the first part of the array
  plants[1,1,1]<- "a"
  plants[1,2,1]<-"b"
  plants[1,3,1]<-""
  plants[2,1,1]<-"a"
  plants[2,2,1]<-""
  plants[2,3,1]<-"b"
  plants[3,3,1]<-""
  plants[3,2,1]<-"c"
  plants[3,1,1]<-"a"
#setting the NA's
for(k in seq_len(dim(plants)[3]))
    plants[,,k][is.na(terrain)] <- NA
  
###plant.timestep
plant.timestep <- function(plants, info){
  survive <- function(cell, info){
    if(is.na(cell))
      return(NA)
    if (cell=='')
      return('')
    if(runif(1) <= info$survive[cell])
      return(cell)
    if(runif(1) >= info$survive[cell])
      return('')
  }
  #the timestep: k dimensions, i rows, and j columns
  for (k in 1:(dim(plants)[3]-1)){
    for (i in 1:(dim(plants)[1])){
      for (j in 1:(dim(plants)[2])){
           plants[i,j,(k+1)]<- survive(plants[i,j,k], info)
     }
    }
  }
  return(plants[,,k])
  }

plant.timestep(plants, info)
plants 



###run.plant.ecosystem
run.plant.ecosystem<-function(plants, info, timesteps){
  plants <- array("", dim=c(dim(terrain),timesteps+1))
  #seeding the first part of the array
  plants[1,1,1]<- "a"
  plants[1,2,1]<-"b"
  plants[1,3,1]<-""
  plants[2,1,1]<-"a"
  plants[2,2,1]<-""
  plants[2,3,1]<-"b"
  plants[3,3,1]<-""
  plants[3,2,1]<-"c"
  plants[3,1,1]<-"a"
  #setting the NA's
  for(k in seq_len(dim(plants)[3]))
    plants[,,k][is.na(terrain)] <- NA
  plants<- plant.timestep(plants, info)
  return(plants)
}
run.plant.ecosystem(plants, info, 3)

###Reproduction

reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  #figure out the locations in i rows, and j col
  for (i in possible.locations){
    for(j in possible.locations){
      #if it is not NA pick a plant to reproduce given the reporduction probability
      if(!is.na(possible.locations)){
        if(runif(1)<= info$reproduce[plant]){
          plants[i,j]<- info$names[plants]
        }
      }
    }
  }
  return(plants)
}
  #...now filter out which ones are not water-logged and reproduce there...
  #...being careful to check you do have somewhere to reproduce to!...

plants <- reproduce(row, column, plants, info)

###Competition
sample(species_names, 1, prob=comp.mat[row,column])

