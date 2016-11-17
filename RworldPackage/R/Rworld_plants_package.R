##RWorld Plants
##Programming for biologists
##11/18/2016
##use an array/matrix (wrapper for psecies properties)
#for loop involved in survival steps.
#survive or die
#reproduce->disperse
#' Rworld plants.
#'
#' @author Maggi Kraft
#' @description  This uses a survive function and reproduction function to populate an array
#'    It uses five functions.
#'    First the mat_function to create a matrix
#'    second is the setup plants function. This checks and sets up the input data for the rest of the function
#'    third is the survival function. This is used to determine the probablity of surviving to the next timestep
#'    fourth is the reproduction function. This is the probability of reproducing in through time
#'    fifth is the main function. This is the timestep function that includes the survival and reproduction
#'
#'
#' The input data is as follows:
#' repro(based on number spp):vector that is the same length of the matrix (each one needs a reproduction prob 0-1)
#' suvive (0-1): probability of surving to the next timestep
#' comp.mat(matrix if you have 3 spp, then 3 rows and 3 col)
#' survive: is probability (0-1)
#' comp.mat (competition matrix):matrix of spp. number of rows or columns reflects the number of species

#creating the input data
##making a matrix
# mat_func<- function(x, y){
#   mat<- matrix(ncol=x, nrow=y)
#   return(mat)
# }
# ##terrain matrix
# terrain<- mat_func(3,3)
# terrain[1,1]<-NA
# terrain[1,2]<-2.2
# terrain[1,3]<-3.4
# terrain[2,1]<-.6
# terrain[2,2]<-.5
# terrain[2,3]<-.3
# terrain[3,3]<-NA
# terrain[3,2]<-4.4
# terrain[3,1]<-1.8
# terrain
#
#
# c_mat<- mat_func(3,3)
# c_mat[1,1]<-.5
# c_mat[1,2]<-.2
# c_mat[1,3]<-.4
# c_mat[2,1]<-.6
# c_mat[2,2]<-.5
# c_mat[2,3]<-.3
# c_mat[3,3]<-.5
# c_mat[3,2]<-.4
# c_mat[3,1]<-.8
# c_mat
#
# #repro
# repr<- c(.3,.8,.9)
# repr
#
# #survival
# surv<-c(.2,.5,.6)
#
# #names
# names<-c("a","b","c")
# rownames(c_mat)<-names
# colnames(c_mat)<-names

#' @param repro is the probability of reproduction created above
#' @param survival is the probability of surviving created above
#' @param comp.mat is the competition matrix created above
#' @return info which contains all the info/input data
#'
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


#' @param cell is a specific cell
#' @param info the output from setup.plants
#' @return whether a plant survives
#' @export
###Suvival
survive <- function(cell, info){
  r<- runif(1)
  if (is.na(cell)){ cell<- NA
    }else{
  if (cell=='')
    return('')
  if(r <= info$survive[cell])
    return(cell)
  if(r >= info$survive[cell])
    return('')
}
}

#' @param row: the specific row  a plant can reproduce to
#' @param col: the specific col a plant can reproduce to
#' @param plants: the plants array created in the run.ecosystem
#' @param info: the info/input data created in setup.plants
#' @param k: the dimesions/number of timesteps
#' @return whether the plants reproduce
###Reproduction- So a row and col is i an j.
reproduce <- function(row, col, plants, info, k){
  possible.locations<- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  for (loc in 1:nrow(possible.locations)){
    x<- possible.locations[loc,1]
    y<- possible.locations[loc,2]
    p<-runif(1)
    print(p)
    if(x <= ncol(plants) & y <= ncol(plants) & plants[row, col, k]!= "" & !is.na(plants[row,col,k]) & x>0 & y>0){
      if(!is.na(plants[x, y, k]) & plants[x, y, k] == ""){
        if (p <= info$repr[plants[row, col, k]]) {
          plants[x, y, k+1]<- plants[row, col, k]
        }else{
          plants[x,y,k+1]<- plants[x, y, k]}
      }
    }
    for(remov in seq_len(dim(plants)[3]))
      plants[,,remov][is.na(terrain)] <- NA
  }
  return(plants)
}

#' This function steps through time
#' @param plants is the plants array
#' @param info the output from setup.plants functions
#' @return the matrix of results
#'
###plant.timestep
plant.timestep <- function(plants, info){
  survive <- function(cell, info){
    r<- runif(1)
    if (is.na(cell)){ cell<- NA
    }else{
      if (cell==''){
        return('')
      }else{
      if(r <= info$survive[cell])
        return(cell)
      if(r >= info$survive[cell])
        return('')
      }
    }
  }
  #the timestep: k dimensions, i rows, and j columns
  for (k in 1:(dim(plants)[3]-1)){
    for (i in 1:(dim(plants)[1])){
      for (j in 1:(dim(plants)[2])){
        please_work <- survive(plants[i,j,k], info)
        plants[i,j,(k+1)] <- please_work
        print(k)
      }
    }
    for (i in 1:(dim(plants)[1])){
      for (j in 1:(dim(plants)[2])){
        plants<- reproduce(i, j, plants, info, k)
      }
    }
  }
  return(plants)
     }


#' this is the run.plant.ecosystem wrapper for plant.timestep
#' @param plants is the plants array
#' @param info from the setup.plants
#' @param is the number of timestpes
#' @return matrix of results
#' @export
###run.plant.ecosystem
run.plant.ecosystem<-function(plants, info, timesteps, repr, surv, c_mat){
  info<- info<-setup.plants(repr, surv, c_mat)
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
  #run plant.timestep
  plants<- plant.timestep(plants, info)
  return(plants)
}




###Competition
#sample(species_names, 1, prob=comp.mat[row,column])

