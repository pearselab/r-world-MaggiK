###programming for biologists
###Herbivores

#sated= time to live (value of 5 if it just ate). If it doesn't eat the value goes down. When it does eat it goes back up to 5. 0 is dead
#overall the same as the plants. 

###making a matrix
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

#only one species so only one reproduction, eat, and kill probability??

#probability of reproducing when they eat
reproduce_herb<- c(.4)
reproduce_herb

#eat is a vector of the probability of eating
eat<-c(.6)
#whether a herbivore feeds or not is related to how long since they last fed. 

#probability of killing a plant
kill<-c(.5)

#names
names<-c("d")

###checking life history parameters
setup.herb <- function(reproduce, eat, kill, name=NULL){
  if(is.null(name))
    name <- letters[seq_along(reproduce)]
  if(length(reproduce) != length(eat))
    stop("Reproduction and survival parameters needed for all species")
  reproduce <- setNames(reproduce, names)
  eat <- setNames(eat, names)
  kill <- setNames(kill, names)
  return(list(reproduce=reproduce, eat=eat, kill=kill,
              names=names))
}
herb_info<-setup.herb(reproduce_herb, eat, kill)
herb_info

#timesteps:assign a number
timesteps<-3
#setupmy array values are between 0 and 5 where 0 is a dead herbivore and 5 is one who just ate
herb <- array(dim=c(dim(terrain),timesteps+1))
#seeding the first part of the array
herb[1,1,1]<- 1 
herb[1,2,1]<- 2
herb[1,3,1]<- 3
herb[2,1,1]<- 4
herb[2,2,1]<- 1
herb[2,3,1]<- 2
herb[3,3,1]<- 5
herb[3,2,1]<- 3
herb[3,1,1]<- 4
#setting the NA's
for(k in seq_len(dim(herb)[3]))
  herb[,,k][is.na(terrain)] <- NA
herb

# in each timestep an herbivore can either move or eat, and if it eats it may reproduce
#herbifores likelihood of eating (and not moving) is determined by how long it can go until feeding. Subset eat, pull out the relevant probability for that herbivore. eat[herbivore[row,col]]. If it eats, the value in the matrix is changed to 5 (sated). 

#start with a survival function for herbivores. If the cell is NA then it stays NA. if blank, it stays blank, if the probability of eating is less than a random number then return 5 if greater than a random number then return the value of the cell -1. 
herb_eat <- function(cell, info){
  if(is.na(cell)) 
    return(NA)
  if (cell=='')
    return('')
  if(runif(1) <= herb_info$eat)
    return(5)
  if(runif(1) >= herb_info$eat)
    return(cell-1)
}

#testing out survival
herb_eat(herb[1,3,1], herb_info)
herb
herb[3,2,3]<-herb_eat(herb[3,2,2], herb_info)
herb
###Loop through the different timesteps (dimensions). If the herbivore ate (cell value is 5 at k+1) then it may reproduce.
for (k in 1:(dim(herb)[3]-1)){
  for (i in 1:(dim(herb)[1])){
    for (j in 1:(dim(herb)[2])){
      herb[i,j, (k+1)]<- herb_eat(herb[i,j,k], herb_info)
    }
  }
}

herb

if(herb[i,j,(k+1)] == 5){
  #it can reproduce to possible locations
}
###The herbivore can move to possible locations

