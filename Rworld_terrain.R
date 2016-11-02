###Rworld terrain exercise
#creating an empty matrix
matrix<- matrix(ncol=5, nrow=5)
matrix
#creating my corners for the matrix
ul<- round(rnorm(1, mean=3), digits=2) #starting height for upper left
ur<- round(rnorm(1, mean=3), digits=2) #starting height for upper right
ll<- round(rnorm(1, mean=3), digits=2) #starting height for lower left
lr<- round(rnorm(1, mean=3), digits=2) #starting height for lower right
#adding the corners into the matrix
matrix[1,1]<-ul
matrix[1,5]<-ur
matrix[5,1]<-ll
matrix[5,5]<-lr

### First diamond 
#creating the center point
md<- mean(ul, ur, ll, rr)
md<- matrix[3,3]

#creating the side points
matrix[1,3]<- mean(ul,ur,md) 
matrix[3,1]<- mean(ul, md, ll)
matrix[5,3]<- mean(ur,md,lr)
matrix[3,5]<- mean(ll,md,lr)
