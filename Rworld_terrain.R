###Rworld terrain exercise
#creating an empty matrix
#create a matrix with x rows and y columns
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  return(mat)
}
mat<- mat_func(5,5)
  #creating the corners of the matrix
ul<- round(rnorm(1, mean=3), digits=2) #starting height for upper left
ur<- round(rnorm(1, mean=3), digits=2) #starting height for upper right
ll<- round(rnorm(1, mean=3), digits=2) #starting height for lower left
lr<- round(rnorm(1, mean=3), digits=2) #starting height for lower right
#adding the values to the matrix
mat[1,1]<- ul #upper left
mat[1,y]<- ur #upper right
mat[x,1]<- ll #lower left
mat[x,y]<- lr #lower right

#checking out the matrix with corners
mat

###Diamond Step: 
#creating the center point. This is a function of the matrix. 
diamond_step<-function(mat){
  x<- ncol(mat) #number of columns in the matrix
  y<- ncrow(mat) #number of rows in the matrix
  #defining the matrix corners
  mat[1,1]<- ul #upper left
  mat[1,y]<- ur #upper right
  mat[x,1]<- ll #lower left
  mat[x,y]<- lr #lower right
  mn_vector<- c(ul,ur,ll,lr) #addingt the corner values into a vector to take the mean
  md <- mean(mn_vector) #md: mean of the four corners
  #defing the middle of the four corners so it knows where to insert the mean (md) value
  med_x= median(1:x)
  med_y= median(1:y)
  mat[med_y, med_x]<- md  #adding the mean of the four corners to the middle of the matrix
  return(mat) 
}
mat<-diamond_step(mat)
mat

#note to self: return in function- only works for one value. I cant do return() then return()

####Square.Step: creating the side points
square_step<-function(mat){
  x<- ncol(mat) #number of columns in the matrix
  y<- nrow(mat) #number of rows in the matrix
  #defining the matrix corners
  mat[1,1]<- ul #upper left
  mat[1,y]<- ur #upper right
  mat[x,1]<- ll #lower left
  mat[x,y]<- lr #lower right
  mn_vector<- c(ul,ur,ll,lr) #addingt the corner values into a vector to take the mean
  #md: mean of the four corners
  md <- mean(mn_vector)
  #defing the middle of the four corners so it knows where to insert the mean (md) value
  med_x= median(1:x)
  med_y= median(1:y)
  mat[1, med_y]<- mean(c(ul,ur, md)) #middle upper
  mat[med_x, 1]<- mean(c(ul, md, ll)) #middle left
  mat[med_x, y]<- mean(c(ur, md, lr)) #middle right
  mat[x, med_y]<- mean(c(ll, md, lr)) #middle bottom
  return(mat)
}

mat<-square_step(mat)
mat

##Creating the diamond_square_step: Function calls the two above functions and repeats them for each smaller square. 
diamond_square_step<- function(mat){
  mat<-diamond_step(mat)
  mat<- square_step(mat)
  return(mat)
}

mat<- diamond_square_step(mat)
mat



#diamond
x<- ncol(mat) #number of columns in the matrix
y<- nrow(mat) #number of rows in the matrix

#right square
mat[1,3]<- ul #upper left
mat[1,y]<- ur #upper right
mat[3,3]<- ll #lower left
mat[3,5]<- lr #lower right
mn_vector<- c(ul,ur,ll,lr) #addingt the corner values into a vector to take the mean
md <- mean(mn_vector) #md: mean of the four corners

#defing the middle of the four corners so it knows where to insert the mean (md) value
med_x= median(3:5)
med_y= median(1:3)
mat[med_y, med_x]<- md  #adding the mean of the four corners to the middle of the matrix
mat


#square
mat[1, med_y]<- mean(ul,ur, md) #middle upper
mat[med_x, 1]<- mean(ul, md, ll) #middle left
mat[med_x, y]<- mean(ur, md, lr) #middle right
mat[x, med_y]<- mean(ll, md, lr) #middle bottom
mat

#smaller squares
