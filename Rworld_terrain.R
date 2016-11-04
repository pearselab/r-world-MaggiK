###Rworld terrain exercise
#creating an empty matrix
#create a matrix with x rows and y columns
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  return(mat)
}
mat<- mat_func(5,5)
x<-5
y<-5
  #creating the corners of the matrix
upl<- round(rnorm(1, mean=3), digits=2) #starting height for upper left
upr<- round(rnorm(1, mean=3), digits=2) #starting height for upper right
lol<- round(rnorm(1, mean=3), digits=2) #starting height for lower left
lor<- round(rnorm(1, mean=3), digits=2) #starting height for lower right
#adding the values to the matrix
mat[1,1]<- upl #upper left
mat[1,y]<- upr #upper right
mat[x,1]<- lol #lower left
mat[x,y]<- lor #lower right

#checking out the matrix with corners
mat

###Diamond Step: 
#creating the center point. This is a function of the matrix. 
diamond_step<-function(mat){
  x<- ncol(mat) #number of columns in the matrix
  y<- nrow(mat) #number of rows in the matrix
  #defining the matrix corners
  ul<- mat[1,1] #upper left
  ur<- mat[1,y] #upper right
  ll<- mat[x,1] #lower left
  lr<- mat[x,y] #lower right
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
  ul<- mat[1,1] #upper left
  ur<- mat[1,y] #upper right
  ll<- mat[x,1] #lower left
  lr<- mat[x,y] #lower right
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
mat<-diamond_square_step(mat)
mat


#upper left
x=5
y=5
med_x<- median(1:x)
med_y<- median(1:y)
mat1<- diamond_square_step(mat[1:med_x, 1:med_y])
mat1
mat
#upper right
x=5
y=5
med_x<- median(1:x)
med_x
med_y<- median(1:y)
med_y
len_x<- ncol(mat)
len_x
len_y<- nrow(mat)
mat2<- diamond_square_step(mat[1:med_x, med_y:len_y])
mat2

#combine the two matrices
m1_m2<-cbind(mat1, mat2)

#lower left matrice
x=5
y=5
med_x<- median(1:x)
med_x
med_y<- median(1:y)
med_y
len_x<- ncol(mat)
len_x
len_y<- nrow(mat)
mat3<- diamond_square_step(mat[med_y:len_y, 1:med_x])
mat3
mat

#lower right matrice
x=5
y=5
med_x<- median(1:x)
med_x
med_y<- median(1:y)
med_y
len_x<- ncol(mat)
len_x
len_y<- nrow(mat)
mat4<- diamond_square_step(mat[med_y:len_y, med_x:len_x])
mat4

m3_m4<-cbind(mat3, mat4)
full_matr<-rbind(m1_m2, m3_m4)
