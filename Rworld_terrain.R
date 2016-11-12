###Rworld terrain exercise
#creating an empty matrix
#create a matrix with x rows and y columns
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  return(mat)
}
mat<- mat_func(17,17)
x<-17
y<-17
  #creating the corners of the matrix
upl<- rnorm(1, mean=2) #starting height for upper left
upr<- rnorm(1, mean=2) #starting height for upper right
lol<- rnorm(1, mean=2) #starting height for lower left
lor<- rnorm(1, mean=2) #starting height for lower right
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
for (i in 1:ncol(mat)){
    med_x<- median(1:i)
    med_y<- median(1:i)
    len_x<- ncol(mat)
    len_y<- nrow(mat)
    mat[1:med_x, 1:med_y]<- diamond_step(mat[1:med_x, 1:med_y])
    mat[1:med_x, med_y:len_y]<- diamond_step(mat[1:med_x, med_y:len_y])
    mat[med_y:len_y, 1:med_x]<- diamond_step(mat[med_y:len_y, 1:med_x])
    mat[med_y:len_y, med_x:len_x]<- diamond_step(mat[med_y:len_y, med_x:len_x]) 
    mat[1:med_x, 1:med_y]<- square_step(mat[1:med_x, 1:med_y])
    mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])
    mat[med_y:len_y, 1:med_x]<- square_step(mat[med_y:len_y, 1:med_x])
    mat[med_y:len_y, med_x:len_x]<- square_step(mat[med_y:len_y, med_x:len_x])
    for (j in 1:nrow(mat)){
      med_x<- median(1:j)
      med_y<- median(1:j)
      len_x<- ncol(mat)
      len_y<- nrow(mat)
      mat[1:med_x, 1:med_y]<- diamond_step(mat[1:med_x, 1:med_y])
      mat[1:med_x, med_y:len_y]<- diamond_step(mat[1:med_x, med_y:len_y])
      mat[med_y:len_y, 1:med_x]<- diamond_step(mat[med_y:len_y, 1:med_x])
      mat[med_y:len_y, med_x:len_x]<- diamond_step(mat[med_y:len_y, med_x:len_x]) 
      mat[1:med_x, 1:med_y]<- square_step(mat[1:med_x, 1:med_y])
      mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])
      mat[med_y:len_y, 1:med_x]<- square_step(mat[med_y:len_y, 1:med_x])
      mat[med_y:len_y, med_x:len_x]<- square_step(mat[med_y:len_y, med_x:len_x])
    }
}
  for (i in seq(from=1, to=(ncol(mat)-2), by=2)){
    mat[i:(i+2), i:(i+2)]<-diamond_step(mat[i:(i+2), i:(i+2)])
    mat[i:(i+2), i:(i+2)]<-square_step(mat[i:(i+2), i:(i+2)])
    for (j in seq(from=1, to=(nrow(mat)-2), by=2)){
      mat[j:(j+2), i:(i+2)]<-diamond_step(mat[j:(j+2), i:(i+2)])
      mat[j:(j+2), i:(i+2)]<-square_step(mat[j:(j+2), i:(i+2)])
    }
  }
  return(mat)
}

mat<-diamond_square_step(mat)
mat

