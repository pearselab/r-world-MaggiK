###Rworld terrain exercise
#creating an empty matrix
#' Rworld terrain.
#'
#' @author Maggi Kraft
#' @description This code makes an elevational grid with negative numbers as lakes/water. This matrix code only works with matrices of size 5x5 and 9x9. Don't do it any other size matrix. This code was created for the programming for biologists course in fall of 2016.
#'
#'  It uses three-ish functions.
#'    First the mat_function to create a matrix with corners filled in
#'    second the diamond step function
#'    third the square step function
#'    fourth the wrapper for everything
#'
#' @param x is number of columns
#' @param y is number of rows
#' @return a matrix

#create a matrix with x rows and y columns
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  upl<- rnorm(1, mean=2) #starting height for upper left
  upr<- rnorm(1, mean=2) #starting height for upper right
  lol<- rnorm(1, mean=2) #starting height for lower left
  lor<- rnorm(1, mean=2) #starting height for lower right
  #adding the values to the matrix
  mat[1,1]<- upl #upper left
  mat[1,y]<- upr #upper right
  mat[x,1]<- lol #lower left
  mat[x,y]<- lor #lower right
  return(mat)
}

#' This is the diamond step portion
#' @param mat or matrix created above is the first parameter
#' @return diamond step function
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
  mat[med_y, med_x]<- (md+rnorm(1))  #adding the mean of the four corners to the middle of the matrix
  return(mat)
}

#' This is the square step function
#' @param mat is the only input. mat is the matrix created in the matrix function

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
  md <- (mean(mn_vector)+rnorm(1))
  #defing the middle of the four corners so it knows where to insert the mean (md) value
  med_x= median(1:x)
  med_y= median(1:y)
  mat[1, med_y]<- mean(c(ul,ur, md)) #middle upper
  mat[med_x, 1]<- mean(c(ul, md, ll)) #middle left
  mat[med_x, y]<- mean(c(ur, md, lr)) #middle right
  mat[x, med_y]<- mean(c(ll, md, lr)) #middle bottom
  return(mat)
}

##Creating the diamond_square_step: Function calls the two above functions and repeats them for each smaller square.
#' Diamond_square_step combines the diamond and square step functions
#' @param  x =number col
#' @param and y= rows
#' @return diamond_square_step
diamond_square_step<- function(x,y){
  mat<- mat_func(x,y)
  mat<-diamond_step(mat)
  mat<- square_step(mat)
  for (i in ncol(mat)){
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

###wrapper function
#' wrapper function: make.terrain is a wrapper for diamond_square_step(mat, x, y). Change negative numbers to NA to represent water. The output is an image.
#' @param x is the number of columns in the matrix
#' @param y is the number of rows in the matrix
#' @return image of the terrain
#' @export
make.terrain<- function(x, y){
  mat<- diamond_square_step(x, y)
  mat[mat<0] <- NA
  image(mat)
}
make.terrain(9, 9)

