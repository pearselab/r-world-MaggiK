###Rworld terrain exercise
#creating an empty matrix
#create a matrix with x rows and y columns
mat_func<- function(x, y){
  mat<- matrix(ncol=x, nrow=y)
  return(mat)
}
mat<- mat_func(9,9)
x<-9
y<-9
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
  for (i in rep(seq(from=1, to=(ncol(mat)-2), by=2), each=3)){
    mat[i:(i+2), i:(i+2)]<-diamond_step(mat[i:(i+2), i:(i+2)])
    mat[i:(i+2), i:(i+2)]<-square_step(mat[i:(i+2), i:(i+2)])
  }
  return(mat)
}

mat<-diamond_square_step(mat)
mat

diamond_square_step<- function(mat){
  mat<-diamond_step(mat)
  mat<- square_step(mat)
  return(mat)
}

#variables needed in other_squares
med_x<- median(1:x)
med_y<- median(1:y)
len_x<- ncol(mat)
len_y<- nrow(mat)

#change into for loop within diamond step. Iterate over the sequence (col?)
#function to fill in the squares
other_squares<-function(mat){
  med_x<- median(1:x)
  med_y<- median(1:y)
  len_x<- x
  len_y<- y
  mat[1:med_x, 1:med_y]<- diamond_square_step(mat[1:med_x, 1:med_y])
  mat[1:med_x, med_y:len_y]<- diamond_square_step(mat[1:med_x, med_y:len_y])
  mat[med_y:len_y, 1:med_x]<- diamond_square_step(mat[med_y:len_y, 1:med_x])
  mat[med_y:len_y, med_x:len_x]<- diamond_square_step(mat[med_y:len_y, med_x:len_x])
return(mat)
}
mat<-other_squares(mat)
mat
x=7
y=7
mat[1:med_x, 1:med_y]<- diamond_square_step(mat[1:med_x, 1:med_y])
mat[1:med_x, med_y:len_y]<- diamond_square_step(mat[1:med_x, med_y:len_y])
mat[med_y:len_y, 1:med_x]<- diamond_square_step(mat[med_y:len_y, 1:med_x])
mat[med_y:len_y, med_x:len_x]<- diamond_square_step(mat[med_y:len_y, med_x:len_x])

mat

diamond_square_step(mat[1:med_x, 1:med_y])
diamond_square_step(mat[1:med_x, med_y:len_y])
diamond_square_step(mat[med_y:len_y, 1:med_x])
diamond_square_step(mat[med_y:len_y, med_x:len_x])

mat
#upper left: start with 9. Then change x and y to 5?
mat[1:med_x, 1:med_y]<- diamond_step(mat[1:med_x, 1:med_y])
mat[1:med_x, 1:med_y]<- square_step(mat[1:med_x, 1:med_y])
mat[1:x, 1:y]
x=3
y=3
med_x<- median(1:x)
med_y<- median(1:y)
mat[1:med_x, 1:med_y]<-diamond_square_step(mat[1:med_x, 1:med_y])
mat

#upper right
mat[1:med_x, med_y:len_y]<- diamond_step(mat[1:med_x, med_y:len_y]) #run the sequence with the initial x and y values for all the corners. THen to run all the smaller squares: need to change something with x and y. 
mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])
#second lap
mat[1:3, 7:9]<- diamond_step(mat[1:3, 7:len_y])
mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])
#third lap
mat[1:3, 5:7]<- diamond_step(mat[1:3, 5:7])
mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])
#fourth lap
mat[3:5, 7:9]<- diamond_step(mat[1:3, 7:len_y])
mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])
#fifth lap
mat[3:5, 5:7]<- diamond_step(mat[1:3, 5:7])
mat[1:med_x, med_y:len_y]<- square_step(mat[1:med_x, med_y:len_y])


x=5
y=5
med_x<- median(1:x)
med_x
med_y<- median(1:y)
med_y
len_x<- ncol(mat)
len_x<-x
len_y<- nrow(mat)
len_y<-y
mat[1:med_x, med_y:len_y]<- diamond_square_step(mat[1:med_x, med_y:len_y])
mat

mat[1:3 ,7:9]<- diamond_square_step(mat[1:3, 7:9])
mat
#lower left matrice
mat[med_y:len_y, 1:med_x]<- diamond_step(mat[med_y:len_y, 1:med_x])
mat[med_y:len_y, 1:med_x]<- square_step(mat[med_y:len_y, 1:med_x])

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
mat[1,1]
mat[med_y:len_y, med_x:len_x]
mat
#lower right matrice
mat[med_y:len_y, med_x:len_x]<- diamond_step(mat[med_y:len_y, med_x:len_x])
mat[med_y:len_y, med_x:len_x]<- square_step(mat[med_y:len_y, med_x:len_x])

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
