% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rworld_terrain_package.R
\name{make.terrain}
\alias{make.terrain}
\title{wrapper function: make.terrain is a wrapper for diamond_square_step(mat, x, y). Change negative numbers to NA to represent water. The output is an image.}
\usage{
make.terrain(x, y)
}
\arguments{
\item{x}{is the number of columns in the matrix}

\item{y}{is the number of rows in the matrix}
}
\value{
image of the terrain
}
\description{
wrapper function: make.terrain is a wrapper for diamond_square_step(mat, x, y). Change negative numbers to NA to represent water. The output is an image.
}
\examples{
my_terrain<- make.terrain(5, 5)

}

