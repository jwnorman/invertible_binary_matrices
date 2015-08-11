# Sampling.R

library(MASS)
library(parallel)
library(ggplot2)
library(grid)
source("multiplot.R")

generateBinaryMatrix <- function(n, p = .5) {
	data = sample(c(0, 1), n^2, replace = TRUE, prob = c(p, 1 - p))
	A = matrix(data, n)
	return(A)
}

inverseIteration <- function(n, p) {
	A = generateBinaryMatrix(n, p)
	A_det <- det(A)
	return(A_det != 0)
}

runner <- function(B = 100000, n = 5, p = .5) {
	mean(vapply(1:B, function(x) {
		inverseIteration(n, p)
	}, logical(1L)))
}

grid_runner <- function(grid, B) {
	df <- as.data.frame(t(sapply(1:nrow(grid), function(x) {
		n <- grid[x, 1]
		p <- grid[x, 2]
		res <- runner(B, n, p)
		c(n, p, res)
	})))
	names(df) <- c("n", "p", "isInverse")
	df
}

nrange <- seq(2, 15, 1)
prange <- c(seq(.4, .6, .05))
allgrid <- expand.grid(nrange, prange)
B <- 10000 # 46 seconds

beg <- Sys.time()
grid_sol2 <- grid_runner(allgrid, B)
end <- Sys.time()
end - beg

p <- ggplot() + 
	geom_point(data = grid_sol2, 
	 		   aes(x = p, y = isInverse, colour = n, size = 2)) +
	ggtitle(paste("Invertibility by Size of Matrix and Bias\nB = ", B)) +
	xlab("Bias") +
	ylab("Proportion Invertible") +
	coord_cartesian(xlim = c(0, 1)) +
	scale_x_continuous(breaks = prange[seq(1, length(prange), 2)])


	 			
q <- ggplot() + 
	geom_point(data = grid_sol2, 
	 		   aes(x = n, y = isInverse, colour = p, size = 2)) +
	ggtitle(paste("Invertibility by Size of Matrix and Bias\nB = ", B)) +
	xlab("Size of Matrix") +
	ylab("Proportion Invertible") +
	coord_cartesian(xlim = c(4, 22)) +
	scale_x_continuous(breaks = nrange)
	 			 			
multiplot(p, q, cols = 2)
	 