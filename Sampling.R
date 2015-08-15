# Sampling.R

setwd("/Users/jacknorman1/Documents/USF/Bootcamp/Linear Algebra/Project/invertible_binary_matrices")
library(MASS)
library(ggplot2)
library(grid)
library(rmarkdown)
source("multiplot.R")

generateBinaryMatrix <- function(n, p = .5) {
	# Generate one random binary matrix
	#
	# Args:
	#	n: the size of the matrix (nxn)
	#	p: the probability at which to sample zeros (i.e., 1 - p will be
	#		the rate at which ones are sampled)
	# Returns:
	#	An nxn matrix
	#
	data = sample(c(0, 1), n^2, replace = TRUE, prob = c(p, 1 - p))
	A = matrix(data, n)
	return(A)
}

inverseIteration <- function(n, p) {
	# Determines the invertibility of one matrix
	#
	# Args:
	#	n: the size of the matrix (nxn)
	#	p: the probability at which to sample zeros (i.e., 1 - p will be
	#		the rate at which ones are sampled)
	# Returns:
	#	A logical value indicating the invertibility of the matrix
	#
	A = generateBinaryMatrix(n, p)
	A_det <- det(A)
	return(A_det != 0)
}

runner <- function(B = 100000, n = 5, p = .5) {
	# Iterates over inverseIteration many times.
	#
	# Args:
	#	B: the number of random binary matrices to create
	#	n: the size of the matrix (nxn)
	#	p: the probability at which to sample zeros (i.e., 1 - p will be
	#		the rate at which ones are sampled)
	# Returns:
	#	The mean proportion of invertible matrices
	#
	mean(vapply(1:B, function(x) {
		inverseIteration(n, p)
	}, logical(1L)))
}


grid_runner <- function(grid, B) {
	# Runs many sample for a grid of values for n and p
	#
	# Args:
	#	grid: a data.frame with combinatorial values for n and p
	#	B: the number of random binary matrices to create
	#
	# Returns:
	#	A data.frame with variables n, p, and isInverse which represent
	#		the size of the matrices, the proportion bias towards zeros,
	#		and the proprtion of the invertible matrices from the smaple.
	#
	df <- as.data.frame(t(sapply(1:nrow(grid), function(x) {
		n <- grid[x, 1]
		p <- grid[x, 2]
		res <- runner(B, n, p)
		c(n, p, res)
	})))
	names(df) <- c("n", "p", "isInverse")
	df
}

#-------------------------------------------------------------------------------

# Create grid
nrange <- seq(2, 30, 1)
prange <- seq(.05, .95, .05)
allgrid <- expand.grid(nrange, prange)
B <- 10 # .320138
B <- 100 # 2.429802
B <- 1000 # 22.50329
B <- 10000 # 693.5904

# Simulate
beg <- Sys.time()
grid_sol2 <- grid_runner(allgrid, B)
save(grid_sol2, file = "grid_sol_2_30_1_05_95_05_10000.Rda")
end <- Sys.time()
end - beg


# Plot
p <- ggplot() + 
	stat_smooth(data = grid_sol2[grid_sol2$n <= 20, ], 
	 		    aes(x = p, y = isInverse, color = factor(n)),
	 		    se = FALSE) +
	ggtitle(paste("Invertibility by Size of Matrix and Bias\nB = ", B)) +
	xlab("Bias") +
	ylab("Proportion Invertible") +
	scale_x_continuous(breaks = seq(0, 1, .1)) + 
	scale_colour_discrete(name = "n")

q <- ggplot() + 
	geom_smooth(data = grid_sol2, 
	 		   aes(x = n, y = isInverse, colour = factor(p)),
	 		   se = FALSE) +
	ggtitle(paste("Invertibility by Size of Matrix and Bias\nB = ", B)) +
	xlab("Size of Matrix") +
	ylab("Proportion Invertible") +
	coord_cartesian(xlim = c(2, 25)) +
	scale_x_continuous(breaks = seq(2, 24, 2)) + 
	scale_colour_discrete(name = "Bias")

# Data frame with brute force info (hard coded from Python script results)
invertible_info <- data.frame(n = c(2, 3, 4, 5), 
	Percent_Invertible = c(.375, .33984375, .34423828125, .372955799103),
	Seconds_to_Complete = c(.00039, .00916, 1.2962, 649.57))
		   
# 
s <- ggplot() + 
	geom_point(data = grid_sol2[grid_sol2$n <= 30, ], 
	 		    aes(x = n, y = isInverse, color = factor(n))) +
	 		    facet_wrap( ~ p) +
	ggtitle(paste("Invertibility by Size of Matrix and Bias\nB = ", B)) +
	xlab("n") +
	ylab("Proportion Invertible") +
	guides(col = guide_legend(ncol = 2)) +
	scale_x_discrete(breaks = seq(2, 30, 4)) + 
	scale_colour_discrete(name = "n")
