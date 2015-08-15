# Brute force
import itertools
import numpy as np
import time

def percent_invertible_bf(n):
    """Calculates the exact proportion of invertible matrices

    Args:
        n: the size of the matrix (nxn)

    Returns:
        The proportion of invertible matrices
    """
    
    sum = 0.
    for i in itertools.product(range(2), repeat = n**2):
       mat = np.matrix(i).reshape(n, n)
       det = np.linalg.det(mat)
       sum += int(det != 0)
    return sum / 2**n**2