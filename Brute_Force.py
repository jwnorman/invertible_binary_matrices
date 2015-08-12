# Brute force
import itertools
import numpy as np
import time

def percent_invertible_bf(n):
    sum = 0.
    for i in itertools.product(range(2), repeat = n**2):
	   mat = np.matrix(i).reshape(n, n)
	   det = np.linalg.det(mat)
	   sum += int(det != 0)
    return sum / 2**n**2

before2 = time.time()
res2 = percent_invertible_bf(2)
after2 = time.time()
tottime2 = after2 - before2

before3 = time.time()
res3 = percent_invertible_bf(3)
after3 = time.time()
tottime3 = after3 - before3

before4 = time.time()
res4 = percent_invertible_bf(4)
after4 = time.time()
tottime4 = after4 - before4

before5 = time.time()
res5 = percent_invertible_bf(5)
after5 = time.time()
tottime5 = after5 - before5

print "2x2 Matrix"
print "----------"
print res2
print tottime2
print ""

print "3x3 Matrix"
print "----------"
print res3
print tottime3
print ""

print "4x4 Matrix"
print "----------"
print res4
print tottime4
print ""

print "5x5 Matrix"
print "----------"
print res5
print tottime5
print ""

# 2x2 Matrix
# ----------
# 0.375
# 0.000393867492676

# 3x3 Matrix
# ----------
# 0.33984375
# 0.00915694236755

# 4x4 Matrix
# ----------
# 0.34423828125
# 1.29621386528

# 5x5 Matrix
# ----------
# 0.372955799103
# 649.569082975