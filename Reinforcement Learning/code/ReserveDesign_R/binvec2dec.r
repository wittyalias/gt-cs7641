binvec2dec <- function(vec) {

# BINVEC2DEC Convert binary vector to decimal number.
#
#    BINVEC2DEC(B) interprets the binary vector B and returns the 
#    equivalent decimal number.  The least significant bit is 
#    represented by the first column.
#
#    Non-zero values will be mapped to 1, e.g. [1 2 3 0] maps
#    to [1 1 1 0].
# 
#    Note: The binary vector cannot exceed 52 values.
#
#    Example: binvec2dec([1 1 1 0 1]) returns 23
#

# Error if B is not defined
if (missing(vec)) {
	print('B must be defined')
}

# Error if B is not a double.
if ( class(vec) != "numeric" ) {
	print('B must be a binvec')
}

# Non-zero values map to 1.
vec[vec!=0] <- 1

# Binary conversion
h <- rev(as.logical(vec))
out <- sum(2^(which(rev(h))-1))

return(out)

}

