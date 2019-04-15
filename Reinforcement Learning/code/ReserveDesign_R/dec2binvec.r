dec2binvec <- function(dec,n) {

# DEC2BINVEC Convert decimal number to a binary vector.
#
#    DEC2BINVEC(D) returns the binary representation of D as a binary
#    vector.  The least significant bit is represented by the first 
#    column.  D must be a non-negative integer. 
# 
#    DEC2BINVEC(D,N) produces a binary representation with at least
#    N bits.
# 
#    Example: dec2binvec(23) returns [1 1 1 0 1]
#

# Error if dec is not defined
if ( nargs() < 1 ) {
	print('dec must be defined')
}

if ( class(dec) != "numeric") {
	print('dec must be a double')
}

# Error if a negative number is passed in.
if (dec < 0) {
	print('D must be a positive integer')
}

x <- dec

exponent <- floor(log(x, 2)) + 1
mantissa <- x * 2 ^(-exponent)

rem <- function(X, Y) return(X - trunc(X/Y)*Y)

out <- as.numeric( rem( floor( x*2^( (1-max(n, exponent)):0 )), 2) )

return(rev(out))

}
