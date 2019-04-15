# Author: iadine.Chades@csiro.au
# input: site variable representing the current configuration of the J
# sites
# output: corresponding state index for MDP Toolbox
# note: we use a ternary base.

getState <- function(site) {
	J <- length(site)
	baseTern <- (3*rep(1,J))^(0:(J-1))
	state <- sum(site*baseTern)
	return(state)
}
