# Author: iadine.Chades@csiro.au
# input: stateid number
# output: site variable corresponding to the configuration of the J sites
# note: we use a ternary base.

getSite <- function(stateid,J) {
	baseTern <- (3*rep(1,J))^(0:(J-1))
	site <- numeric(J)
	for (i in J:1) {
		if (stateid - 2*baseTern[i] >= 0) {
			site[i] <- 2
			stateid <- stateid - 2*baseTern[i]
		} else if (stateid - baseTern[i] >= 0) {
			site[i] <- 1
			stateid <- stateid - baseTern[i]
		}
	}
	
	return(site)
}
