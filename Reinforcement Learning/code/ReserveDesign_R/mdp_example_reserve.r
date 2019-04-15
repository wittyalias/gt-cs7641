# function [P, R] = mdp_example_reserve (M, pj)

mdp_example_reserve <- function(M, pj) {

# mdp_example_reserve   Generate a Markov Decision Process example based on
#                       a simple reserve design problem
#                       (see the related documentation for more detail)
# Arguments -------------------------------------------------------------
#   M(JxI) = species distribution across sites
#        J = number of sites (> 0), optional (default 5)
#        I = number of species (>0), optional (default 7)
#   pj = probability of development occurence, in ]0, 1[, optional (default 0.1)
# Evaluation -------------------------------------------------------------
#   P(SxSxA) = transition probability matrix
#   R(SxA) = reward matrix
#   M(JxI) = random species distribution across sites

if ( nargs() >= 1 & dim(M)[1] <= 1 & dim(M)[2] <= 1 ) {
	print('----------------------------------------------------------')
	print('MDP Toolbox ERROR: M is a JxI matrix with Number of sites J and species I must be greater than 0')
	print('----------------------------------------------------------')
} else if ( nargs() >= 2 & ( pj < 0 | pj > 1 ) ) {
	print('-----------------------------------------------------------')
	print('MDP Toolbox ERROR: Probability pj must be in [0; 1]')
	print('-----------------------------------------------------------')
} else {
	# initialization of optional arguments
	if (nargs() < 2) pj <- 0.1
	if (nargs() < 1) {
		J <- 3
		I <- 3
		M <- round(matrix(nrow=J, ncol=I, data=runif(I*J,0,1)))
	}
	
	J <- nrow(M)
	I <- ncol(M)
	
	# Definition of states
	# each site, 0 is available; 1 is reserved; 2 is developed
	S <- 3^J
	# action space
	A <- J
	
	# There are J actions corresponding to the selection of a site for
	# reservation. A site can only be reserved if it is available.
	# By convention we will use a ternary base where state #0 is the state
	# that corresponds to [0,0, ..,0] all sites are available. State #1 is
	# [1,0,0,...,0]; state 2 is [2,0,0 ..,0] and state 3 is [0,1,0, .. 0] and so forth.
	# for example 
	# site = [0,0,1,2] means the first 2 sites are available (site[1:2]=0), site 3 is
	# reserved (site[3]=1) and site 4 is developped (site[4]=2).

	# Build P(AxSxS)
	# complexity is in SxAx2^navail; with 2^navail<=S
	
	P <- array(0, c(S,S,A))
	
	for (s1 in 1:S) {
		site1 <- getSite(s1-1, J)
		for (a in 1:A) {
			site2 <- site1
			if (site1[a] == 0) {
				site2[a] <- 1
			}
			availSite <- which(site2 == 0)
			if (length(availSite) > 0) {
				navail <- length(availSite)
				siten <- rep(1,2^navail) %*% t(site2)
				aux <- numeric(navail)
				for (k in 1:2^navail) {
					siten[k,availSite] <- aux*2
					ndev <- sum(abs(site2[availSite]-aux))
					s2 <- getState(siten[k,])+1
					P[s1,s2,a] <- pj^ndev*(1-pj)^(navail-ndev)
					aux <- dec2binvec(binvec2dec(aux)+1, navail)
				}
			} else {
				s2 <- getState(site2)+1
				P[s1,s2,a] <- 1
			}
		}
	}
	
	# Build R
	
	R <- array(0, c(S,A))
	for (s1 in 1:S) {
		site1 <- getSite(s1-1,J)
		for (a in 1:J) {
			if (site1[a] != 2) {
				targetSp <- M[a,]
				reservedSites <- which(site1==1)
				if (length(reservedSites) > 0) {
					X <- M[reservedSites,]
					if (is.vector(X)) {
						reservedSp <- pmax(M[reservedSites,])
					}
					if (is.array(X)) {
						reservedSp <- apply((M[reservedSites,]),2,max)
					}
					R[s1,a] <- sum(pmax(0,targetSp-reservedSp))
				} else {
					R[s1,a] <- sum(pmax(0,targetSp))
				}
			}
		}
	}
	
	return(list("P"=P, "R"=R))
}

}

