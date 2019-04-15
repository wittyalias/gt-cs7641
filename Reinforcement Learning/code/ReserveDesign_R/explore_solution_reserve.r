explore_solution_reserve <- function(init_site, policy, M, P, R) {
	
	J <- nrow(M)
	I <- ncol(M)
	
	S <- 3^J
	
	T <- J+1
	action <- numeric(T)
	Tsites <- matrix(0, nrow=J, ncol=T)
	Treward <- numeric(T)
	
	current_site <- init_site
	
	for (i in 1:T) {
		state <- getState(current_site)+1
		# Tstates[i,] <- state
		Tsites[,i] <- current_site
		action[i] <- policy[state]
		
		if (i != 1) {
			Treward[i] <- R[state, action[i]] + Treward[i-1]
		} else {
			Treward[i] <- R[state, action[i]]
		}
		
		p_state_new <- runif(1)
		p <- 0
		state_new <- 0
		while(p < p_state_new & state_new < S) {
			state_new <- state_new + 1
			p <- p + P[state, state_new, action[i]]
		}
		
		current_site <- getSite(state_new-1, J)
	}
	
	pardef <- par()
	par(mfrow=c(2,1),
	    mar = c(2,5,4,1))
	# par(mfrow = c(2,1))
	image(t(Tsites)+1, xlab="Time horizon", ylab="Sites", col=c("white", "grey", "black"))
	title(main="white = Unreserved, grey=Reserved, black=Developed", font.main=1)
	plot(1:T, Treward, xlab="Time horizon", ylab="Number of species protected", type="l")
	par(pardef)

}