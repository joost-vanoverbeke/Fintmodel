

#### functies fuzzy logic ####

membership <- function(values, tolerantie, print.M=FALSE) {

	memb.item <- function(x, Tol) {
		Memb <- switch(as.character(sum(sapply(Tol, function(y) x<y))),
									 "0" = c(L=0, M=ifelse(Tol[3]==Tol[4],1,0), H=ifelse(Tol[3]==Tol[4],0,1)),
									 "1" = c(L=0, M=1-(x-Tol[3])/(Tol[4]-Tol[3]), H=(x-Tol[3])/(Tol[4]-Tol[3])),
									 "2" = c(L=0, M=1, H=0),
									 "3" = c(L=1-(x-Tol[1])/(Tol[2]-Tol[1]), M=(x-Tol[1])/(Tol[2]-Tol[1]), L=0),
									 "4" = c(L=1, M=0, L=0))
		return(Memb)
	}

	S <- sapply(1:ncol(tolerantie), function(i) memb.item(values[i],tolerantie[,i]))
	row.names(S) <- c("L","M","H")
	S <- data.frame(apply(S, 2, unlist))
	names(S) <- names(tolerantie)
	if(print.M) {
		cat("membership: \n")
		print(values)
		cat("\n")
		print(S)
		cat("\n\n")
	}
	return(S)
}


###vertakking membership
expand.membership <- function(S) {
	R <- data.frame(replicate(ncol(S), row.names(S)))
	Ex.P <- expand.grid(S)
	Ex.M <- expand.grid(R)
	Ex.M <- data.frame(Ex.M[apply(Ex.P, 1, function(x) !any(x == 0)),])
	Ex.P <- data.frame(Ex.P[apply(Ex.P, 1, function(x) !any(x == 0)),])
	#   names(Ex.M) <- names(Ex.P)
	names(Ex.M) <- names(S)
	Ex.M$p <- apply(Ex.P, 1, min)
	return(Ex.M)
}


###matchen met regelbank
match.rules <- function(cases, rules) {
	#   return(rules$SI[apply(cases, 1, function(c) which(apply(sapply(names(cases), function(v) c[v]==rules[,v]), 1, all)))])
	ps <-
		apply(cases, 1, function(c) {
			pos <- which(apply(sapply(names(cases), function(v) c[v]==rules[,v]), 1, all))
			if(length(pos)>0)
				pos
			else
				-1
		})
	# lines that do not correspond to any rule in the rulebank correspond to SI = 'L'
	SI <- rep("L", length(ps))
	# lines that do not correspond to any rule in the rulebank receive label 'A'
	#   SI <- rep("A", length(ps))
	SI[ps>0] <- rules$SI[ps[ps>0]]
	return(SI)
}


###bereken gemiddelde score
calc.score <- function(x) {
	if(any(x$SI=='A')) print("bingo!")
	x <- x[x$SI!='A',]
	score <- with(x, tapply(p, SI, max))
	mean.score <- sum(0.5*score["M"], score["H"], na.rm=TRUE)/sum(score)
	return(mean.score)
}



#### tolerantiegrenzen ####

Zoopl.10 <- exp(-6.251 + 1.023*log(10))
Zoopl.50 <- exp(-6.251 + 1.023*log(50))


Tolerantie.larvaal <- data.frame(row.names=c("C1","C2","C3","C4"),
																 Temp=c(11,17,25,30),
																 Oxy=c(4,5,100,100),
																 Sal=c(5,10,15,30),
																 SPM=c(0,0,50,500),
																 #                          Zoopl=c(0,50,100,100),
																 Zoopl=c(0,Zoopl.10,Zoopl.10,Zoopl.50),
																 Diepte=c(0.15,1.5,6,15),
																 Stroom=c(0,0.3,0.7,1.5))

Tolerantie.adult <- data.frame(row.names=c("C1","C2","C3","C4"),
															 Temp.A=c(14,15,25,30),
															 Oxy=c(4,5,100,100),
															 Sal=c(0,0,5,10),
															 Diepte=c(0.3,1.5,6,15),
															 Stroom=c(0,0.3,1.5,2),
															 SIL=c(0.25,0.5,0.5,0.75))



#### parallel berekenen ####

parFun <- function(x, memb, exp.m, match.r, calc.sc, Tol, Rules, vars) {
  if(!any(is.na(x)))
  {
    S <- memb(x,Tol)
    M <- exp.m(S)
    M$SI <- match.r(M[vars], Rules)
    calc.sc(M[c("p","SI")])
  } else
    NA
}


