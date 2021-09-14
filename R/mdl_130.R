#' @title Stem N
#' @return Stem N
#' @param p01 Stem Spec
#' @param p02 Stem H
mdl_N_130 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = -340
		c03 = 3.7
		c01 = 4950
		c01+c02 * p02+c03 * p02^2
	}
	xmdl.2<-function(p02){
		c02 = -200
		c03 = 8.1
		c01 = 3300
		c01+c02 * p02+c03 * p02^2
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
		)))
}
