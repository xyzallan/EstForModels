#' @title Stem N
#' @return Stem N
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_N_140 <- function(p01, p02){
	xmdl.2<-function(p02){
		c02 = -0.0341
		c01 = 6190
		c01 * 10^(c02 * p02)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
		))
}
