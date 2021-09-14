#' @title Stem BM
#' @return Stem BM
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_BM_213 <- function(p01, p02){
	xmdl.3<-function(p02){
		c01 = 0.0
		c02 = 0.0
		(c02 * 0.00001)*p02^c03
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('NA'), xmdl.3(p02), 
		NA
		))
}
