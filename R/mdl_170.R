#' @title Stem H
#' @return Stem H
#' @param p01 Stem Spec
#' @param p02 Stem H
mdl_H_170 <- function(p01, p02){
	xmdl.2<-function(p02){
		c02 = 1.1206
		c01 = 0.5981
		c01 * p02^c02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
		))
}
