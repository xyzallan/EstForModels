#' @title Crwn D
#' @return Crwn D
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_D_152 <- function(p01, p02){
	xmdl.2<-function(p02){
		c02 = 0.1104
		c01 = 0.8425
		c01+c02 * p02
	}
	xmdl.21<-function(p02){
		c02 = 0.1810
		c01 = 1.3887
		c01+c02 * p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02), 
		NA
		)))
}
