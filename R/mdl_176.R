#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
#' @param p04 Crwn H
mdl_V_176 <- function(p01, p02, p03, p04){
	xmdl.5<-function(p02, p03, p04){
		c02 = 1.9669
		c03 = 0.8292
		c04 = 0.1013
		c01 = 0.000057
		c01 * p02^c02 * p03^c03 * p04^c04
	}
	xmdl.6<-function(p02, p03, p04){
		c02 = 3.80770
		c03 = 0.87541
		c04 = 1
		c01 = 1.699864e-07
		c01 * p02^c02 * p03^c03 * p04^c04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04), 
		NA
		)))
}
