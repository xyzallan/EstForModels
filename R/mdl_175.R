#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_V_175 <- function(p01, p02, p03){
	xmdl.5<-function(p02, p03){
		c02 = 1.9291
		c03 = 1.004
		c01 = 0.000049
		c01 * p02^c02 * p03^c03
	}
	xmdl.6<-function(p02, p03){
		c02 = 1.5771
		c03 = 1
		c01 = 0.000128
		c01 * p02^c02 * p03^c03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		NA
		)))
}
