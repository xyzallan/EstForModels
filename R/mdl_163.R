#' @title LigSt F
#' @return LigSt F
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_F_163 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = -318.3342
		c03 = 36.90522
		c04 = 4.05292
		c01 = 0.40804
		c01+c02 / (p03 * p02 * p02)+c03 / (p03 * p02)+c04 / (p02 * p02)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
		))
}
