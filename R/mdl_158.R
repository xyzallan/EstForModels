#' @title LigSt F
#' @return LigSt F
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_F_158 <- function(p01, p02, p03){
	xmdl.7<-function(p02, p03){
		c02 = 1.011176
		c03 = 2.10428
		c04 = 203.1997
		c01 = 0.4786
		c01-(c02 / p02)+(c03 / p03)-(c04 / (p02 * p03 * p03))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		NA
		))
}
