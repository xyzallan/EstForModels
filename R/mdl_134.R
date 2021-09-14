#' @title Stem HF
#' @return Stem HF
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem D
mdl_HF_134 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = -0.0263
		c03 = 3.0028
		c04 = 3.7094
		c05 = -1.2138
		c01 = 0.6390
		p02 * (1-exp(-(c01+c02 * (p03 / p02)^c03+c04 * p02^c05)))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
		))
}
