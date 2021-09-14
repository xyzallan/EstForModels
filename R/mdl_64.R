#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem A
#' @param p03 Stem V
mdl_V_64 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 0.68384
		c03 = 5.69
		c04 = 1.25
		c05 = -0.155543
		c06 = 1.55
		c07 = -1
		c08 = 5.97
		c01 = 1.63805
		ipf1 = c03+c07*log(p02)
		ipf2 = (c08+c07*log(p03))
		exp(c01+c02 * ipf1^c04-c05 * ipf2^c06)
	}
	xmdl.2<-function(p02, p03){
		c02 = -1.31769
		c03 = 0.0
		c04 = 0.8
		c05 = 0.52557
		c06 = 0.9
		c07 = 1
		c08 = 0.0
		c01 = 9.5015
		ipf1 = c03+c07*log(p02)
		ipf2 = (c08+c07*log(p03))
		exp(c01+c02 * ipf1^c04-c05 * ipf2^c06)
	}
	xmdl.3<-function(p02, p03){
		c02 = -2.66489
		c03 = 0.0
		c04 = 0.6
		c05 = 0.36693
		c06 = 1
		c07 = 1
		c08 = 0.0
		c01 = 11.16634
		ipf1 = c03+c07*log(p02)
		ipf2 = (c08+c07*log(p03))
		exp(c01+c02 * ipf1^c04-c05 * ipf2^c06)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
