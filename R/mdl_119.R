#' @title Stem L
#' @return Stem L
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem D
mdl_L_119 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 16.0
		c03 = -4.6
		c01 = 176.8
		c01+c02 * p03+c03 * p02
	}
	xmdl.2<-function(p02, p03){
		c02 = 15.5
		c03 = -2.5
		c01 = 124.2
		c01+c02 * p03+c03 * p02
	}
	xmdl.3<-function(p02, p03){
		c02 = 14.5
		c03 = 0.0
		c01 = 134.3
		c01+c02 * p03+c03 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
