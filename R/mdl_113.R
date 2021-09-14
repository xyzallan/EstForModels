#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem H
mdl_G_113 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 1.47
		c03 = -0.0297
		c04 = 0.55
		c01 = -4.00
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}
	xmdl.2<-function(p02, p03){
		c02 = 2.07
		c03 = -0.0397
		c04 = 0.327
		c01 = -5.08
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}
	xmdl.3<-function(p02, p03){
		c02 = 1.2
		c03 = -0.0211
		c04 = 0.0
		c01 = 4.87
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
