#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem H
mdl_G_116 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 1.29
		c03 = -0.0237
		c04 = 0.445
		c01 = -7.01
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}
	xmdl.2<-function(p02, p03){
		c02 = 1.83
		c03 = -0.0336
		c04 = 0.494
		c01 = -14.31
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}
	xmdl.3<-function(p02, p03){
		c02 = 0.708
		c03 = -0.0074
		c04 = 0.0
		c01 = 2.99
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
