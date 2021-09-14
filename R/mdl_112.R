#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem H
mdl_G_112 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 1.67
		c03 = -0.0289
		c04 = 0.506
		c01 = -5.18
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}
	xmdl.2<-function(p02, p03){
		c02 = 2.48
		c03 = -0.0446
		c04 = 0.685
		c01 = -18.5
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}
	xmdl.3<-function(p02, p03){
		c02 = 1.25
		c03 = -0.0156
		c04 = 0.0
		c01 = 1.58
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
