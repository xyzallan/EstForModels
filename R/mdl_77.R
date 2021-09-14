#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_V_77 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 2.01395
		c03 = 0.99676
		c04 = 2.07025
		c05 = -1.07209
		c01 = 0.036089
		(c01 * p02^c02 * (c03)^p02 * p03^c04 * (p03-1.3)^(c05)) / 1000
	}
	xmdl.2<-function(p02, p03){
		c02 = 1.91505
		c03 = 0.99146
		c04 = 2.82541
		c05 = -1.53547
		c01 = 0.022927
		(c01 * p02^c02 * (c03)^p02 * p03^c04 * (p03-1.3)^(c05)) / 1000
	}
	xmdl.3<-function(p02, p03){
		c02 = 2.10253
		c03 = 0.986
		c04 = 3.98519
		c05 = -2.659
		c01 = 0.011197
		(c01 * p02^c02 * (c03)^p02 * p03^c04 * (p03-1.3)^(c05)) / 1000
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
