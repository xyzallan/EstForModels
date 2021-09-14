#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
#' @param p04 Stem H
mdl_D_157 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c02 = 1.3311
		c03 = -0.7016
		c04 = 0.0
		c05 = -0.2142
		c06 = 0.1306
		c01 = -1.7258
		ipf1 = c01+c02*(1/log(p03^(1/p02)))+c03*(1/(p03/p02)^2)
		ipf2 = c04+c05*(1/log(p03^(1/p02)))+c06*(1/(p03/p02)^2)
		(ipf1 * (1-(p04 / p03)^3)+ipf2 * (log(p04 / p03))) * 2
	}
	xmdl.2<-function(p02, p03, p04){
		c02 = 1.595
		c03 = -3.155
		c04 = 0.512
		c05 = -0.158
		c06 = -0.502
		c01 = -0.223
		ipf1 = c01+c02*(1/log(p03^(1/p02)))+c03*(1/(p03/p02)^2)
		ipf2 = c04+c05*(1/log(p03^(1/p02)))+c06*(1/(p03/p02)^2)
		(ipf1 * (1-(p04 / p03)^3)+ipf2 * (log(p04 / p03))) * 2
	}
	xmdl.16<-function(p02, p03, p04){
		c02 = 1.4423
		c03 = -2.1807
		c04 = 0.4369
		c05 = -0.2008
		c06 = -0.2836
		c01 = -0.5828
		ipf1 = c01+c02*(1/log(p03^(1/p02)))+c03*(1/(p03/p02)^2)
		ipf2 = c04+c05*(1/log(p03^(1/p02)))+c06*(1/(p03/p02)^2)
		(ipf1 * (1-(p04 / p03)^3)+ipf2 * (log(p04 / p03))) * 2
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('TS'), xmdl.16(p02, p03, p04), 
		NA
		))))
}
