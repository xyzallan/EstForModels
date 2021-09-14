#' @title stand (stem), height ()
#' @return stand (stem), height ()
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), height 
#' @param p04 stand (stem), age (random point)
mdl_H_180 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = -0.0109
		c03 = 1.3925
		c01 = 0.7283
		ipf1 = p03*(1+c01*((50/p02)^c03-1))/(1-c02*p03*((50/p02)^c03-1))
		ipf1 / (1+(c01+c02 * ipf1) * ((50 / p04)^c03-1))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = -0.0137
		c03 = 1.6116
		c01 = 0.7977
		ipf1 = p03*(1+c01*((50/p02)^c03-1))/(1-c02*p03*((50/p02)^c03-1))
		ipf1 / (1+(c01+c02 * ipf1) * ((50 / p04)^c03-1))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = -0.0161
		c03 = 1.3460
		c01 = 0.7298
		ipf1 = p03*(1+c01*((50/p02)^c03-1))/(1-c02*p03*((50/p02)^c03-1))
		ipf1 / (1+(c01+c02 * ipf1) * ((50 / p04)^c03-1))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA','TA','SA','VA','JA','KP'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU','TS','NU','TO','LH'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS','HB','LM','LV','RE','PP','PN','TL'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
