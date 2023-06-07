#' @title single tree (stem), probability 
#' @return single tree (stem), probability 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 stand (stem), height (100 years old)
#' @param p04 stand (larger trees), basal area (breast height)
#' @param p05 stand (stem), basal area (breast height)
#' @param p06 species level (stem), basal area (breast height)
#' @param p07 single tree (stem), age 
#' @param p08 single tree (stem), age (random point)
#' @export
#' @rdname mdl_Prob_81
mdl_Prob_81 <- function(p01, p02, p03, p04, p05, p06, p07, p08){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08){
		c02 = -14.2660
		c03 = -0.0462
		c04 = -0.0761
		c05 = 0.0
		c01 = 8.4904
		ipf1 = p06/p05*100
		(1-(1+exp(-(c01+c02 / p02+c03 * p04+c04 * p03+c05 * ipf1)))^-(p08-p07)) * 100
	}

	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08){
		c02 = -6.7020
		c03 = -0.0281
		c04 = -0.0264
		c05 = -0.0132
		c01 = 8.0599
		ipf1 = p06/p05*100
		(1-(1+exp(-(c01+c02 / p02+c03 * p04+c04 * p03+c05 * ipf1)))^-(p08-p07)) * 100
	}

	xmdl.3<-function(p02, p03, p04, p05, p06, p07, p08){
		c02 = -2.5280
		c03 = 0.0
		c04 = 0.0
		c05 = 0.0
		c01 = 4.8923
		ipf1 = p06/p05*100
		(1-(1+exp(-(c01+c02 / p02+c03 * p04+c04 * p03+c05 * ipf1)))^-(p08-p07)) * 100
	}

	xmdl.19<-function(p02, p03, p04, p05, p06, p07, p08){
		c02 = -7.3544
		c03 = -0.0199
		c04 = 0.0
		c05 = 0.0
		c01 = 5.1575
		ipf1 = p06/p05*100
		(1-(1+exp(-(c01+c02 / p02+c03 * p04+c04 * p03+c05 * ipf1)))^-(p08-p07)) * 100
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07, p08 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06, p07, p08), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07, p08), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05, p06, p07, p08), 
		ifelse(p01 %in% c('TL','LV','LM','HB'), xmdl.19(p02, p03, p04, p05, p06, p07, p08), 
		NA
	)))))
}
