#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species ()
#' @param p02 stand (stem), age ()
#' @param p03 stand (stem), basal area (breast height)
#' @param p04 stand (stem), height (dominant)
mdl_V_65 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 1.91365
		c03 = 0.0
		c04 = 0.6
		c05 = 0.33499
		c06 = 1.3
		c01 = 2.43732
		c07 = 1
		ipf1 = c03+c07*log(p02)
		ipf2 = log(p03*p04)
		exp(c01+c02 * ipf1^c04-c05 * ipf2^c06)
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.71152
		c03 = 5.5
		c04 = 1.1
		c05 = 0.00499
		c06 = 3.0
		c01 = 3.16628
		c07 = -1
		ipf1 = c03+c07*log(p02)
		ipf2 = log(p03*p04)
		exp(c01+c02 * ipf1^c04-c05 * ipf2^c06)
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = -0.60329
		c03 = 0.0
		c04 = 1.2
		c05 = 0.09984
		c06 = 1.5
		c07 = 1
		c01 = 7.8397
		ipf1 = c03+c07*log(p02)
		ipf2 = log(p03*p04)
		exp(c01+c02 * ipf1^c04-c05 * ipf2^c06)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
