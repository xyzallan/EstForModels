#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @export
#' @rdname mdl_V_32
mdl_V_32 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 2.483
		c03 = 1.203
		c04 = 0.085
		c05 = -0.011
		c01 = -3.357
		c01 * 0.001 * p02^3+c02 * 0.1 * p02^2+c03 * 10 * p02+c04 * 10+c05 * 100 / sqrt(p02)
	}

	xmdl.2<-function(p02){
		c02 = 4.172
		c03 = 0.797
		c04 = -0.062
		c05 = 0.0
		c01 = -1.135
		c01 * 0.001 * p02^3+c02 * 0.1 * p02^2+c03 * 10 * p02+c04 * 10+c05 * 100 / sqrt(p02)
	}

	xmdl.3<-function(p02){
		c02 = 7.285
		c03 = 0.0
		c04 = 2.987
		c05 = -0.038
		c01 = -8.737
		c01 * 0.001 * p02^3+c02 * 0.1 * p02^2+c03 * 10 * p02+c04 * 10+c05 * 100 / sqrt(p02)
	}

	xmdl.4<-function(p02){
		c02 = 9.054
		c03 = -0.398
		c04 = 9.408
		c05 = -0.123
		c01 = -8.898
		c01 * 0.001 * p02^3+c02 * 0.1 * p02^2+c03 * 10 * p02+c04 * 10+c05 * 100 / sqrt(p02)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('TA','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('LV','LM','HB'), xmdl.4(p02), 
		NA
	)))))
}
