#' @title stand (stem), diameter (mature)
#' @return stand (stem), diameter (mature)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
mdl_D_110 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 0.5
		c03 = 0.0
		c01 = 15.25
		c01+c02 * p02+c03 * sqrt(p02)
	}

	xmdl.2<-function(p02){
		c02 = -1.568
		c03 = 18.84
		c01 = -29.82
		c01+c02 * p02+c03 * sqrt(p02)
	}

	xmdl.3<-function(p02){
		c02 = -0.306
		c03 = 6.594
		c01 = -3.86
		c01+c02 * p02+c03 * sqrt(p02)
	}

	xmdl.6<-function(p02){
		c03 = 0.0
		c02 = 0.25
		c01 = 7.125
		c01+c02 * p02+c03 * sqrt(p02)
	}

	xmdl.8<-function(p02){
		c03 = 0.0
		c01 = 23.625
		c02 = 0.25
		c01+c02 * p02+c03 * sqrt(p02)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('LM','HB','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('SA','TA'), xmdl.8(p02), 
		NA
	))))))
}
