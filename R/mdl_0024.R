#' @title single tree (stem), age (breast height)
#' @return single tree (stem), age (breast height)
#' @param p01 single tree (stem), species 
#' @param p02 stand (area), bonitet 
#' @export
#' @rdname mdl_A_24
mdl_A_24 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 0.123
		c03 = 0.012
		c04 = 5.0
		c01 = 0.072
		c04+p02 * (c03+p02 * (c02+p02 * c01))
	}

	xmdl.2<-function(p02){
		c02 = -0.394
		c03 = 0.595
		c04 = 8.1
		c01 = 0.146
		c04+p02 * (c03+p02 * (c02+p02 * c01))
	}

	xmdl.3<-function(p02){
		c02 = -0.154
		c03 = 0.473
		c04 = 1.7
		c01 = 0.054
		c04+p02 * (c03+p02 * (c02+p02 * c01))
	}

	xmdl.4<-function(p02){
		c02 = 0.232
		c03 = 0.092
		c04 = 1.0
		c01 = -0.021
		c04+p02 * (c03+p02 * (c02+p02 * c01))
	}

	xmdl.5<-function(p02){
		c02 = 0.346
		c03 = -0.043
		c04 = 1.3
		c01 = -0.027
		c04+p02 * (c03+p02 * (c02+p02 * c01))
	}

	xmdl.6<-function(p02){
		c02 = -0.057
		c03 = 0.712
		c04 = 1.0
		c01 = 0.017
		c04+p02 * (c03+p02 * (c02+p02 * c01))
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		NA
	)))))))
}
