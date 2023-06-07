#' @title stand (stem), formheight 
#' @return stand (stem), formheight 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
mdl_HF_41 <- function(p01, p02){

	xmdl.1<-function(p02){
		c03 = 0.00013
		c01 = 1.3
		c02 = 0.3866
		c01+c02 * p02+c03 * p02^2
	}

	xmdl.2<-function(p02){
		c02 = 0.4181
		c03 = 0.00003
		c01 = 1.23
		c01+c02 * p02+c03 * p02^2
	}

	xmdl.3<-function(p02){
		c02 = 0.422
		c03 = -0.000028
		c01 = 0.78
		c01+c02 * p02+c03 * p02^2
	}

	xmdl.4<-function(p02){
		c02 = 0.4476
		c03 = -0.001364
		c01 = 0.75
		c01+c02 * p02+c03 * p02^2
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('TA','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('LV','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('LM','HB'), xmdl.4(p02), 
		NA
	)))))
}
