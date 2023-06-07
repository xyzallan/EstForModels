#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
mdl_G_88 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 15.67
		c03 = -5
		c01 = 15.85
		c01+c02 * log10(p02+c03)
	}

	xmdl.2<-function(p02){
		c02 = 92.49
		c03 = 20
		c01 = -114.91
		c01+c02 * log10(p02+c03)
	}

	xmdl.3<-function(p02){
		c02 = 99.88
		c03 = 30
		c01 = -143.92
		c01+c02 * log10(p02+c03)
	}

	xmdl.4<-function(p02){
		c02 = 120.46
		c03 = 30
		c01 = -173.99
		c01+c02 * log10(p02+c03)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('LV','LM','HB'), xmdl.4(p02), 
		NA
	)))))
}
