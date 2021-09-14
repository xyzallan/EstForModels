#' @title single tree (stem), diameter (base)
#' @return single tree (stem), diameter (base)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
mdl_D_91 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 1.15
		c03 = 0.0
		c01 = 2.17
		c01+c02 * p02-c03 / p02
	}

	xmdl.2<-function(p02){
		c02 = 1.348
		c03 = 0.0
		c01 = -0.94
		c01+c02 * p02-c03 / p02
	}

	xmdl.3<-function(p02){
		c02 = 1.09
		c03 = 41.34
		c01 = 7.34
		c01+c02 * p02-c03 / p02
	}

	xmdl.4<-function(p02){
		c02 = 1.13
		c03 = 0.0
		c01 = 0.37
		c01+c02 * p02-c03 / p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('LV','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('LM','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		NA
	)))))
}
