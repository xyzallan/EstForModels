#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_D_92 <- function(p01, p02){
	xmdl.1<-function(p02){
		c03 = 0.0
		c01 = -1.89
		c02 = 0.87
		c01+c02 * p02+c03 / p02
	}
	xmdl.2<-function(p02){
		c03 = 0.0
		c01 = 0.7
		c02 = 0.742
		c01+c02 * p02+c03 / p02
	}
	xmdl.3<-function(p02){
		c02 = 0.916
		c03 = 50.4
		c01 = -6.7
		c01+c02 * p02+c03 / p02
	}
	xmdl.4<-function(p02){
		c02 = 0.88
		c03 = 0.0
		c01 = -0.33
		c01+c02 * p02+c03 / p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('LV','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('LM','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		NA
		)))))
}
