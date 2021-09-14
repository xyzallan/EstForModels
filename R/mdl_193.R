#' @title Area L
#' @return Area L
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_L_193 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 0.1834
		c03 = -0.00216
		c04 = 0.008641
		c01 = -0.00437
		c01+c02 * p02+c03 * (p02 * p03)+c04 * p03
	}
	xmdl.2<-function(p02, p03){
		c02 = 0.1556
		c03 = -0.00181
		c04 = 0.003598
		c01 = 0.1807
		c01+c02 * p02+c03 * (p02 * p03)+c04 * p03
	}
	xmdl.3<-function(p02, p03){
		c02 = 0.1822
		c03 = -0.00151
		c04 = 0.00671
		c01 = 0.4083
		c01+c02 * p02+c03 * (p02 * p03)+c04 * p03
	}
	xmdl.4<-function(p02, p03){
		c02 = 0.1991
		c03 = -0.00277
		c04 = 0.01046
		c01 = 0.02032
		c01+c02 * p02+c03 * (p02 * p03)+c04 * p03
	}
	xmdl.6<-function(p02, p03){
		c02 = 0.1878
		c03 = -0.00277
		c04 = 0.0000325
		c01 = 0.3867
		c01+c02 * p02+c03 * (p02 * p03)+c04 * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LV','LM','RE'), xmdl.6(p02, p03), 
		NA
		))))))
}
