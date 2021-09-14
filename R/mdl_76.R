#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_V_76 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 2.01395
		c03 = 2.07025
		c04 = -1.07209
		c05 = -0.0032473
		c01 = -3.32176
		exp(c01+c02 * log(p02)+c03 * log(p03)+c04 * log(p03-1.3)+c05 * p02) / 1000
	}
	xmdl.2<-function(p02, p03){
		c02 = 1.91505
		c03 = 2.82541
		c04 = -1.53547
		c05 = -0.0085726
		c01 = -3.77543
		exp(c01+c02 * log(p02)+c03 * log(p03)+c04 * log(p03-1.3)+c05 * p02) / 1000
	}
	xmdl.3<-function(p02, p03){
		c03 = 3.98519
		c04 = -2.659
		c05 = -0.014097
		c01 = -4.49213
		c02 = 2.10253
		exp(c01+c02 * log(p02)+c03 * log(p03)+c04 * log(p03-1.3)+c05 * p02) / 1000
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
		))))
}
