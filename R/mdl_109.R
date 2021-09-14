#' @title Stem A
#' @return Stem A
#' @param p01 Stem Spec
#' @param p02 Stem H
mdl_A_109 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = -2.8
		c03 = 0.0
		c01 = 177.1
		c01+c02 * p02+c03 * (abs(p02-21.5))^0.5
	}
	xmdl.2<-function(p02){
		c02 = -2.5
		c03 = 0.0
		c01 = 153.75
		c01+c02 * p02+c03 * (abs(p02-21.5))^0.5
	}
	xmdl.3<-function(p02){
		c02 = 0.0
		c03 = -4.18
		c01 = 83
		c01+c02 * p02+c03 * (abs(p02-21.5))^0.5
	}
	xmdl.4<-function(p02){
		c02 = 0.0
		c03 = 0.0
		c01 = 50
		c01+c02 * p02+c03 * (abs(p02-21.5))^0.5
	}
	xmdl.6<-function(p02){
		c02 = 0.0
		c03 = 0.0
		c01 = 30
		c01+c02 * p02+c03 * (abs(p02-21.5))^0.5
	}
	xmdl.8<-function(p02){
		c02 = -2.5
		c03 = 0.0
		c01 = 183.75
		c01+c02 * p02+c03 * (abs(p02-21.5))^0.5
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('LM','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('SA','TA'), xmdl.8(p02), 
		NA
		)))))))
}
