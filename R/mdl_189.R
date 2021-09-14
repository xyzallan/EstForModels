#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem H
mdl_G_189 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = 1.2110
		c03 = -0.0175
		c01 = 3.9685
		c01+c02 * p02+c03 * p02^2
	}
	xmdl.2<-function(p02){
		c02 = 0.8393
		c03 = 0.0
		c01 = 3.7448
		c01+c02 * p02+c03 * p02^2
	}
	xmdl.3<-function(p02){
		c02 = 0.6235
		c03 = 0.0
		c01 = 2.3247
		c01+c02 * p02+c03 * p02^2
	}
	xmdl.4<-function(p02){
		c02 = 0.7095
		c03 = 0.0
		c01 = 3.2701
		c01+c02 * p02+c03 * p02^2
	}
	xmdl.5<-function(p02){
		c03 = 0.0
		c01 = 1.9956
		c02 = 0.7306
		c01+c02 * p02+c03 * p02^2
	}
	xmdl.7<-function(p02){
		c02 = 0.6315
		c03 = 0.0
		c01 = 2.8209
		c01+c02 * p02+c03 * p02^2
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB','LV'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('TA','SA','VA','JA'), xmdl.7(p02), 
		NA
		)))))))
}
