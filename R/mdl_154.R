#' @title Stem HF
#' @return Stem HF
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem D
mdl_HF_154 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 3.387
		c03 = -0.494392
		c04 = 3.67116
		c05 = -1.83211
		c06 = 0.273999
		c07 = -0.459282
		c08 = 0.29989
		c09 = -0.0444931
		c01 = -5.80915
		ipf1 = c01+c02*log(p03)+c03*log(p03)^2
		ipf2 = c04+c05*log(p03)+c06*log(p03)^2
		ipf3 = c07+c08*log(p03)+c09*log(p03)^2
		exp(ipf1+ipf2 * log(p02)+ipf3 * log(p02)^2)
	}
	xmdl.2<-function(p02, p03){
		c02 = 1.80213
		c03 = -0.288243
		c04 = 1.06247
		c05 = -0.128993
		c06 = 0.0353434
		c07 = 0.142264
		c08 = -0.058259
		c09 = 0.0045985
		c01 = -3.59624
		ipf1 = c01+c02*log(p03)+c03*log(p03)^2
		ipf2 = c04+c05*log(p03)+c06*log(p03)^2
		ipf3 = c07+c08*log(p03)+c09*log(p03)^2
		exp(ipf1+ipf2 * log(p02)+ipf3 * log(p02)^2)
	}
	xmdl.15<-function(p02, p03){
		c02 = 4.75438
		c03 = -0.672495
		c04 = 5.17159
		c05 = -2.27654
		c06 = 0.311633
		c07 = -0.555379
		c08 = 0.302799
		c09 = -0.041251
		c01 = -9.26182
		ipf1 = c01+c02*log(p03)+c03*log(p03)^2
		ipf2 = c04+c05*log(p03)+c06*log(p03)^2
		ipf3 = c07+c08*log(p03)+c09*log(p03)^2
		exp(ipf1+ipf2 * log(p02)+ipf3 * log(p02)^2)
	}
	xmdl.16<-function(p02, p03){
		c02 = 6.62441
		c03 = -0.911185
		c04 = 7.27277
		c05 = -3.58346
		c06 = 0.489149
		c07 = -0.87715
		c08 = 0.515586
		c09 = -0.0714395
		c01 = -12.5017
		ipf1 = c01+c02*log(p03)+c03*log(p03)^2
		ipf2 = c04+c05*log(p03)+c06*log(p03)^2
		ipf3 = c07+c08*log(p03)+c09*log(p03)^2
		exp(ipf1+ipf2 * log(p02)+ipf3 * log(p02)^2)
	}
	xmdl.18<-function(p02, p03){
		c02 = 3.33667
		c03 = -0.426419
		c04 = 4.00998
		c05 = -1.39533
		c06 = 0.165198
		c07 = -0.321612
		c08 = 0.14401
		c09 = -0.0165461
		c01 = -7.41365
		ipf1 = c01+c02*log(p03)+c03*log(p03)^2
		ipf2 = c04+c05*log(p03)+c06*log(p03)^2
		ipf3 = c07+c08*log(p03)+c09*log(p03)^2
		exp(ipf1+ipf2 * log(p02)+ipf3 * log(p02)^2)
	}
	xmdl.21<-function(p02, p03){
		c02 = 0.837563
		c03 = -0.105843
		c04 = 1.62283
		c05 = -0.214812
		c06 = 0.0289272
		c07 = -0.087972
		c08 = 0.0325667
		c09 = -0.004463
		c01 = -2.7284
		ipf1 = c01+c02*log(p03)+c03*log(p03)^2
		ipf2 = c04+c05*log(p03)+c06*log(p03)^2
		ipf3 = c07+c08*log(p03)+c09*log(p03)^2
		exp(ipf1+ipf2 * log(p02)+ipf3 * log(p02)^2)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('LH'), xmdl.15(p02, p03), 
		ifelse(p01 %in% c('TS'), xmdl.16(p02, p03), 
		ifelse(p01 %in% c('NU'), xmdl.18(p02, p03), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03), 
		NA
		)))))))
}
