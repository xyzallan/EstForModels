#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), bonitet 
#' @param p04 stand (stem), density 
mdl_V_179 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c03 = 1.0952
		c04 = 0.0007
		c05 = 2.4787
		c06 = -0.0005
		c01 = 1225.0
		c02 = -127.1
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	xmdl.2<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c05 = 4.9657
		c06 = -0.0018
		c02 = -145.8
		c03 = 1.7668
		c04 = 0.0005
		c01 = 1351.7
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	xmdl.3<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c02 = -107.6
		c03 = 1.7771
		c04 = 0.0011
		c05 =  3.2267
		c06 = -0.0015
		c01 = 821.4
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	xmdl.4<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c02 = -150.7
		c03 = 1.9665
		c04 = 0.0007
		c05 = 1.7283
		c06 = 0.0002
		c01 = 1057.6
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	xmdl.6<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c02 = -130.5
		c03 = 5.4757
		c04 = -0.0023
		c05 = 2.8239
		c06 = -0.0019
		c01 = 715.3
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	xmdl.7<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c02 = -146.5
		c03 = 2.8689
		c04 = -0.0011
		c05 = 2.6105
		c06 = -0.0009
		c01 = 1046.5
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	xmdl.8<-function(p02, p03, p04){
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c10 = 0.0
		c02 = -156.2
		c03 = 1.1949
		c04 = 0.0001
		c05 = 2.1747
		c06 = -0.0007
		c01 = 1112.8
		ipf1 = c01+c02*p03
		ipf2 = c03+c04*ipf1
		ipf3 = c05+c06*ipf1
		(ipf1 * ipf2 * ipf3 * (1-exp(-0.01 * ipf2 * p02))^(ipf3-1) * exp(-0.01 * ipf2 * p02)) / 100 * (((c07/max(p02, 30)^c08)+c09)-((c07/max(p02, 30)^c08)+c10)*(p04/100))*(p04/100)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA','SD','LH'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU','TO','TS','NU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS','LM'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB','PP','PN'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('LV','RE','TL'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('SA','VA','JA','KP'), xmdl.8(p02, p03, p04), 
		NA
	))))))))
}
