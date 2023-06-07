#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), age 
#' @param p04 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_V_31
mdl_V_31 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 2.806
		c03 = 0.0
		c04 = 4.730
		c05 = 0.0
		c06 = 0.0
		c07 = 0.0
		c01 = 6.631
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 2.421
		c03 = 2.923
		c04 = -2.100
		c05 = 2.650
		c06 = -0.556
		c07 = 0.0
		c01 = -3.600
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 0.0
		c03 = 0.0
		c04 = 0.0
		c05 = -2.530
		c06 = 0.0
		c07 = 3.026
		c01 = -1.650
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	xmdl.4<-function(p02, p03, p04){
		c02 = 0.0
		c03 = 0.0
		c04 = 0.0
		c05 = -5.117
		c06 = 0.0
		c07 = 4.236
		c01 = -1.320
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	xmdl.5<-function(p02, p03, p04){
		c02 = 0.0
		c03 = 0.0
		c04 = 0.0
		c05 = -4.760
		c06 = 0.0
		c07 = 4.120
		c01 = -4.600
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	xmdl.6<-function(p02, p03, p04){
		c02 = 0.0
		c03 = 0.0
		c04 = 0.0
		c05 = -6.100
		c06 = 0.0
		c07 = 4.450
		c01 = -3.500
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	xmdl.7<-function(p02, p03, p04){
		c02 = 3.900
		c03 = 0.0
		c04 = 0.0
		c05 = 0.0
		c06 = 0.0
		c07 = 0.0
		c01 = 1.900
		c01+c02 * (p02 / sqrt(p03))+c03 * (10 * p02 / sqrt(p03))+c04 * (p02)+c05 * (0.1 * p02^2)+c06 * (0.001 * p02^4 / log(p03))+c07 * (p02^2 / log(p03))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		NA
	))))))))
}
