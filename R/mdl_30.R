#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
#' @param p04 Stem H
mdl_D_30 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c02 = 0.0075
		c03 = 0.0
		c04 = -0.1038
		c05 = 0.0
		c06 = -0.0518
		c07 = 0.0006315
		c08 = 0.0
		c09 = 100
		c01 = 2.322
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
	}
	xmdl.2<-function(p02, p03, p04){
		c02 = -0.000156
		c03 = -4.8
		c04 = 0.0
		c05 = 0.0
		c06 = -0.044
		c07 = 0.00055
		c08 = 0.0
		c09 = 100
		c01 = 1.92
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
	}
	xmdl.3<-function(p02, p03, p04){
		c02 = 0.0
		c03 = 0.0
		c04 = 0.0
		c05 = 0.146
		c06 = 0.0
		c07 = 0.0
		c08 = -0.2976
		c09 = 50
		c01 = 1.245
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
	}
	xmdl.4<-function(p02, p03, p04){
		c03 = 0.0
		c04 = 0.0
		c05 = 0.3066
		c06 = 0.0
		c07 = 0.0
		c08 = -0.4064
		c02 = 0.0
		c09 = 50
		c01 = 1.052
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
	}
	xmdl.5<-function(p02, p03, p04){
		c03 = 0.0
		c04 = 0.0
		c05 = 0.148
		c06 = 0.0
		c07 = 0.0
		c08 = -0.401
		c09 = 50
		c01 = 1.588
		c02 = 0.0
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
	}
	xmdl.6<-function(p02, p03, p04){
		c03 = 0.0
		c04 = 0.0
		c05 = 0.128
		c06 = 0.0
		c07 = 0.0
		c08 = -0.3
		c09 = 50
		c01 = 1.353
		c02 = 0.0
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
	}
	xmdl.7<-function(p02, p03, p04){
		c02 = 0.002
		c03 = 0.0
		c04 = 0.0
		c05 = 0.0
		c06 = -0.047
		c07 = 0.0
		c08 = 0.0
		c09 = 100
		c01 = 2.18
		p02 * (c01+c02 * p03+c03 / p03+c04 * sqrt(p03)+c05 * log(p03)+c06 * p04+c07 * p04^2+c08 * log(p04))
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
