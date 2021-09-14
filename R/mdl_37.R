#' @title Stem L
#' @return Stem L
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
#' @param p04 Area OHOR
mdl_L_37 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c06 = 1
		c02 = 0.24009
		c03 = -0.002887
		c04 = 0.0
		c05 = -0.00348
		c01 = 0.3011
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.2<-function(p02, p03, p04){
		c04 = 0.0
		c05 = -0.00509
		c06 = 1
		c01 = 0.4835
		c02 = 0.28678
		c03 = -0.004690
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.3<-function(p02, p03, p04){
		c03 = -0.00222
		c04 = -0.0457
		c05 = -0.147
		c06 = 0.0
		c01 = 2.275
		c02 = 0.2088
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.4<-function(p02, p03, p04){
		c02 = 0.2799
		c03 = -0.00448
		c04 = 0.0
		c05 = -0.120
		c06 = 0.0
		c01 = 0.8143
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.5<-function(p02, p03, p04){
		c02 = 0.3693
		c03 = -0.00788
		c04 = 0.02913
		c05 = -0.0107
		c06 = 1
		c01 = 0.03152
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.6<-function(p02, p03, p04){
		c02 = 0.2111
		c03 = -0.00319
		c04 = 0.0
		c05 = -0.00369
		c06 = 1
		c01 = 0.8337
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.7<-function(p02, p03, p04){
		c02 = 0.3423
		c03 = -0.00756
		c04 = 0.01482
		c05 = 0.0
		c06 = 0.0
		c01 = 0.1644
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}
	xmdl.8<-function(p02, p03, p04){
		c02 = 0.3322
		c03 = -0.00623
		c04 = -0.0119
		c05 = -0.20
		c06 = 0.0
		c01 = 0.8116
		(c01)+(c02 * p02)+(c03 * p02 * p03)+(c04 * p03)+(c05 * p02^c06 * log(p04+1))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04), 
		NA
		)))))))))
}
