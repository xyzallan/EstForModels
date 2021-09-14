#' @title Stem H
#' @return Stem H
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem D
#' @param p04 Stem D
mdl_H_205 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c02 = 0.00434
		c03 = 1.48076
		c01 = 0.3183
		p02/(1-(c01-c02*p03)*(1-(p03/p04)^c03))
	}
	xmdl.2<-function(p02, p03, p04){
		c02 = 0.00643
		c03 = 1.37255
		c01 = 0.44103
		p02/(1-(c01-c02*p03)*(1-(p03/p04)^c03))
	}
	xmdl.3<-function(p02, p03, p04){
		c02 = 0.00629
		c03 = 1.32998
		c01 = 0.39554
		p02/(1-(c01-c02*p03)*(1-(p03/p04)^c03))
	}
	xmdl.4<-function(p02, p03, p04){
		c02 = 0.00582
		c03 = 2.04430
		c01 = 0.25803
		p02/(1-(c01-c02*p03)*(1-(p03/p04)^c03))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA','LH','SD'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU','NU','TO','TS'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS','PN'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB','LV','LM'), xmdl.4(p02, p03, p04), 
		NA
		)))))
}
