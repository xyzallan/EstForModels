#' @title stand (stem), number of trees (base)
#' @return stand (stem), number of trees (base)
#' @param p01 stand (stem), species ()
#' @param p02 stand (stem), number of trees (random point)
#' @param p03 stand (stem), diameter (breast height)
#' @param p04 stand (stem), diameter (breast height)
mdl_N_56 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = -1.54038
		c03 = -0.6744
		c04 = 0.76537
		c01 = 3.1107
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = -1.42629
		c03 = -0.85544
		c04 = 0.76537
		c01 = 2.99151
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = -1.79834
		c03 = -0.53367
		c04 = 0.69237
		c01 = 3.10934
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.4<-function(p02, p03, p04){
		c02 = -2.51157
		c03 = 0.57348
		c04 = 0.14743
		c01 = 3.38133
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.5<-function(p02, p03, p04){
		c04 = 2.15344
		c01 = 2.60968
		c02 = -0.13959
		c03 = -3.23323
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.6<-function(p02, p03, p04){
		c03 = -2.32232
		c04 = 2.41759
		c01 = 3.2396
		c02 = -1.901
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.7<-function(p02, p03, p04){
		c02 = -1.4554
		c03 = 0.0
		c04 = 0.16182
		c01 = 3.0671
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
	}

	xmdl.8<-function(p02, p03, p04){
		c02 = -2.1786
		c03 = -0.06941
		c04 = 0.74158
		c01 = 2.9369
		ipf1 = p04/p03
		p02 / exp((1-ipf1) * (((c04 * ipf1+c03) * ipf1+c02) * ipf1+c01))
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
