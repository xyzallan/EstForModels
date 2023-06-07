#' @title species level (stem), volume ()
#' @return species level (stem), volume ()
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 stand (stem), density 
#' @param p04 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_V_16
mdl_V_16 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.01728
		c03 = 2.113
		c04 = 0.041
		c05 = 1741
		c06 = 2
		c07 = -0.133
		c08 = 1.801
		c01 = 843
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.02156
		c03 = 3.321
		c04 = 0.344
		c05 = 10.44
		c06 = 1
		c07 = 0.24
		c08 = 0.588
		c01 = 930
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	xmdl.3<-function(p02, p03, p04){
		c03 = 2.451
		c02 = 0.02352
		c01 = 536
		c04 = 0.041
		c05 = 1741
		c06 = 2
		c07 = -0.133
		c08 = 1.801
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	xmdl.4<-function(p02, p03, p04){
		c03 = 1.837
		c04 = 0.041
		c05 = 1741
		c06 = 2
		c07 = -0.133
		c08 = 1.801
		c01 = 621
		c02 = 0.02170
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	xmdl.6<-function(p02, p03, p04){
		c02 = 0.04743
		c03 = 2.230
		c04 = 0.041
		c05 = 1741
		c06 = 2
		c07 = -0.133
		c08 = 1.801
		c01 = 328
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	xmdl.7<-function(p02, p03, p04){
		c03 = 2.055
		c04 = 0.041
		c05 = 1741
		c06 = 2
		c07 = -0.133
		c08 = 1.801
		c01 = 608
		c02 = 0.02241
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	xmdl.8<-function(p02, p03, p04){
		c02 = 0.01264
		c03 = 1.702
		c04 = 0.041
		c05 = 1741
		c06 = 2
		c07 = -0.133
		c08 = 1.801
		c01 = 632
		ipf1 = iif(p02<30,c08,iif(p02>100,c04,c05/p02^c06+c07))
		ipf2 = ((ipf1+1)-ipf1*p03)*p03
		c01 * c02 * c03 * (1-exp(-c02 * p02))^(c03-1) * exp(-c02 * p02) * ipf2
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('TO','NU','TS','KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('LM','KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('PN','PP','HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('TL','RE','LV'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('KP','JA','VA','SA'), xmdl.8(p02, p03, p04), 
		NA
	))))))))
}
