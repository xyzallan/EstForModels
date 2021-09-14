#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem A
#' @param p03 Stem D
mdl_D_45 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c03 = 0.18858
		c04 = 3.1
		c05 = 100
		c01 = 0.44824
		c02 = -0.36319
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.2<-function(p02, p03){
		c03 = 0.089791
		c04 = 4.0
		c05 = 100
		c01 = 0.4385
		c02 = -0.47171
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.3<-function(p02, p03){
		c02 = -0.74499
		c03 = 0.258
		c04 = 2.1
		c05 = 70
		c01 = 0.78063
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.4<-function(p02, p03){
		c02 = -1.41697
		c03 = -1.026
		c04 = 2.1
		c05 = 60
		c01 = 1.18603
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.5<-function(p02, p03){
		c02 = -0.5408
		c03 = 0.621
		c04 = 1.7
		c05 = 70
		c01 = 0.74558
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.6<-function(p02, p03){
		c02 = -2.7811
		c03 = 0.837
		c04 = 3.7
		c05 = 40
		c01 = 1.2537
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.7<-function(p02, p03){
		c02 = -0.2943
		c03 = -0.10104
		c04 = 2.9
		c05 = 100
		c01 = 0.80674
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}
	xmdl.8<-function(p02, p03){
		c02 = -0.36578
		c03 = 0.10038
		c04 = 1
		c05 = 100
		c01 = 0.52284
		ipf1 = (p02-c05)/100
		p03 * (1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)+c04 * ipf1
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03), 
		NA
		)))))))))
}
