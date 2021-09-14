#' @title Stem H
#' @return Stem H
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
mdl_H_51 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = -0.37747
		c03 = 0.40062
		c04 = 3.4
		c05 = 100
		c01 = 0.22191
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.2<-function(p02, p03){
		c02 = -0.49125
		c03 = 0.374
		c04 = 5.3
		c05 = 100
		c01 = 0.1348
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.3<-function(p02, p03){
		c02 = -0.8942
		c03 = 0.88715
		c04 = 3.7
		c05 = 70
		c01 = 0.36793
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.4<-function(p02, p03){
		c02 = -1.2545
		c03 = 0.82943
		c04 = 3.4
		c05 = 60
		c01 = 0.61536
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.5<-function(p02, p03){
		c02 = -0.75888
		c03 = 1.2127
		c04 = 3.7
		c05 = 70
		c01 = 0.30313
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.6<-function(p02, p03){
		c02 = -2.1044
		c03 = 6.6193
		c04 = 5.1
		c05 = 40
		c01 = 0.59916
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.7<-function(p02, p03){
		c02 = -0.45015
		c03 = 0.21242
		c04 = 3.0
		c05 = 100
		c01 = 0.33743
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
	}
	xmdl.8<-function(p02, p03){
		c02 = -0.26804
		c03 = 0.436
		c04 = 2.1
		c05 = 100
		c01 = 0.29621
		ipf1 = p03-c05
		1.3+((p02-1.3-c04 / 100 * ipf1) / (1+c01 * ipf1 / 100+c02 * (ipf1^2) / 1e4+c03 * (ipf1^3) / 1e6))
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
