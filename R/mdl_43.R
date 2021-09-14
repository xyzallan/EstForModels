#' @title species level (stem), diameter (breast height)
#' @return species level (stem), diameter (breast height)
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
mdl_D_43 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = -0.36319
		c03 = 0.18858
		c04 = 100
		c05 = 3.1
		c01 = 0.44824
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.2<-function(p02){
		c02 = -0.47171
		c03 = 0.089791
		c04 = 100
		c05 = 4.0
		c01 = 0.4385
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.3<-function(p02){
		c02 = -0.74499
		c03 = 0.258
		c04 = 70
		c05 = 2.1
		c01 = 0.78063
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.4<-function(p02){
		c02 = -1.4169
		c03 = -1.026
		c04 = 60
		c05 = 2.1
		c01 = 1.18603
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.5<-function(p02){
		c02 = -0.5408
		c03 = 0.621
		c04 = 70
		c05 = 1.7
		c01 = 0.74558
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.6<-function(p02){
		c02 = -2.7811
		c03 = 0.837
		c04 = 40
		c05 = 3.7
		c01 = 1.2537
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.7<-function(p02){
		c02 = -0.2943
		c03 = -0.10104
		c04 = 100
		c05 = 2.9
		c01 = 0.80674
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	xmdl.8<-function(p02){
		c02 = -0.36578
		c03 = 0.10038
		c04 = 100
		c05 = 1.0
		c01 = 0.53384
		ipf1 = (p02-c04)/100
		(1+((c03 * ipf1+c02) * ipf1+c01) * ipf1)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02), 
		NA
	)))))))))
}
