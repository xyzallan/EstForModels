#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem H
mdl_G_177 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = -3.8531
		c03 = 0.0226
		c04 = 30.2955
		c01 = -33.02
		c01+c02 * p02+c03 * p02^2+c04 * sqrt(p02)
	}
	xmdl.2<-function(p02){
		c02 = -0.5206
		c03 = 0.0069
		c04 = 10.8713
		c01 = -7.94
		c01+c02 * p02+c03 * p02^2+c04 * sqrt(p02)
	}
	xmdl.3<-function(p02){
		c02 = 2.0183
		c03 = -0.0155
		c04 = -4.4908
		c01 = 11.65
		c01+c02 * p02+c03 * p02^2+c04 * sqrt(p02)
	}
	xmdl.4<-function(p02){
		c02 = 1.4932
		c03 = -0.0076
		c04 = -0.9684
		c01 = 7.93
		c01+c02 * p02+c03 * p02^2+c04 * sqrt(p02)
	}
	xmdl.7<-function(p02){
		c02 = 0.7335
		c03 = -0.0039
		c04 = 3.1637
		c01 = 1.13
		c01+c02 * p02+c03 * p02^2+c04 * sqrt(p02)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA','LH','SD'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU','NU','TS','TO'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS','PN'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB','LM','LV'), xmdl.4(p02), 
		ifelse(p01 %in% c('TA','SA','VA','JA','TL'), xmdl.7(p02), 
		NA
		))))))
}
