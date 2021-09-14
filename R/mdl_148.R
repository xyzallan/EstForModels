#' @title Stem H
#' @return Stem H
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_H_148 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = 1.1
		c03 = 9.9241
		c01 = 1.5601
		c01 * (p02 / (p02+c02))^c03
	}
	xmdl.2<-function(p02){
		c02 = 1.3
		c03 = 10.8578
		c01 = 1.7731
		c01 * (p02 / (p02+c02))^c03
	}
	xmdl.3<-function(p02){
		c02 = 8.0
		c03 = 1.4625
		c01 = 1.5231
		c01 * (p02 / (p02+c02))^c03
	}
	xmdl.4<-function(p02){
		c02 = 4.3
		c03 = 2.4979
		c01 = 1.5093
		c01 * (p02 / (p02+c02))^c03
	}
	xmdl.7<-function(p02){
		c02 = 1.6
		c03 = 8.2934
		c01 = 1.7079
		c01 * (p02 / (p02+c02))^c03
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA','LH'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU','NU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS','LV'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB','LM'), xmdl.4(p02), 
		ifelse(p01 %in% c('TA','SA'), xmdl.7(p02), 
		NA
		))))))
}
