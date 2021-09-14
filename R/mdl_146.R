#' @title Stem T
#' @return Stem T
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
mdl_T_146 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = -0.0001
		c03 = 0.0013
		c04 = -1.3304
		c05 = -37.3285
		c01 = 0.0156
		((c01+c02 * p03) * p02+c03 * p03)+(p03 / (c04 * p03+c05))^2
	}
	xmdl.2<-function(p02, p03){
		c02 = -0.0002
		c03 = 0.0020
		c04 = 1.2606
		c05 = 23.0083
		c01 = 0.0153
		((c01+c02 * p03) * p02+c03 * p03)+(p03 / (c04 * p03+c05))^2
	}
	xmdl.3<-function(p02, p03){
		c02 = 0.0001
		c03 = -0.0015
		c04 = 1.2644
		c05 = -0.6362
		c01 = -0.0024
		((c01+c02 * p03) * p02+c03 * p03)+(p03 / (c04 * p03+c05))^2
	}
	xmdl.4<-function(p02, p03){
		c02 = 0.0000
		c03 = -0.0005
		c04 = -1.4204
		c05 = -0.1398
		c01 = 0.0012
		((c01+c02 * p03) * p02+c03 * p03)+(p03 / (c04 * p03+c05))^2
	}
	xmdl.7<-function(p02, p03){
		c02 = -0.0001
		c03 = 0.0017
		c04 = -1.4029
		c05 = -19.0590
		c01 = 0.0158
		((c01+c02 * p03) * p02+c03 * p03)+(p03 / (c04 * p03+c05))^2
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('LM','HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('SA','TA'), xmdl.7(p02, p03), 
		NA
		))))))
}
