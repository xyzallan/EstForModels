#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
mdl_V_40 <- function(p01, p02, p03){
	xmdl.3<-function(p02, p03){
		c02 = 3.026
		c03 = 0.253
		c01 = -1.65
		c01+(c02 / log(p03)-c03) * p02^2
	}
	xmdl.4<-function(p02, p03){
		c02 = 4.236
		c03 = 0.5117
		c01 = -3.22
		c01+(c02 / log(p03)-c03) * p02^2
	}
	xmdl.5<-function(p02, p03){
		c02 = 4.12
		c03 = 0.476
		c01 = -4.6
		c01+(c02 / log(p03)-c03) * p02^2
	}
	xmdl.6<-function(p02, p03){
		c02 = 4.45
		c03 = 0.61
		c01 = -3.5
		c01+(c02 / log(p03)-c03) * p02^2
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		NA
		)))))
}
