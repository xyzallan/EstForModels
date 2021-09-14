#' @title Stem H
#' @return Stem H
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem D
#' @param p04 Stem H
mdl_H_155 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c02 = 1.30645374
		c01 = 0.25963741
		1.3+(p04-1.3) * exp(-(c01 * p03+c02) * (1 / p02-1 / p03))
	}
	xmdl.2<-function(p02, p03, p04){
		c02 = 5.68789430
		c01 = 0.18290951
		1.3+(p04-1.3) * exp(-(c01 * p03+c02) * (1 / p02-1 / p03))
	}
	xmdl.7<-function(p02, p03, p04){
		c02 = 3.78686023
		c01 = 0.14657227
		1.3+(p04-1.3) * exp(-(c01 * p03+c02) * (1 / p02-1 / p03))
	}
	xmdl.15<-function(p02, p03, p04){
		c02 = 4.44234560
		c01 = 0.12931522
		1.3+(p04-1.3) * exp(-(c01 * p03+c02) * (1 / p02-1 / p03))
	}
	xmdl.16<-function(p02, p03, p04){
		c02 = 4.63277655
		c01 = 0.19965100
		1.3+(p04-1.3) * exp(-(c01 * p03+c02) * (1 / p02-1 / p03))
	}
	xmdl.21<-function(p02, p03, p04){
		c02 = 5.64023296
		c01 = 0.20213328
		1.3+(p04-1.3) * exp(-(c01 * p03+c02) * (1 / p02-1 / p03))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('LH'), xmdl.15(p02, p03, p04), 
		ifelse(p01 %in% c('TS'), xmdl.16(p02, p03, p04), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03, p04), 
		NA
		)))))))
}
