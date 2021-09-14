#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), age (random point)
#' @param p04 stand (stem), height ()
#' @param p05 stand (area), cultivated 
#' @param p06 stand (area), humus layer 
mdl_H_9 <- function(p01, p02, p03, p04, p05, p06){

	xmdl.1<-function(p02, p03, p04, p05, p06){
		c02 = 1.58
		c01 = 8319
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.2<-function(p02, p03, p04, p05, p06){
		c02 = 1.71
		c01 = 12867
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.3<-function(p02, p03, p04, p05, p06){
		c02 = 1.48
		c01 = 4990
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.4<-function(p02, p03, p04, p05, p06){
		c02 = 1.30
		c01 = 3882
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.5<-function(p02, p03, p04, p05, p06){
		c02 = 1.41
		c01 = 4228
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.6<-function(p02, p03, p04, p05, p06){
		c02 = 1.38
		c01 = 2749
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.7<-function(p02, p03, p04, p05, p06){
		c02 = 1.61
		c01 = 6742
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.8<-function(p02, p03, p04, p05, p06){
		c02 = 1.35
		c01 = 3732
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	xmdl.15<-function(p02, p03, p04, p05, p06){
		c02 = 1.38
		c01 = 903
		ipf1 = c01 - 493 * log(p06 + 1) + 1355 * p05
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * (p03^(-c02)) / (p04-ipf2+ipf3))
	}

	with(data.frame( p01, p02, p03, p04, p05, p06 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04, p05, p06), 
		ifelse(p01 %in% c('LH'), xmdl.15(p02, p03, p04, p05, p06), 
		NA
	))))))))))
}
