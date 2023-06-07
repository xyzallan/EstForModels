#' @title stand (stem), volume ()
#' @return stand (stem), volume ()
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), age (random point)
#' @param p04 stand (stem), volume 
#' @param p05 stand (area), humus layer 
#' @param p06 stand (area), cultivated 
mdl_V_11 <- function(p01, p02, p03, p04, p05, p06){

	xmdl.1<-function(p02, p03, p04, p05, p06){
		c02 = 1.93
		c01 = 380540
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.2<-function(p02, p03, p04, p05, p06){
		c02 = 2.20
		c01 = 875924
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.3<-function(p02, p03, p04, p05, p06){
		c02 = 2.05
		c01 = 446641
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.4<-function(p02, p03, p04, p05, p06){
		c02 = 1.77
		c01 = 310877
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.5<-function(p02, p03, p04, p05, p06){
		c02 = 1.93
		c01 = 378317
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.6<-function(p02, p03, p04, p05, p06){
		c02 = 1.78
		c01 = 205882
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.7<-function(p02, p03, p04, p05, p06){
		c02 = 2.02
		c01 = 277948
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
	}

	xmdl.8<-function(p02, p03, p04, p05, p06){
		c02 = 2.12
		c01 = 345440
		ipf1 = c01 - 54348 * log(p05 + 1) + 56290 * p06
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2)^2 + 4 * ipf1 * p04 / p02 ^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^-c02 / (p04-ipf2+ipf3))
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
		NA
	)))))))))
}
