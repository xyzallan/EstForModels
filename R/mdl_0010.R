#' @title stand (stem), diameter (breast height)
#' @return stand (stem), diameter (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), age (random point)
#' @param p04 stand (stem), diameter (breast height)
#' @param p05 stand (area), humus layer 
#' @export
#' @rdname mdl_D_10
mdl_D_10 <- function(p01, p02, p03, p04, p05){

	xmdl.1<-function(p02, p03, p04, p05){
		c02 = 1.33
		c01 = 6051
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.2<-function(p02, p03, p04, p05){
		c02 = 1.54
		c01 = 9805
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.3<-function(p02, p03, p04, p05){
		c02 = 1.37
		c01 = 5034
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.4<-function(p02, p03, p04, p05){
		c02 = 1.15
		c01 = 7092
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.5<-function(p02, p03, p04, p05){
		c02 = 1.41
		c01 = 4438
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.6<-function(p02, p03, p04, p05){
		c02 = 1.35
		c01 = 2864
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.7<-function(p02, p03, p04, p05){
		c02 = 1.45
		c01 = 10509
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	xmdl.8<-function(p02, p03, p04, p05){
		c02 = 1.03
		c01 = 5405
		ipf1 = c01 - 306 * log(p05 + 1)
		ipf2 = ipf1 / 50 ^ c02
		ipf3 = sqrt((p04 - ipf2) ^ 2 + 4 * ipf1 * p04/p02^ c02)
		(p04+ipf2+ipf3) / (2+4 * ipf1 * p03^(-c02) / (p04-ipf2+ipf3))
	}

	with(data.frame( p01, p02, p03, p04, p05 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04, p05), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04, p05), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04, p05), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04, p05), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04, p05), 
		NA
	)))))))))
}
