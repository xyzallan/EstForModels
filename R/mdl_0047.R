#' @title stand (stem), diameter (100 years old)
#' @return stand (stem), diameter (100 years old)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), density 
#' @export
#' @rdname mdl_D_47
mdl_D_47 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = -0.53
		c03 = -0.0055
		c01 = 1.94
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.2<-function(p02, p03){
		c02 = -0.28
		c03 = -0.0018
		c01 = 1.49
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.3<-function(p02, p03){
		c02 = -0.29
		c03 = 0.0132
		c01 = 1.08
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.4<-function(p02, p03){
		c02 = -0.25
		c03 = 0.0132
		c01 = 0.97
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.5<-function(p02, p03){
		c02 = -0.2
		c03 = 0.0153
		c01 = 0.99
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.6<-function(p02, p03){
		c02 = -0.15
		c03 = 0.0111
		c01 = 0.92
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.7<-function(p02, p03){
		c02 = -0.57
		c03 = -0.0003
		c01 = 1.99
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
	}

	xmdl.8<-function(p02, p03){
		c02 = -0.39
		c03 = 0.0052
		c01 = 1.5
		(c01+c02 * p03+c03 * p02) * (p02-1.3)
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
