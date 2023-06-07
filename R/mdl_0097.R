#' @title single tree (stem), diameter (breast height)
#' @return single tree (stem), diameter (breast height)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (base)
#' @export
#' @rdname mdl_D_97
mdl_D_97 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = -1.53
		c01 = 0.85
		c01 * p02+c02
	}

	xmdl.2<-function(p02){
		c02 = 0.1
		c01 = 0.77
		c01 * p02+c02
	}

	xmdl.3<-function(p02){
		c02 = -0.33
		c01 = 0.79
		c01 * p02+c02
	}

	xmdl.4<-function(p02){
		c02 = -0.60
		c01 = 0.85
		c01 * p02+c02
	}

	xmdl.5<-function(p02){
		c02 = -0.24
		c01 = 0.82
		c01 * p02+c02
	}

	xmdl.6<-function(p02){
		c02 = 0.46
		c01 = 0.80
		c01 * p02+c02
	}

	xmdl.7<-function(p02){
		c02 = -0.76
		c01 = 0.84
		c01 * p02+c02
	}

	xmdl.8<-function(p02){
		c02 = -0.95
		c01 = 0.85
		c01 * p02+c02
	}

	xmdl.12<-function(p02){
		c02 = -3.19
		c01 = 0.96
		c01 * p02+c02
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
		ifelse(p01 %in% c('RE'), xmdl.12(p02), 
		NA
	))))))))))
}
