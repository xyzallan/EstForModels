#' @title single tree (stem), height 
#' @return single tree (stem), height 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @export
#' @rdname mdl_H_22
mdl_H_22 <- function(p01, p02){

	xmdl.1<-function(p02){
		c01 = 0.671
		c02 = 1.062
		c01 * p02+c02
	}

	xmdl.2<-function(p02){
		c01 = 0.712
		c02 = 0.984
		c01 * p02+c02
	}

	xmdl.3<-function(p02){
		c02 = 1.527
		c01 = 0.963
		c01 * p02+c02
	}

	xmdl.4<-function(p02){
		c02 = 1.455
		c01 = 0.991
		c01 * p02+c02
	}

	xmdl.5<-function(p02){
		c02 = 1.224
		c01 = 1.069
		c01 * p02+c02
	}

	xmdl.6<-function(p02){
		c02 = 1.384
		c01 = 1.140
		c01 * p02+c02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		NA
	)))))))
}
