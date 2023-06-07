#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
#' @export
#' @rdname mdl_L_129
mdl_L_129 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 105.0
		c01 = 12.9
		c01 * p02+c02
	}

	xmdl.2<-function(p02){
		c02 = 96.0
		c01 = 14.9
		c01 * p02+c02
	}

	xmdl.3<-function(p02){
		c02 = 105.0
		c01 = 16.8
		c01 * p02+c02
	}

	xmdl.4<-function(p02){
		c02 = 97.5
		c01 = 16.1
		c01 * p02+c02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		NA
	)))))
}
