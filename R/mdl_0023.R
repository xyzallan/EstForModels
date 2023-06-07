#' @title single tree (stem), diameter (base)
#' @return single tree (stem), diameter (base)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
mdl_D_23 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 1.522
		c01 = 0.915
		c01 * p02+c02
	}

	xmdl.2<-function(p02){
		c02 = 1.449
		c01 = 0.893
		c01 * p02+c02
	}

	xmdl.3<-function(p02){
		c02 = 0.996
		c01 = 0.804
		c01 * p02+c02
	}

	xmdl.4<-function(p02){
		c02 = 0.565
		c01 = 0.876
		c01 * p02+c02
	}

	xmdl.5<-function(p02){
		c02 = 0.634
		c01 = 0.786
		c01 * p02+c02
	}

	xmdl.6<-function(p02){
		c02 = 0.895
		c01 = 0.901
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
