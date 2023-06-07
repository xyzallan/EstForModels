#' @title single tree (stem), diameter (base)
#' @return single tree (stem), diameter (base)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @export
#' @rdname mdl_D_4
mdl_D_4 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 1.150
		c01 = 2.17
		c01+c02 * p02
	}

	xmdl.2<-function(p02){
		c02 = 1.266
		c01 = 0.00
		c01+c02 * p02
	}

	xmdl.3<-function(p02){
		c02 = 1.242
		c01 = 1.59
		c01+c02 * p02
	}

	xmdl.4<-function(p02){
		c02 = 1.130
		c01 = 0.37
		c01+c02 * p02
	}

	xmdl.5<-function(p02){
		c02 = 1.194
		c01 = 1.03
		c01+c02 * p02
	}

	xmdl.6<-function(p02){
		c02 = 1.111
		c01 = 1.41
		c01+c02 * p02
	}

	xmdl.8<-function(p02){
		c02 = 1.170
		c01 = 2.27
		c01+c02 * p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('TO','NU','TS','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('PN','PP','HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('TL','RE','LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.8(p02), 
		NA
	))))))))
}
