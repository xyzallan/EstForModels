#' @title species level (stem), volume 
#' @return species level (stem), volume 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), height ()
#' @export
#' @rdname mdl_V_3
mdl_V_3 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 16.6305
		c03 = 0.0254
		c04 = 0.0
		c01 = -30.5946
		((c04 * p02+c03) * p02+c02) * p02+c01
	}

	xmdl.2<-function(p02){
		c02 = 9.2794
		c03 = 0.3473
		c04 = 0.0
		c01 = -7.988
		((c04 * p02+c03) * p02+c02) * p02+c01
	}

	xmdl.3<-function(p02){
		c04 = -0.0087
		c02 = 0.0
		c01 = 15.3442
		c03 = 0.7411
		((c04 * p02+c03) * p02+c02) * p02+c01
	}

	xmdl.4<-function(p02){
		c04 = 0.0
		c01 = -18.7579
		c02 = 8.3848
		c03 = 0.3233
		((c04 * p02+c03) * p02+c02) * p02+c01
	}

	xmdl.8<-function(p02){
		c02 = 8.4737
		c03 = 0.2767
		c04 = 0.0
		c01 = -11.7131
		((c04 * p02+c03) * p02+c02) * p02+c01
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('TO','NU','TS','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('PN','PP','RE','LV','LM','HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('TL','KP','JA','VA','SA','TA'), xmdl.8(p02), 
		NA
	))))))
}
