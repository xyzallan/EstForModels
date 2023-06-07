#' @title stand (stem), volume ()
#' @return stand (stem), volume ()
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
#' @param p03 stand (stem), height ()
#' @param p04 stand (stem), number of trees 
#' @export
#' @rdname mdl_V_187
mdl_V_187 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.6321
		c03 = 13.4558
		c04 = 3.3642
		c01 = 0.0000785
		c01 * p02^2 * p03 * (c02+c03 / p03^c04) * p04
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.6819
		c03 = 55.1416
		c04 = 4.7457
		c01 = 0.0000785
		c01 * p02^2 * p03 * (c02+c03 / p03^c04) * p04
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 0.5922
		c03 = 46.7815
		c04 = 4.1932
		c01 = 0.0000785
		c01 * p02^2 * p03 * (c02+c03 / p03^c04) * p04
	}

	xmdl.4<-function(p02, p03, p04){
		c02 = 0.5964
		c03 = 28.1186
		c04 = 3.7832
		c01 = 0.0000785
		c01 * p02^2 * p03 * (c02+c03 / p03^c04) * p04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA','SD','LH'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU','TS','NU','TO'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS','PN','TA','SA','VA','VA','KP','RE','PP','TL'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB','LM','LV'), xmdl.4(p02, p03, p04), 
		NA
	)))))
}
