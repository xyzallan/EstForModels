#' @title species level (stem), volume 
#' @return species level (stem), volume 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), height ()
#' @param p03 species level (stem), basal area ()
mdl_V_6 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c03 = -0.0617
		c04 =  0.2107
		c02 =  2.5936
		c01 = -0.0309
		p02 * p03 * (c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02))
	}

	xmdl.2<-function(p02, p03){
		c02 =  1.5653
		c04 =  0.0172
		c03 =  0.0000
		c01 =  0.3495
		p02 * p03 * (c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02))
	}

	xmdl.3<-function(p02, p03){
		c02 =  0.8180
		c04 =  0.0000
		c01 =  0.4193
		c03 =  0.0000
		p02 * p03 * (c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02))
	}

	xmdl.4<-function(p02, p03){
		c02 = -0.5950
		c03 =  0.0437
		c04 = -0.1969
		c01 =  0.8813
		p02 * p03 * (c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02))
	}

	xmdl.7<-function(p02, p03){
		c02 =  0.9350
		c03 =  0.0286
		c04 = -0.1006
		c01 =  0.5993
		p02 * p03 * (c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('TO','NU','TS','KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('PN','KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('TL','PP','RE','LV','LM','HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.7(p02, p03), 
		NA
	))))))
}
