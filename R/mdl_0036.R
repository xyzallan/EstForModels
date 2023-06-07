#' @title species level (stem), basal area (breast height)
#' @return species level (stem), basal area (breast height)
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_G_36
mdl_G_36 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c12 = 0.05
		c11 = -2.78
		c10 = 8.479
		c02 = -0.00592
		c03 = 1
		c04 = 0.1448
		c05 = 1
		c06 = -9.90
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c01 = -9.02
		(c01)+(c02 * p03 * p02^c03)+(c04 * p02^c05)+(c06 * log(log(p02)))+(c07 * p02^c08)+(c09 * p03)+(c10 * log(log(p02)) * sqrt(p03))+(c11 * sqrt(p03) * p02^c12)
	}

	xmdl.2<-function(p02, p03){
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.004630
		c03 = 0.5
		c04 = -212
		c05 = -0.5
		c06 = 0.0
		c07 = -47.7
		c08 = 0.1
		c09 = 0.5494
		c01 = 103.9
		(c01)+(c02 * p03 * p02^c03)+(c04 * p02^c05)+(c06 * log(log(p02)))+(c07 * p02^c08)+(c09 * p03)+(c10 * log(log(p02)) * sqrt(p03))+(c11 * sqrt(p03) * p02^c12)
	}

	xmdl.3<-function(p02, p03){
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.06809
		c03 = 0.5
		c04 = -5.82
		c05 = 0.1
		c06 = 10.87
		c07 = -0.209
		c08 = 0.5
		c09 = 0.0
		c01 = -1.06
		(c01)+(c02 * p03 * p02^c03)+(c04 * p02^c05)+(c06 * log(log(p02)))+(c07 * p02^c08)+(c09 * p03)+(c10 * log(log(p02)) * sqrt(p03))+(c11 * sqrt(p03) * p02^c12)
	}

	xmdl.4<-function(p02, p03){
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.07656
		c03 = 0.5
		c04 = 10.09
		c05 = 0.1
		c06 = 0.0
		c07 = 0.0
		c08 = 0.0
		c09 = 0.0
		c01 = -12.1
		(c01)+(c02 * p03 * p02^c03)+(c04 * p02^c05)+(c06 * log(log(p02)))+(c07 * p02^c08)+(c09 * p03)+(c10 * log(log(p02)) * sqrt(p03))+(c11 * sqrt(p03) * p02^c12)
	}

	xmdl.7<-function(p02, p03){
		c02 = 0.004047
		c03 = 0.5
		c04 = 72.91
		c05 = 0.1
		c06 = -4.65
		c07 = 0.0
		c08 = 0.0
		c09 = 0.5646
		c01 = -98.6
		(c01)+(c02 * p03 * p02^c03)+(c04 * p02^c05)+(c06 * log(log(p02)))+(c07 * p02^c08)+(c09 * p03)+(c10 * log(log(p02)) * sqrt(p03))+(c11 * sqrt(p03) * p02^c12)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		NA
	))))))
}
