#' @title single tree (stem), formheight 
#' @return single tree (stem), formheight 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), height 
#' @param p03 single tree (stem), diameter (breast height)
#' @param p04 single tree (stem), diameter (6 meters)
#' @export
#' @rdname mdl_HF_99
mdl_HF_99 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = -0.000733
		c03 = 0.004635
		c04 = 3.57708
		c05 = 1.23011
		c06 = 0.56326
		c01 = -0.19633
		p02 * (c01+c02 * p03+c03 * p02+c04 / p02+c05 * (p03^2+p03 * p04+p04^2) / (p03^2 * p02)+c06 * p04^2 * (p02-6) / (p03^2 * p02))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = -0.001518
		c03 = 0.005905
		c04 = 2.71492
		c05 = 1.72359
		c06 = 0.47726
		c01 = -0.20078
		p02 * (c01+c02 * p03+c03 * p02+c04 / p02+c05 * (p03^2+p03 * p04+p04^2) / (p03^2 * p02)+c06 * p04^2 * (p02-6) / (p03^2 * p02))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = -0.001635
		c03 = 0.006053
		c04 = 3.02164
		c05 = 1.26247
		c06 = 0.42652
		c01 = -0.14726
		p02 * (c01+c02 * p03+c03 * p02+c04 / p02+c05 * (p03^2+p03 * p04+p04^2) / (p03^2 * p02)+c06 * p04^2 * (p02-6) / (p03^2 * p02))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
