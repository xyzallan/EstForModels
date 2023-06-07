#' @title single tree (stem), volume 
#' @return single tree (stem), volume 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), diameter (6 meters)
#' @param p04 single tree (stem), height 
#' @export
#' @rdname mdl_V_78
mdl_V_78 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.0145543
		c03 = 0.0478628
		c04 = 0.334101
		c05 = 0.0973148
		c06 = 0.0440716
		c01 = 0.268621
		(c01 * p02^2-c02 * p02^2 * p04-(c03 / 1000) * p02^3 * p04+(c04 / 1000) * (p02 * p04)^2+c05 * (p02^2+p02 * p03+p03^2)+c06 * p03^2 * (p04-6)) / 1000
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.0149567
		c03 = 0.114406
		c04 = 0.436781
		c05 = 0.133947
		c06 = 0.0374599
		c01 = 0.208043
		(c01 * p02^2-c02 * p02^2 * p04-(c03 / 1000) * p02^3 * p04+(c04 / 1000) * (p02 * p04)^2+c05 * (p02^2+p02 * p03+p03^2)+c06 * p03^2 * (p04-6)) / 1000
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 0.0104691
		c03 = 0.122258
		c04 = 0.438033
		c05 = 0.099162
		c06 = 0.0334836
		c01 = 0.226547
		(c01 * p02^2-c02 * p02^2 * p04-(c03 / 1000) * p02^3 * p04+(c04 / 1000) * (p02 * p04)^2+c05 * (p02^2+p02 * p03+p03^2)+c06 * p03^2 * (p04-6)) / 1000
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
