#' @title stand (stem), number of trees 
#' @return stand (stem), number of trees 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
#' @param p03 stand (stem), height 
mdl_N_105 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 14.23
		c01 = 42.8
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.2<-function(p02, p03){
		c02 = 14.51
		c01 = 56.2
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.3<-function(p02, p03){
		c02 = 14.15
		c01 = 117
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.4<-function(p02, p03){
		c02 = 12.67
		c01 = 106.8
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.5<-function(p02, p03){
		c02 = 14.21
		c01 = 95.5
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.6<-function(p02, p03){
		c02 = 14.85
		c01 = 84.2
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.7<-function(p02, p03){
		c02 = 14.15
		c01 = 117
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	xmdl.8<-function(p02, p03){
		c02 = 14.15
		c01 = 117
		(10000 / (c01+c02 * p02^2 / p03) / 1.34)^2
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('NU','TS','KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('PP','HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03), 
		NA
	)))))))))
}
