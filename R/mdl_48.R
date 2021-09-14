#' @title stand (stem), formheight 
#' @return stand (stem), formheight 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), diameter (breast height)
mdl_HF_48 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.47997
		c03 = 1.02196
		c04 = 0.1288
		c05 = -2.8412
		c06 = 6.3796
		c07 = 0.0
		c08 = 0.0
		c01 = 0.41097
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.91213
		c03 = 0.0
		c04 = 0.0
		c05 = 0.0
		c06 = 0.0
		c07 = 0.13122
		c08 = -0.19231
		c01 = 0.34138
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.3<-function(p02, p03){
		c02 = 0.00888
		c03 = 1.1711
		c04 = -1.9487
		c05 = -2.7384
		c06 = 10.0527
		c07 = 0.0
		c08 = 0.0
		c01 = 0.4029
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.4<-function(p02, p03){
		c02 = 0.67
		c03 = 0.0
		c04 = 0.0
		c05 = 0.0
		c06 = 0.0
		c07 = 0.049
		c08 = -0.0859
		c01 = 0.412
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.5<-function(p02, p03){
		c02 = 0.13262
		c03 = 1.08288
		c04 = -2.3733
		c05 = -6.77777
		c06 = 37.4517
		c07 = 0.0
		c08 = 0.0
		c01 = 0.44867
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.6<-function(p02, p03){
		c02 = 0.74393
		c03 = 1.00692
		c04 = -2.04877
		c05 = -2.82135
		c06 = 9.6973
		c07 = 0.0
		c08 = 0.0
		c01 = 0.36559
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.7<-function(p02, p03){
		c02 = 0.47997
		c03 = 1.02196
		c04 = 0.1288
		c05 = -2.8412
		c06 = 6.3796
		c07 = 0.0
		c08 = 0.0
		c01 = 0.41097
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	xmdl.8<-function(p02, p03){
		c02 = 0.32534
		c03 = 0.19431
		c04 = 1.58022
		c05 = 0.77113
		c06 = -3.6713
		c07 = 0.0
		c08 = 0.0
		c01 = 0.42628
		p02 * (((c06 / p03+c05 * p02 / p03+c04+c03 * p02) / p03+c02) / p02+c01+(c08 / p03+c07) * p02 / p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03), 
		NA
	)))))))))
}
