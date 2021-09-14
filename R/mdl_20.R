#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_D_20 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = 0.826
		c01 = -1.231
		c01+c02 * p02
	}
	xmdl.2<-function(p02){
		c02 = 0.764
		c01 = -0.074
		c01+c02 * p02
	}
	xmdl.3<-function(p02){
		c02 = 0.758
		c01 = -0.249
		c01+c02 * p02
	}
	xmdl.4<-function(p02){
		c02 = 0.856
		c01 = -0.674
		c01+c02 * p02
	}
	xmdl.5<-function(p02){
		c02 = 0.809
		c01 = -0.318
		c01+c02 * p02
	}
	xmdl.6<-function(p02){
		c02 = 0.784
		c01 =  0.107
		c01+c02 * p02
	}
	xmdl.7<-function(p02){
		c02 = 0.79
		c01 = -0.82
		c01+c02 * p02
	}
	xmdl.8<-function(p02){
		c02 = 0.774
		c01 =  0.075
		c01+c02 * p02
	}
	xmdl.12<-function(p02){
		c02 = 0.816
		c01 = -0.601
		c01+c02 * p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02), 
		ifelse(p01 %in% c('RE'), xmdl.12(p02), 
		NA
		))))))))))
}
