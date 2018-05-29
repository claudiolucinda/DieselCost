Petro_Losses <- function(.Big_T,.Nr_Period,.Coefs_YC,.Vol_dia,.desconto,.S0) {
  
  u<-exp(.Vol_dia*sqrt(.Big_T/.Nr_Period))
  d<-1/u
  
  r<-Yield_Curve(.beta=.Coefs_YC[1:4],.lambda=.Coefs_YC[5:6],.t=1)
  q<-(exp(r*(.Big_T/(252*10)))-d)/(u-d)
  
  vertices<-seq(from=1, to=.Big_T, length.out = .Nr_Period+1)
  intervals<-diff(vertices)
  YC<-sapply(vertices, function(.x) Yield_Curve(.beta=.Coefs_YC[1:4],.lambda=.Coefs_YC[5:6],.t=.x) )
  
  b0<-.01
  Tester<-BDT_Calibration(.b=b0,.short_mkt=YC)
  
  SR_Lattice<-Tester[[3]]
  
  ending_Share<-function(u_n,d_n,S0,n) {
    out<-sapply(0:n,function(.x) (S0*(u_n^.x)*(d_n^(n-.x))))
  }
  
  ending_Values<-ending_Share(u,d,.S0,.Nr_Period)
  
  Tot_probs<-dbinom(0:.Nr_Period,.Nr_Period,q)
  
  Strike<-.S0-.desconto
  
  temp1<-(rep.int(Strike,.Nr_Period+1)-ending_Values)*exp(SR_Lattice[[.Nr_Period+1]]*(.Big_T/(252*.Nr_Period)))
  OUT<-sum(temp1*(Tot_probs))
  
  
  
}