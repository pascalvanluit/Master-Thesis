myModel <- ' # regressions
             y1 + y2 ~ f1 + f2 + x1 + x2
                  f1 ~ f2 + f3
                  f2 ~ f3 + x1 + x2
        
             # latent variable definitions
             f1 =~ y1 + y2 + y3
             f2 =~ y4 + y5 + y6 
             f3 =~ y7 + y8 + y9 + y10

             # variances and covariances 
             y1 ~~ y1 
             y1 ~~ y2 
             f1 ~~ f2
            
             # intercepts 
             y1 ~ 1 
             f1 ~ 1
           '

#Holzinger Swineford
HS.model <- ' visual =~ x1 + 0.5*x2 + c(0.6, 0.8)*x3
		          textual =~ x4 + start(c(1.2, 0.6))*x5 + a*x6
		          speed =~ x7 + x8 + x9'

#Political Democracy data
model <- '
  # measurement model
  ind60 =~ x1 + x2 + myLabel*x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

  # regressions
  dem60 ~ ind60
  dem65 ~ ind60 + dem60

  # residual correlations
  y1 ~~ y5
  y2 ~~ y4 + y6
  y3 ~~ y7
  y4 ~~ y8
  y6 ~~ y8
'
