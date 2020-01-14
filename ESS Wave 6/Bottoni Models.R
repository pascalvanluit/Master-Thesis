ModelEFA <- "Interpersonal_Trust =~ ppltrst + pplfair + pplhlp
          Density_of_Social_Relations =~ sclmeet + inprdsc + sclact
          Social_Support =~ pplahlp + flapppl + rehlppl
          Participation =~ contplt + wrkprty + wrkorg + badge + sgnptit + pbldmn + bctprd + wkvlorg
          Openness =~ imbgeco + imueclt + imwbcnt + imdfetn
          Institutional_Trust =~ trstprl + trstlgl + trstplc + trstplt + trstprt + trstep + trstun
          Legitimacy_of_Institutions =~ stfgov + stfdem + stfedu + stfhlth"

ModelCFA <- "Interpersonal_Trust =~ ppltrst + pplfair + pplhlp
            Density_of_Social_Relations =~ sclmeet + inprdsc + sclact
            Social_Support =~ pplahlp + flapppl + rehlppl
            Participation =~ wrkprty + wrkorg + sgnptit + wkvlorg
            Openness =~ imbgeco + imueclt + imwbcnt
            Institutional_Trust =~ trstprl + trstlgl + trstplt + trstprt
            Legitimacy_of_Institutions =~ stfgov + stfdem + stfedu + stfhlth"

ModelHigh <- "# Measurement model:
                Interpersonal_Trust =~ ppltrst + pplfair + pplhlp
                Density_of_Social_Relations =~ sclmeet + inprdsc + sclact
                Social_Support =~ pplahlp + flapppl + rehlppl
                Participation =~ wrkprty + wrkorg + sgnptit + wkvlorg
                Openness =~ imbgeco + imueclt + imwbcnt
                Institutional_Trust =~ trstprl + trstlgl + trstplt + trstprt
                Legitimacy_of_Institutions =~ stfgov + stfdem + stfedu + stfhlth
              # Regressions
                micro =~ Interpersonal_Trust + Density_of_Social_Relations + Social_Support
                meso =~ Participation + Openness
                macro =~ Institutional_Trust + Legitimacy_of_Institutions
                cohesion =~ micro + meso + macro
              # Correlations
                sclmeet ~~ sclact
                flapppl ~~ rehlppl
                stfedu ~~ stfhlth"
