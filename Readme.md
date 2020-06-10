
## Data

``` r
# load german football-league (Bundesliga) data
library("ordBTL")
library("wikibooks")
data(Bundesliga)
str(Bundesliga)
```

    ## 'data.frame':    13406 obs. of  10 variables:
    ##  $ Saison            : Factor w/ 44 levels "1963/1964","1964/1965",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Spieltag          : int  1 1 1 1 1 1 1 1 2 2 ...
    ##  $ Datum             : Date, format: "1963-08-24" "1963-08-24" ...
    ##  $ Anpfiff           : Factor w/ 31 levels "14:00","14:15",..: 13 13 13 13 13 13 13 13 13 13 ...
    ##  $ Heim              : Factor w/ 50 levels "1. FC Kaiserslautern",..: 27 40 16 29 43 33 4 20 15 10 ...
    ##  $ Gast              : Factor w/ 50 levels "1. FC Kaiserslautern",..: 3 10 1 32 15 24 2 47 33 43 ...
    ##  $ Tore.Heim         : int  1 3 1 1 1 1 0 2 1 3 ...
    ##  $ Tore.Gast         : int  1 2 1 4 1 1 2 0 0 3 ...
    ##  $ Tore.Heim.Halbzeit: int  0 1 1 0 1 0 0 2 1 2 ...
    ##  $ Tore.Gast.Halbzeit: int  1 1 1 3 0 0 2 0 0 1 ...

``` r
# add new variable Y3 reflecting the response which is coded as 
# 1 if the home team wins
# 2 if the game ends up with a tie
# 3 if the home team loses
diff = Bundesliga$Tore.Heim - Bundesliga$Tore.Gast
Bundesliga$Y3 = as.ordered(ifelse(diff >= 1, 1, ifelse(diff <= -1, 3, 2)))
buli0506 = subset(Bundesliga, Saison=="2005/2006")
str(buli0506)
```

    ## 'data.frame':    306 obs. of  11 variables:
    ##  $ Saison            : Factor w/ 44 levels "1963/1964","1964/1965",..: 43 43 43 43 43 43 43 43 43 43 ...
    ##  $ Spieltag          : int  1 1 1 1 1 1 1 1 1 2 ...
    ##  $ Datum             : Date, format: "2005-08-05" "2005-08-06" ...
    ##  $ Anpfiff           : Factor w/ 31 levels "14:00","14:15",..: 28 6 6 6 6 6 6 15 15 6 ...
    ##  $ Heim              : Factor w/ 50 levels "1. FC Kaiserslautern",..: 18 2 31 24 49 40 25 20 16 8 ...
    ##  $ Gast              : Factor w/ 50 levels "1. FC Kaiserslautern",..: 11 5 47 3 10 7 27 1 8 18 ...
    ##  $ Tore.Heim         : int  3 1 1 3 2 5 2 2 1 2 ...
    ##  $ Tore.Gast         : int  0 0 1 0 2 2 2 1 4 5 ...
    ##  $ Tore.Heim.Halbzeit: int  1 0 1 2 1 3 0 0 1 1 ...
    ##  $ Tore.Gast.Halbzeit: int  0 0 1 0 0 2 0 1 1 3 ...
    ##  $ Y3                : Ord.factor w/ 3 levels "1"<"2"<"3": 1 1 2 1 2 1 2 1 3 3 ...

## Estimate the team ability

``` r
# Create design matrix for ordinal Bradley-Terry model
des.nohome = design(buli0506, var1="Heim", var2="Gast",
  use.vars="Y3", home.advantage="no", reference="GAMMA.MSV.Duisburg")

mod.nohome = ordBTL(Y3~., data=des.nohome)
# team 'abilities' (should be approximately the ranking of the final standings)
getRank(mod.nohome, prefix="GAMMA", reference="GAMMA.MSV.Duisburg")
```

    ##                                   Estimate Std. Error   z value     Pr(>|z|)
    ## GAMMA.FC.Bayern.Muenchen        2.15183470  0.4894014 4.3968712 1.098225e-05
    ## GAMMA.SV.Werder.Bremen          1.97066756  0.4805982 4.1004475 4.123520e-05
    ## GAMMA.Hamburger.SV              1.85128996  0.4755611 3.8928539 9.907181e-05
    ## GAMMA.FC.Schalke.04             1.53694777  0.4650047 3.3052306 9.489829e-04
    ## GAMMA.Bayer.Leverkusen          1.11218303  0.4562407 2.4377114 1.478057e-02
    ## GAMMA.Hertha.BSC.Berlin         0.96890640  0.4545961 2.1313565 3.305978e-02
    ## GAMMA.Borussia.Dortmund         0.88674951  0.4539380 1.9534594 5.076519e-02
    ## GAMMA.VfB.Stuttgart             0.84283463  0.4536699 1.8578147 6.319534e-02
    ## GAMMA.Borussia.Moenchengladbach 0.73609299  0.4532612 1.6239928 1.043773e-01
    ## GAMMA.1.FC.Nuernberg            0.69608421  0.4531961 1.5359449 1.245519e-01
    ## GAMMA.Hannover.96               0.67862317  0.4531825 1.4974611 1.342733e-01
    ## GAMMA.1.FSV.Mainz.05            0.46470092  0.4537577 1.0241170 3.057800e-01
    ## GAMMA.VfL.Wolfsburg             0.37696046  0.4543918 0.8295935 4.067687e-01
    ## GAMMA.Eintracht.Frankfurt       0.33368265  0.4547928 0.7337027 4.631300e-01
    ## GAMMA.Arminia.Bielefeld         0.29518165  0.4551965 0.6484708 5.166805e-01
    ## GAMMA.1.FC.Kaiserslautern       0.17249245  0.4567996 0.3776108 7.057198e-01
    ## GAMMA.1.FC.Koeln                0.06391278  0.4586289 0.1393562 8.891687e-01
    ## GAMMA.MSV.Duisburg              0.00000000         NA        NA 0.000000e+00

``` r
# for more examples see ?ordBTL
```
