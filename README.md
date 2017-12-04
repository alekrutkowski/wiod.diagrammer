wiod.diagrammer -- R package for an easy work with WIOD (the 2016 release) including diagramming (flowcharts)
================
Aleksander Rutkowski
2017-12-04

Installation
------------

``` r
## if package `devtools` not installed, first do this:
# install.packages('devtools')
devtools::install_github('alekrutkowski/wiod.diagrammer')
```

Example
-------

Load WIOD data for a given year from an official WIOD .Rdata file (release 2016), which has to be first downloaded from <http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip> and extracted (unzipped).

``` r
library(wiod.diagrammer)
```

``` r
W <- loadWIOD('WIOT2014_October16_ROW.RData')
```

    ## Loading "WIOT2014_October16_ROW.RData"...

``` r
# Check the names of the first 10 columns:
cat(head(colnames(W), 20),
    sep='\n')
```

    ## IndustryCode
    ## IndustryDescription
    ## Country
    ## RNr
    ## AUS1
    ## AUS2
    ## AUS3
    ## AUS4
    ## AUS5
    ## AUS6
    ## AUS7
    ## AUS8
    ## AUS9
    ## AUS10
    ## AUS11
    ## AUS12
    ## AUS13
    ## AUS14
    ## AUS15
    ## AUS16

``` r
# How many columns and rows?
message(ncol(W),' columns; ',nrow(W),' rows')
```

    ## 2689 columns; 2472 rows

Now let's flatten (reshape into long format):

``` r
W_flat <- flatWIOD(W)
```

    ## Reshaping into long/flat format...

``` r
# See the structure of W:
str(W_flat)
```

    ## Classes 'data.table' and 'data.frame':   6613376 obs. of  5 variables:
    ##  $ ExpSectorNr: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ ExpCountry : chr  "AUS" "AUS" "AUS" "AUS" ...
    ##  $ value      : num  12924.2 83 19.1 115.9 1590.8 ...
    ##  $ ImpSectorNr: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ImpCountry : chr  "AUS" "AUS" "AUS" "AUS" ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "isFlatWIOD")= logi TRUE

``` r
head(W_flat, 10)
```

    ##     ExpSectorNr ExpCountry       value ImpSectorNr ImpCountry
    ##  1:           1        AUS 12924.17969           1        AUS
    ##  2:           2        AUS    83.02964           1        AUS
    ##  3:           3        AUS    19.14773           1        AUS
    ##  4:           4        AUS   115.92985           1        AUS
    ##  5:           5        AUS  1590.84059           1        AUS
    ##  6:           6        AUS    42.39361           1        AUS
    ##  7:           7        AUS    22.95618           1        AUS
    ##  8:           8        AUS    27.70757           1        AUS
    ##  9:           9        AUS    64.61939           1        AUS
    ## 10:          10        AUS   877.44099           1        AUS

``` r
tail(W_flat, 10)
```

    ##     ExpSectorNr ExpCountry         value ImpSectorNr ImpCountry
    ##  1:          47        ROW  3662.2757637          61        ROW
    ##  2:          48        ROW -9023.1820353          61        ROW
    ##  3:          49        ROW   523.1589139          61        ROW
    ##  4:          50        ROW 13628.6077185          61        ROW
    ##  5:          51        ROW  7020.4946769          61        ROW
    ##  6:          52        ROW 13934.2109353          61        ROW
    ##  7:          53        ROW  2960.8473143          61        ROW
    ##  8:          54        ROW  2171.8532215          61        ROW
    ##  9:          55        ROW   -73.4200616          61        ROW
    ## 10:          56        ROW     0.8743851          61        ROW

`wiod.diagrammer` uses internally the [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) package for performance. You can work with `W_flat` using the `data.table`'s semantcs (e.g. the `:=` in-place column generation/modification) or, if you prefer base R `data.frame`s, you may convert `W_flat` into a regular `data.frame` with the function `as.data.frame`, do the modifications, and then convert it back into `data.table` with the function `data.table::as.data.table` for further processing (through the function `findLinks` described below).

Now let's find the top supplier-user linkages in WIOD (as defined by the column `value` in `W_flat` which -- if not modified -- corresponds to the intermediate consumption in `W`).

First let's introduce a helper function `tieRobustRankLessOrEqual` (available in `wiod.diagrammer`) comparing it with base R `rank`:

``` r
x <- c(1,1,2,2,2,3,3)
# A comparison:
print(data.frame(x,
                 rank(x) <= 2,
                 tieRobustRankLessOrEqual(x, 2),
                 check.names = FALSE))
```

    ##  x   rank(x) <= 2   tieRobustRankLessOrEqual(x, 2)  
    ##  1    TRUE           TRUE                           
    ##  1    TRUE           TRUE                           
    ##  2   FALSE           TRUE                           
    ##  2   FALSE           TRUE                           
    ##  2   FALSE           TRUE                           
    ##  3   FALSE          FALSE                           
    ##  3   FALSE          FALSE

Now, let's find e.g. the 3 main customers of German automotive industry as well as 2 main customers of those customers and 1 main customer of those customer's customers (we could go even deeper if we wanted or use different cut-off ranks, not necessarily in the declining order). So, we have 3 levels of linkages (the 1st one -- direct, the 2nd one and the 3rd one -- indirect). NB: Some industries may re-emerge at different rounds and they may be both customers and suppliers at the same time. Let's also differentiate our ranks (top 3, 2, and 1) by one more dimension: domestic vs foreign linkages.

``` r
W_flat[, domestic :=   # creating column in-place, following data.table's semantics
           ExpCountry == ImpCountry]
# Let's get rid of self-produced intermediate consumption
W_flat_noself <- W_flat[!(domestic &
                              ExpSectorNr == ImpSectorNr)]
# Let's keep only intra-EU trade
COUNTRIES_DT <- countries()
EU_COUNTRIES <-
    COUNTRIES_DT$Country[COUNTRIES_DT$isEUmember]
# Let's keep only flows >= 1 billion USD for clarity
W_flat_noself_truncated <- W_flat_noself[value >= 1000 &  # original WIOD data is in million USD
                                             ExpCountry %in% EU_COUNTRIES &
                                             ImpCountry %in% EU_COUNTRIES]
TOP_CUSTOMERS <-
    findLinks(partners = 'users',
              flat_wiod = W_flat_noself_truncated,
              start_countries = 'DEU', # We could add here other countries.
              start_sectors = 20, # "Manufacture of motor vehicles, trailers and semi-trailers"
                                  # We could add here other sectors.
              ListOfselectionFuns = # As discussed in the text above:
                  list(function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 3),  # minus because we
                       function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 2),  # want to rank from
                       function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 1)), # the highest to the lowest
              by = c('domestic','ExpCountry','ExpSectorNr')) # as discussed above
```

    ## Round 1...
    ## Selecting users (country and sector combinations)
    ## for each combination of: `domestic`,`ExpCountry`,`ExpSectorNr`...
    ## Accumulated 6 unique linkages.
    ## Round 2...
    ## Selecting users (country and sector combinations)
    ## for each combination of: `domestic`,`ExpCountry`,`ExpSectorNr`...
    ## Accumulated 10 unique linkages.
    ## Round 3...
    ## Selecting users (country and sector combinations)
    ## for each combination of: `domestic`,`ExpCountry`,`ExpSectorNr`...
    ## Accumulated 10 unique linkages.

``` r
str(TOP_CUSTOMERS)
```

    ## Classes 'SelectedLinksDT', 'data.table' and 'data.frame':    10 obs. of  6 variables:
    ##  $ ExpSectorNr: int  20 20 20 20 20 20 19 19 19 19
    ##  $ ExpCountry : chr  "DEU" "DEU" "DEU" "DEU" ...
    ##  $ value      : num  4627 9502 14825 6007 60939 ...
    ##  $ ImpSectorNr: int  57 57 57 19 57 60 60 60 20 60
    ##  $ ImpCountry : chr  "ESP" "FRA" "GBR" "DEU" ...
    ##  $ domestic   : logi  FALSE FALSE FALSE TRUE TRUE TRUE ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
head(TOP_CUSTOMERS, 20)
```

    ##     ExpSectorNr ExpCountry     value ImpSectorNr ImpCountry domestic
    ##  1:          20        DEU  4626.951          57        ESP    FALSE
    ##  2:          20        DEU  9501.616          57        FRA    FALSE
    ##  3:          20        DEU 14825.307          57        GBR    FALSE
    ##  4:          20        DEU  6006.584          19        DEU     TRUE
    ##  5:          20        DEU 60939.054          57        DEU     TRUE
    ##  6:          20        DEU 24389.622          60        DEU     TRUE
    ##  7:          19        DEU  4695.542          60        GBR    FALSE
    ##  8:          19        DEU  4691.281          60        ITA    FALSE
    ##  9:          19        DEU  9941.646          20        DEU     TRUE
    ## 10:          19        DEU 37712.819          60        DEU     TRUE

Now let's do a similar exercise, just "upstream" i.e. for suppliers of suppliers.

``` r
TOP_SUPPLIERS <-
    findLinks(partners = 'suppliers',
              flat_wiod = W_flat_noself_truncated,
              start_countries = 'DEU', # We could add here other countries.
              start_sectors = 20, # "Manufacture of motor vehicles, trailers and semi-trailers"
                                  # We could add here other sectors.
              ListOfselectionFuns = # As discussed in the text above:
                  list(function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 3),  # minus because we
                       function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 2),  # want to rank from
                       function(flat_wiod) tieRobustRankLessOrEqual(-flat_wiod$value, 1)), # the highest to the lowest
              by = c('domestic','ImpCountry','ImpSectorNr')) # as discussed above
```

    ## Round 1...
    ## Selecting suppliers (country and sector combinations)
    ## for each combination of: `domestic`,`ImpCountry`,`ImpSectorNr`...
    ## Accumulated 6 unique linkages.
    ## Round 2...
    ## Selecting suppliers (country and sector combinations)
    ## for each combination of: `domestic`,`ImpCountry`,`ImpSectorNr`...
    ## Accumulated 24 unique linkages.
    ## Round 3...
    ## Selecting suppliers (country and sector combinations)
    ## for each combination of: `domestic`,`ImpCountry`,`ImpSectorNr`...
    ## Accumulated 38 unique linkages.

``` r
str(TOP_SUPPLIERS)
```

    ## Classes 'SelectedLinksDT', 'data.table' and 'data.frame':    38 obs. of  6 variables:
    ##  $ ExpSectorNr: int  20 20 20 15 16 28 15 15 24 31 ...
    ##  $ ExpCountry : chr  "CZE" "HUN" "POL" "DEU" ...
    ##  $ value      : num  5912 5165 4258 10347 15148 ...
    ##  $ ImpSectorNr: int  20 20 20 20 20 20 15 15 15 15 ...
    ##  $ ImpCountry : chr  "DEU" "DEU" "DEU" "DEU" ...
    ##  $ domestic   : logi  FALSE FALSE FALSE TRUE TRUE TRUE ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
head(TOP_SUPPLIERS, 20)
```

    ##     ExpSectorNr ExpCountry     value ImpSectorNr ImpCountry domestic
    ##  1:          20        CZE  5912.361          20        DEU    FALSE
    ##  2:          20        HUN  5165.467          20        DEU    FALSE
    ##  3:          20        POL  4258.478          20        DEU    FALSE
    ##  4:          15        DEU 10346.647          20        DEU     TRUE
    ##  5:          16        DEU 15147.662          20        DEU     TRUE
    ##  6:          28        DEU 16858.074          20        DEU     TRUE
    ##  7:          15        FRA  2105.831          15        DEU    FALSE
    ##  8:          15        ITA  2425.400          15        DEU    FALSE
    ##  9:          24        DEU  5240.981          15        DEU     TRUE
    ## 10:          31        DEU  3520.693          15        DEU     TRUE
    ## 11:          15        DEU  9825.688          16        DEU     TRUE
    ## 12:          50        DEU  4845.994          16        DEU     TRUE
    ## 13:          15        ITA  1041.497          16        DEU    FALSE
    ## 14:          13        CZE  1926.013          20        CZE     TRUE
    ## 15:          28        CZE  1525.946          20        CZE     TRUE
    ## 16:          20        DEU  3192.376          20        CZE    FALSE
    ## 17:          20        POL  1082.968          20        CZE    FALSE
    ## 18:          19        DEU  2446.414          20        HUN    FALSE
    ## 19:          20        DEU  3146.213          20        HUN    FALSE
    ## 20:          20        DEU  3111.205          20        POL    FALSE

Now, let's plot the "upstream" linkages, making all the German sectors blue. By default, the cross-border flows are dashed, while the domestic flows are solid lines (arrows). The numbers in the nodes (rectangles) represent, by default, the sector output (or the total intermediate consumption for the final use sectors such as final consumption or investment if they show up in the customers' graphs).

In `wiod.diagrammer` the rendering of the plot is done internally by [`DiagrammeR::grViz`](https://www.rdocumentation.org/packages/DiagrammeR/topics/grViz). The plots can be saved manually in RStudio, or programmatically e.g. to an .svg file via [`DiagrammeRsvg::export_svg`](https://www.rdocumentation.org/packages/DiagrammeRsvg/topics/export_svg) to a character vector and then [`cat`ed](https://www.rdocumentation.org/packages/base/topics/cat), or to a .png file piping them through [`DiagrammeRsvg::export_svg`](https://www.rdocumentation.org/packages/DiagrammeRsvg/topics/export_svg), [`charToRaw`](https://www.rdocumentation.org/packages/base/topics/charToRaw) and [`rsvg::rsvg_png`](https://www.rdocumentation.org/packages/rsvg/topics/rsvg_png).

``` r
plot(top_links_dt = TOP_SUPPLIERS,
     wiot = W, # this is necessary
     specificNodeOptionsFun =  # this is optional, just to show-off:
         function(country_sector_dt)
             ifelse(country_sector_dt$Country=='DEU',
                    'style=filled, fillcolor=cadetblue1', "")) # GraphViz colour names can be found at:
                                                               # http://www.graphviz.org/doc/info/colors.html
```

![Graph](https://cdn.rawgit.com/alekrutkowski/wiod.diagrammer/master/Graph.svg)

What else is available in the package? The functions which produce auxiliary `data.table`s (that are used by `wiod.diagrammer`s `plot` function if evaluated with default argument values).

``` r
COUNTRIES <- countries() # NB: no argument to function `countries`
str(COUNTRIES)
```

    ## Classes 'data.table' and 'data.frame':   45 obs. of  3 variables:
    ##  $ CountryLab: chr  "Australia" "Austria" "Belgium" "Bulgaria" ...
    ##  $ Country   : chr  "AUS" "AUT" "BEL" "BGR" ...
    ##  $ isEUmember: logi  FALSE TRUE TRUE TRUE FALSE FALSE ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
print(COUNTRIES)
```

    ##  CountryLab         Country   isEUmember  
    ##  Australia          AUS       FALSE       
    ##  Austria            AUT        TRUE       
    ##  Belgium            BEL        TRUE       
    ##  Bulgaria           BGR        TRUE       
    ##  Brazil             BRA       FALSE       
    ##  Canada             CAN       FALSE       
    ##  Switzerland        CHE       FALSE       
    ##  China              CHN       FALSE       
    ##  Cyprus             CYP        TRUE       
    ##  Czech Republic     CZE        TRUE       
    ##  Germany            DEU        TRUE       
    ##  Denmark            DNK        TRUE       
    ##  Spain              ESP        TRUE       
    ##  Estonia            EST        TRUE       
    ##  Finland            FIN        TRUE       
    ##  France             FRA        TRUE       
    ##  United Kingdom     GBR        TRUE       
    ##  Greece             GRC        TRUE       
    ##  Croatia            HRV        TRUE       
    ##  Hungary            HUN        TRUE       
    ##  Indonesia          IDN       FALSE       
    ##  India              IND       FALSE       
    ##  Ireland            IRL        TRUE       
    ##  Italy              ITA        TRUE       
    ##  Japan              JPN       FALSE       
    ##  Korea              KOR       FALSE       
    ##  Lithuania          LTU        TRUE       
    ##  Luxembourg         LUX        TRUE       
    ##  Latvia             LVA        TRUE       
    ##  Mexico             MEX       FALSE       
    ##  Malta              MLT        TRUE       
    ##  Netherlands        NLD        TRUE       
    ##  Norway             NOR       FALSE       
    ##  Poland             POL        TRUE       
    ##  Portugal           PRT        TRUE       
    ##  Romania            ROU        TRUE       
    ##  Rest of the World  ROW       FALSE       
    ##  Russian Federation RUS       FALSE       
    ##  Slovak Republic    SVK        TRUE       
    ##  Slovenia           SVN        TRUE       
    ##  Sweden             SWE        TRUE       
    ##  TOTAL              TOT       FALSE       
    ##  Turkey             TUR       FALSE       
    ##  Taiwan             TWN       FALSE       
    ##  United States      USA       FALSE

``` r
SECTORS <- sectors(W)
SECTORS[, SectorLab :=  # truncate the long sector labels just for clarity below
            substr(SectorLab, 1, 10)]
str(SECTORS)
```

    ## Classes 'data.table' and 'data.frame':   61 obs. of  4 variables:
    ##  $ SectorNr  : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ SectorLab : chr  "Crop and a" "Forestry a" "Fishing an" "Mining and" ...
    ##  $ SectorCode: chr  "A01" "A02" "A03" "B" ...
    ##  $ isFinal   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
head(SECTORS, 20)
```

    ##     SectorNr  SectorLab SectorCode isFinal
    ##  1:        1 Crop and a        A01   FALSE
    ##  2:        2 Forestry a        A02   FALSE
    ##  3:        3 Fishing an        A03   FALSE
    ##  4:        4 Mining and          B   FALSE
    ##  5:        5 Manufactur    C10-C12   FALSE
    ##  6:        6 Manufactur    C13-C15   FALSE
    ##  7:        7 Manufactur        C16   FALSE
    ##  8:        8 Manufactur        C17   FALSE
    ##  9:        9 Printing a        C18   FALSE
    ## 10:       10 Manufactur        C19   FALSE
    ## 11:       11 Manufactur        C20   FALSE
    ## 12:       12 Manufactur        C21   FALSE
    ## 13:       13 Manufactur        C22   FALSE
    ## 14:       14 Manufactur        C23   FALSE
    ## 15:       15 Manufactur        C24   FALSE
    ## 16:       16 Manufactur        C25   FALSE
    ## 17:       17 Manufactur        C26   FALSE
    ## 18:       18 Manufactur        C27   FALSE
    ## 19:       19 Manufactur        C28   FALSE
    ## 20:       20 Manufactur        C29   FALSE

``` r
AGGREGATES <- aggregates(W)
```

The variable/column names produced by the function `aggregates` reflect those in WIOD. They are explained in the documentations of `aggregates`.

``` r
str(AGGREGATES) 
```

    ## Classes 'data.table' and 'data.frame':   2684 obs. of  10 variables:
    ##  $ SectorNr: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Country : chr  "AUS" "AUS" "AUS" "AUS" ...
    ##  $ II_fob  : num  39039 925 1205 72426 58385 ...
    ##  $ TXSP    : num  501.9 68.1 51.7 297.6 631.8 ...
    ##  $ EXP_adj : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PURR    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PURNR   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ VA      : num  30489 1570 1895 98315 24247 ...
    ##  $ IntTTM  : num  261.7 22.2 23.9 946.5 240.3 ...
    ##  $ GO      : num  70292 2585 3175 171985 83504 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
head(AGGREGATES, 20)
```

    ##     SectorNr Country     II_fob       TXSP EXP_adj PURR PURNR        VA
    ##  1:        1     AUS 39039.2389 501.926754       0    0     0 30489.190
    ##  2:        2     AUS   925.1227  68.141295       0    0     0  1569.899
    ##  3:        3     AUS  1204.7060  51.732099       0    0     0  1894.738
    ##  4:        4     AUS 72425.8720 297.643163       0    0     0 98315.120
    ##  5:        5     AUS 58384.5017 631.807491       0    0     0 24247.396
    ##  6:        6     AUS  2605.2096  66.295431       0    0     0  2335.771
    ##  7:        7     AUS  5553.2449  36.009904       0    0     0  3382.998
    ##  8:        8     AUS  5385.5547   8.013658       0    0     0  2463.259
    ##  9:        9     AUS  4671.9083  16.344716       0    0     0  3042.421
    ## 10:       10     AUS 19239.5228 583.397244       0    0     0  3751.804
    ## 11:       11     AUS 10468.8475 120.183333       0    0     0  5333.571
    ## 12:       12     AUS  6321.7307   9.907760       0    0     0  3360.232
    ## 13:       13     AUS  6324.4477  35.313103       0    0     0  4251.740
    ## 14:       14     AUS  9868.0654  62.421243       0    0     0  5473.808
    ## 15:       15     AUS 37334.7513 269.529392       0    0     0  4766.247
    ## 16:       16     AUS 15617.1068  97.862928       0    0     0 10112.567
    ## 17:       17     AUS  1926.6851   4.258397       0    0     0  4016.797
    ## 18:       18     AUS  3637.6790  12.062149       0    0     0  2265.652
    ## 19:       19     AUS  8586.6225  30.389956       0    0     0  4993.905
    ## 20:       20     AUS 10571.1618  70.617707       0    0     0  3167.178
    ##         IntTTM         GO
    ##  1:  261.67872  70292.034
    ##  2:   22.21674   2585.380
    ##  3:   23.86781   3175.044
    ##  4:  946.48747 171985.122
    ##  5:  240.33129  83504.037
    ##  6:   56.74823   5064.024
    ##  7:   27.52288   8999.775
    ##  8:   68.40201   7925.230
    ##  9:   53.40740   7784.082
    ## 10:  880.29705  24455.020
    ## 11:  238.38261  16160.985
    ## 12:   92.86966   9784.740
    ## 13:  173.11259  10784.614
    ## 14:  133.81736  15538.112
    ## 15: 1214.13677  43584.665
    ## 16:  274.81625  26102.353
    ## 17:   85.19562   6032.936
    ## 18:   98.41985   6013.813
    ## 19:  210.65336  13821.571
    ## 20:  278.51840  14087.476
