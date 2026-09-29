# Workshop

## Installing required packages

*must have: RStudio with up to date R*

1.  Install the “pacman” package which will facilitate the installation
    of all other required packages:

    ``` r

    #install.packages("pacman")
    ```

2.  Install all the required packages using pacman::p_load() - it should
    skip any packages that you already have installed.

    ``` r

    # pacman::p_load(dplyr,forcats,gdistance,ggspatial,magrittr,memoise,methods,
    #                   nleqslv,purrr,raster,readr,readxl,rlang,sf,sp,stats,stringr,
    #                   tabularaster,tibble,tidyr,utils,ggplot2,gridExtra,sysfonts)
    ```

## How to install seabORD

1.  unzip folder on computer and know the path to it (e.g.,):

    “C:/Users/madtig/Desktop/seabORD_pkg-main”

2.  open R Studio (not in the project of the zip file necessarily; just
    opening R should be enough)

    Run the following code in the console with your defined path:

``` r

path <- file.path("C:","Users","madtig","Desktop","seabORD_pkg-main", fsep = .Platform$file.sep)

#install.packages(pkgs = path, repos = NULL, type = "source")
```

4.  load the package in an R script:

``` r

library("seabORD")
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: forcats
#> Loading required package: gdistance
#> Loading required package: raster
#> Loading required package: sp
#> 
#> Attaching package: 'raster'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
#> Loading required package: igraph
#> 
#> Attaching package: 'igraph'
#> The following object is masked from 'package:raster':
#> 
#>     union
#> The following objects are masked from 'package:dplyr':
#> 
#>     as_data_frame, groups, union
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
#> Loading required package: Matrix
#> 
#> Attaching package: 'gdistance'
#> The following object is masked from 'package:igraph':
#> 
#>     normalize
#> Loading required package: ggspatial
#> Loading required package: magrittr
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:raster':
#> 
#>     extract
#> Loading required package: memoise
#> Loading required package: nleqslv
#> Loading required package: purrr
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names
#> The following objects are masked from 'package:igraph':
#> 
#>     compose, simplify
#> Loading required package: readr
#> Loading required package: readxl
#> Loading required package: rlang
#> 
#> Attaching package: 'rlang'
#> The following objects are masked from 'package:purrr':
#> 
#>     flatten, flatten_chr, flatten_dbl, flatten_int, flatten_lgl,
#>     flatten_raw, invoke, splice
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names
#> The following object is masked from 'package:igraph':
#> 
#>     is_named
#> Loading required package: sf
#> Linking to GEOS 3.14.1, GDAL 3.12.1, PROJ 9.7.1; sf_use_s2() is TRUE
#> Loading required package: stringr
#> Loading required package: tabularaster
#> Loading required package: tibble
#> 
#> Attaching package: 'tibble'
#> The following object is masked from 'package:igraph':
#> 
#>     as_data_frame
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:magrittr':
#> 
#>     extract
#> The following objects are masked from 'package:Matrix':
#> 
#>     expand, pack, unpack
#> The following object is masked from 'package:igraph':
#> 
#>     crossing
#> The following object is masked from 'package:raster':
#> 
#>     extract
#> Loading required package: gridExtra
#> 
#> Attaching package: 'gridExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     combine
```

5.  you can now follow the examples!
