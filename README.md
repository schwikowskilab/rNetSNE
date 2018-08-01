# RnetSNE
R wrapper for the net-SNE algorithm from Cho et al. (Cell System 2018).


### Installation

To work this package require the installation of net-SNE C code as described in https://github.com/hhcho/netsne

Once it is done you can install the latest version of the package from the github repository :

``` r
if(!require(devtools)){
  install.packages("devtools") # If not already installed
}
devtools::install_github("schwikowskilab/rNetSNE")
```
