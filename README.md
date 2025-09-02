
# Outsourcing and time-related dimensions of job quality: Evidence from the Facility Management business

This is the repository for the scripts used in to produce Godino, A. &
Junquera, Á. F. (2025). Outsourcing and time-related dimensions of job
quality: Evidence from the Facility Management business. In F. P. García
Márquez, B. Lev, & H. Liao (Eds.), *Outsourcing. Cases and Studies in
Using Operations Research and Management Science Methods* (pp. 21-41).
Springer. <https://doi.org/10.1007/978-3-031-95393-4>.

The scripts are in the `src` folder and the microdata used for the
analyses can be downloaded from the webpage of the Instituto Nacional de
Estadística ([INE](https://www.ine.es/prodyser/microdatos.htm)).

To ensure reproducibility, we use the `renv` package. Just place all the
files of this repository in your R working directory and run the
following chunk of code. It will install the versions of the packages
that we used to obtain our results.

``` r
# Install {renv} if not yet installed
install.packages("renv")
renv::restore()
```
