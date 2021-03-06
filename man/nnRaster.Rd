% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{nnRaster}
\alias{nnRaster}
\title{Fit KNN/IDW model and interpolate to raster surface}
\usage{
nnRaster(dtm, model, raster, mask_layer, nmax = 5, idp = 0)
}
\arguments{
\item{dtm}{SpatVector}

\item{model}{formula}

\item{raster}{SpatRaster}

\item{mask_layer}{SpatVector}

\item{nmax}{integer}

\item{idp}{integer}
}
\value{
SpatRaster
}
\description{
Fit KNN/IDW model and interpolate to raster surface
}
