#' Generate hedonic model residuals
#' @param dat data.frame
#' @param model formula
#' @return data.frame
#' @export
genResiduals <- function(dat, model) {
  fit <- stats::lm(data = dat, model)
  resids <- stats::resid(fit)
  dat$err <- resids
  dat <- dat %>% dplyr::select(err)
  return(dat)
}

#' Fit KNN/IDW model and interpolate to raster surface
#' @param dtm SpatVector
#' @param model formula
#' @param raster SpatRaster
#' @param mask_layer SpatVector
#' @param nmax integer
#' @param idp integer
#' @return SpatRaster
#' @export
nnRaster <- function(dtm, model, raster, mask_layer, nmax = 5, idp = 0) {
  d <- data.frame(terra::geom(dtm)[,c("x", "y")], terra::as.data.frame(dtm))
  gs <- gstat::gstat(formula=model, locations=~x+y, data=d, nmax=nmax, set=list(idp = idp))
  interp <- terra::interpolate(raster, gs, debug.level=0)
  nnmsk <- terra::mask(interp, mask_layer)
  names(nnmsk) <- c('err', 'na')
  return(nnmsk)
}

#' Get delta surface
#' @param b SpatVector
#' @param a SpatVector
#' @param model formula
#' @param raster SpatRaster
#' @param mask_layer SpatVector
#' @param nmax integer
#' @param idp integer
#' @return SpatRaster
#' @export
nnGetDelta <- function(b, a, model = stats::as.formula(err~1), raster, mask_layer, nmax, idp) {
  delta <- nnRaster(a, model = model, raster = r, mask_layer = chc_mask, nmax = nmax, idp = idp) - nnRaster(b, model = model, raster = r, mask_layer = chc_mask, nmax = nmax, idp = idp)
  names(delta) <- c('err', 'na')
  return(delta)
}

#' Get data-support-measure raster surface
#' @param dat SpatVector
#' @param r SpatRaster
#' @param chc_mask SpatVector
#' @return SpatRaster
#' @export
dataSupportRaster <- function(dat, r, chc_mask) {
  support <- terra::rasterize(dat, r, fun = function(i){length(i)})
  support <- terra::mask(support, chc_mask)
  names(support) <- c('dens')
  return(support)
}

