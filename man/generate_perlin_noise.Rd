% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranlan.R
\name{generate_perlin_noise}
\alias{generate_perlin_noise}
\title{A perlin noise based potential space map generator}
\usage{
generate_perlin_noise(
  width,
  height,
  cellSize,
  frequency,
  octaves,
  lacunarity,
  categorized,
  cat_method,
  lim = 0,
  percetange = 0
)
}
\arguments{
\item{width}{width of output raster}

\item{height}{height of output raster}

\item{cellSize}{cellsize of output raster}

\item{frequency}{perlin frequency}

\item{octaves}{perlin octave}

\item{lacunarity}{parlin lacunarity}

\item{categorized}{TRUE/FLASE tells you if the slope raster returns categorized or smooth}

\item{cat_method}{either by slope_lim or land_percentage}

\item{lim}{the slope degree cutoff for portential space where 1 will be potential space and 2 non potential space}

\item{percetange}{}
}
\description{
A perlin noise based potential space map generator
}
\examples{
test<-generate_perlin_noise(200,200,1,2,3,0.01,TRUE, "land_percentage", percetange = 75)
raster::plot(test)





}
