% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iviewMain.R
\name{convertToFormat}
\alias{convertToFormat}
\title{Convert Image to Format}
\usage{
convertToFormat(
  imagepath,
  format,
  outputdir = dirname(imagepath),
  wait = TRUE,
  irfanViewExe = defaultIrfanViewExe()
)
}
\arguments{
\item{imagepath}{full path to image file to be converted}

\item{format}{file extension of target format (without ".", e.g.
"jpg", "bmp", ...)}

\item{outputdir}{output directory where converted image is to be saved. Per
default the file is created in the directory of imagepath}

\item{wait}{passed to \code{shell}}

\item{irfanViewExe}{path to IrfanView Executable}
}
\description{
Convert Image to Format
}
