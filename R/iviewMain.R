# appendImages -----------------------------------------------------------------

#' Append Images Horizontally or Vertically
#' 
#' @param files paths to image files to be appended
#' @param direction 1 = horizontally, 2 = vertically
#' @param file name of output file
#' @param run logical. If \code{TRUE} (default) the append action is performed,
#'   otherwise the command line doing the append action is returned
#' @param irfanViewExe path to IrfanView Executable file
#' 
#' @export
#' 
appendImages <- function(
  files, direction, file, run = TRUE, irfanViewExe = defaultIrfanViewExe()
)
{
  command <- paste(
    kwb.utils::windowsPath(irfanViewExe),
    optionStringPanorama(direction, files), 
    optionStringConvert(file)
  )
  
  if (run) {
    
    system(command)
    
  } else {
    
    command
  }
}

# convertToFormat --------------------------------------------------------------

#' Convert Image to Format
#' 
#' @param imagepath full path to image file to be converted
#' @param format file extension of target format (without ".", e.g.
#'   "jpg", "bmp", ...)
#' @param outputdir output directory where converted image is to be saved. Per
#'   default the file is created in the directory of imagepath
#' @param wait passed to \code{shell}
#' @param irfanViewExe path to IrfanView Executable
#' 
#' @export
#' 
convertToFormat <- function(
  imagepath, format, outputdir = dirname(imagepath), wait = TRUE,
  irfanViewExe = defaultIrfanViewExe()  
)
{
  filename <- gsub("\\.[^.]+$", paste0(".", format), basename(imagepath))
  
  targetpath <- file.path(outputdir, filename)
    
  # Assemble command line
  cmd <- paste(
    kwb.utils::cmdLinePath(irfanViewExe), 
    kwb.utils::cmdLinePath(imagepath), 
    optionStringConvert(targetpath)
  )
  
  # Run shell command and wait for it to return
  shell(
    kwb.utils::hsQuoteChr(cmd, qchar = "\"", escapeMethod = "none"), 
    wait = wait
  )
  
  targetpath
}

# cropImageAndSetBorders -------------------------------------------------------

#' Use IrfanView to Crop the Image and Set Borders
#' 
#' @param inputdir full input path including file name or pattern, e.g. "*.png",
#'   "*.gif"
#' @param outputdir full output path including file name or pattern, e.g.
#'   "*.pgm"
#' @param irfanViewExe path to IrfanView Executable File
#' @param crop vector of four integers (left, top, width, height): optional 
#'   coordinates for cropping. If crop["left"] != -1 the image will be cropped, 
#'   otherwise not.
#' @param border vector of four integers (left, right, top, bottom)
#' @param canvasColor passed to \code{kwb.iview:::writeIniFile}
#' @param useIni logical. If \code{TRUE} (default) an INI file is written
#'   and IrfanView is called with the \code{/ini} option on the command line
#' @param wait passed to \code{shell}. 
#' @param inputPattern file name pattern matching the files in \code{inputdir}
#'   to be processed
#' 
#' @export
#' 
cropImageAndSetBorders <- function(
  inputdir, outputdir, irfanViewExe = defaultIrfanViewExe(), 
  crop = c(left = 0, top = 0, width = -1, height = -1),
  border = c(left = 0, right = 0, top = 0, bottom = 0), 
  canvasColor = 255, useIni = TRUE, wait = FALSE, inputPattern = "*.gif"
)
{
  #useIni = TRUE; inputPattern = "*.gif"
  
  if (any(border > 0) && ! useIni) {
    
    stop("You can modify the image borders only with useIni = TRUE")
  }
  
  # If INI file is to be used, create it (in the output directory)  
  cropOption <- if (useIni) {
    
    writeIniFile(
      outputdir, crop, border, canvasColor, 
      filename = toIniFilename(irfanViewExe)
    )
    
  } else {    
    
    optionStringCrop(crop)
  }
  
  # Assemble command line
  cmd <- paste(
    kwb.utils::cmdLinePath(irfanViewExe), 
    kwb.utils::cmdLinePath(file.path(inputdir, inputPattern)), 
    cropOption, 
    #"/bpp=1",
    optionStringConvert(outputdir)
  )
  
  # Run shell command and wait for it to return
  shell(wait = wait, kwb.utils::hsQuoteChr(
    cmd, qchar = "\"", escapeMethod = "none"
  ))
}

# toIniFilename ----------------------------------------------------------------
toIniFilename <- function(irfanViewExe)
{
  if (grepl("64", basename(irfanViewExe))) {
    
    "i_view64.ini"
    
  } else {
    
    "i_view32.ini"
  } 
}

# defaultIrfanViewExe ----------------------------------------------------------

#' Default Path to the IrfanView Executable File
#' 
#' @export
#' 
defaultIrfanViewExe <- function ()
{
  programdirs <- c("C:/Programme", "C:/Program Files (x86)")
  
  paths <- file.path(programdirs, "IrfanView/i_view32.exe")
  
  paths[file.exists(paths)]
}

# writeIniFile -----------------------------------------------------------------

#' Write INI File and Return Option String
#' 
#' @param outputdir output directory
#' @param crop passed to \code{kwb.iview:::iniContentCrop}
#' @param border passed to \code{kwb.iview:::iniContentCrop}
#' @param canvasColor passed to \code{kwb.iview:::iniContentCrop}
#' @param filename name of INI file
#' 
#' @return option string required to tell IrfanView to use the INI file
#' 
writeIniFile <- function(
  outputdir, crop, border, canvasColor, filename = "i_view32.ini"
)
{
  inidir <- dirname(outputdir)    
  
  fileContent <- iniContentCrop(crop, border, canvasColor)
  
  writeLines(fileContent, file.path(inidir, filename))
  
  optionStringIniFile(inidir)  
}

# iniContentCrop ---------------------------------------------------------------

#' Content for INI File Specifying Crop Action
#' 
#' @param crop integer vector with elements named \code{left}, \code{top},
#'   \code{width}, \code{height}
#' @param border integer vector with elements named \code{left}, \code{right},
#'   \code{top}, \code{bottom}
#' @param canvasColor value used in IrvanView \code{[Effects]} option 
#'   \code{CanvColor}
#' 
iniContentCrop <- function(
  crop = c(left = 0, top = 0, width = -1, height = -1), 
  border = c(left = 0, right = 0, top = 0, bottom = 0), 
  canvasColor = 255
) 
{
  iniLines <- c(
    "[Batch]",
    iniFileAssignment("AdvCrop", ifelse(any(crop > 0), "1", "0")),
    iniFileAssignment("AdvCropX", crop["left"]),
    iniFileAssignment("AdvCropY", crop["top"]),
    iniFileAssignment("AdvCropW", crop["width"]),
    iniFileAssignment("AdvCropH", crop["height"]),
    iniFileAssignment("AdvCropC", 0),
    iniFileAssignment("AdvCanvas", ifelse(any(border > 0), "1", "0")),
    "[Effects]",
    iniFileAssignment("CanvL", border["left"]),
    iniFileAssignment("CanvR", border["right"]),
    iniFileAssignment("CanvT", border["top"]),
    iniFileAssignment("CanvB", border["bottom"]),
    iniFileAssignment("CanvInside", 0),
    iniFileAssignment("CanvColor", canvasColor)
  )
  
  paste(iniLines, collapse = "\n")
}

# iniFileAssignment ------------------------------------------------------------

#' Assignement to be Used in INI File
#' 
#' @param optname name of option
#' @param optvalue value of option
#' 
iniFileAssignment <- function(optname, optvalue)
{
  paste(optname, optvalue, sep = "=")
}

# optionStringIniFile ----------------------------------------------------------

optionStringIniFile <- function(inidir)
{
  sprintf("/advancedbatch /ini=%s", kwb.utils::cmdLinePath(inidir))
}

# optionStringCrop -------------------------------------------------------------

optionStringCrop <- function(crop)
{
  sprintf(
    "/crop=(%d,%d,%d,%d)", 
    crop["left"], crop["top"], crop["width"], crop["height"]
  )
}

# optionStringConvert ----------------------------------------------------------

optionStringConvert <- function(outputdir)
{
  sprintf("/convert=%s", kwb.utils::cmdLinePath(kwb.utils::windowsPath(outputdir)))
}

# optionStringPanorama ---------------------------------------------------------

optionStringPanorama <- function(direction, inputFiles)
{
  sprintf(
    "/panorama=(%d,%s)", 
    direction, 
    kwb.utils::commaCollapsed(kwb.utils::windowsPath(inputFiles))
  )
}
