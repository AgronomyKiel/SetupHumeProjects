library(lubridate)


### constants ############################
ModelDrive <- "Q:"
OutputDrive <- "P:"


ParIniDir <- "ParIni"
#ScenParIniDir <- "/ini/ScenParIni/"
StateIniDir <- "StateIni"
#ScenStateIniDir <- "/ini/ScenStateIni/"
OptIniDir <- "OptIni"
IniFileDir <- "IniFiles"
#Scen_IniFileDir <- "/ini/ScenIniFiles/"
CtrlFileDir <- "CtrlFiles"
#ScenCtrlFileDir <- "/ini/ScenCtrlFiles/"

### functions ############################



#' function for creating directories
#'
#' @param IniDir
#'
#' @returns
#' @export
#'
CreateIniDirs <- function(IniDir){
  ParIniDir <- paste(IniDir, ParIniDir, sep = "/")
  dir.create(path = ParIniDir, showWarnings = TRUE, recursive = TRUE)
  StateIniDir <- paste(IniDir, StateIniDir, sep = "/")
  dir.create(StateIniDir, showWarnings = FALSE, recursive = TRUE)
  OptIniDir <- paste(IniDir, OptIniDir, sep = "/")
  dir.create(OptIniDir, showWarnings = FALSE, recursive = TRUE)
  CtrlFileDir <- paste(IniDir, CtrlFileDir, sep = "/")
  dir.create(CtrlFileDir, showWarnings = FALSE, recursive = TRUE)
  IniFileDir <- paste(IniDir, IniFileDir, sep = "/")
  dir.create(IniFileDir, showWarnings = FALSE, recursive = TRUE)
  return(list(ParIniDir = ParIniDir, StateIniDir = StateIniDir,
              OptIniDir = OptIniDir, CtrlFileDir = CtrlFileDir,
              IniFileDir = IniFileDir))
}



#' Setup of directories for a simulation project
#'
#' @param ProjectName Name of the project [string] eg. "MaizeEvaluation"
#' @param Trial Name of the trial [string] if Trial is not empty, a subdirectory with the name of the trial will be created in the data and ini directories
#' @param ModelDrive Drive letter for the model [string]
#' @param OutputDrive Drive letter for the output [string]
#'
#' @returns list with directories for the project
#' @export
#'
#' @examples Setupirs("TestProject", "Trial1")
SetupDirs <- function(ProjectName, TrialName="", ModelDrive="Q:", OutputDrive="P:"){

  # create the main directory for the simulation project

  mainIniDir <- paste(ModelDrive, ProjectName,  "ini", TrialName, sep = "/")
  dir.create(mainIniDir, showWarnings = FALSE)

  mainDataDir <- paste(ModelDrive, ProjectName,  "data",  TrialName, sep = "/")

  dir.create(mainDataDir, showWarnings = FALSE, recursive = TRUE)

  NewIniDirsName <-  mainIniDir#paste(mainIniDir, "IniFiles", sep = "/")

  NewIniDirs <- CreateIniDirs(NewIniDirsName)
  ParIniDir <- NewIniDirs$ParIniDir
  StateIniDir <- NewIniDirs$StateIniDir
  OptIniDir <- NewIniDirs$OptIniDir
  CtrlFileDir <- NewIniDirs$CtrlFileDir
  IniFileDir <- NewIniDirs$IniFileDir
  OutFileDir <- paste(OutputDrive, ProjectName, TrialName, sep = "/")
  dir.create(OutFileDir, showWarnings = FALSE)
  return(list(ParIniDir = ParIniDir, StateIniDir = StateIniDir,
              OptIniDir = OptIniDir, CtrlFileDir = CtrlFileDir,
              IniFileDir = IniFileDir, OutFileDir = OutFileDir,
              DataDir = mainDataDir))

}





#' create time arrays for simulations
#'
#' @param FirstHarvestYear First harvest year of the simulation [integer]
#' @param LastHarvestYear Last harvest year of the simulation [integer]
#' @param Wintercrop logical, is the crop a winter crop? [logical]
#' @param SimStart start date of the simulation "YYYY-MM-DD"
#' @param SimEnd end date of the simulation "YYYY-MM-DD"
#' @param Sowingdate sowing date of the crop "YYYY-MM-DD"
#' @param HarvestDate harvest date of the crop "YYYY-MM-DD"
#'
#' @returns list with time arrays for simulations in Excel year format
#' @export
SetTimeArrays <- function(FirstHarvestYear,
                          LastHarvestYear,
                          Wintercrop=TRUE,
                          SimStart,
                          SimEnd,
                          Sowingdate,
                          HarvestDate){

SimStart <- as.Date(SimStart)
SimEnd <- as.Date(SimEnd)
Sowingdate <- as.Date(Sowingdate)
HarvestDate <- as.Date(HarvestDate)

Wintercrops <- rep(Wintercrop, length(FirstHarvestYear:LastHarvestYear))


SowingDateYear <- ifelse (Wintercrop, FirstHarvestYear-1, FirstHarvestYear)
SowingDateMonth <- month(Sowingdate)
SowingDateDay <- day(Sowingdate)
SowingDate <- as.Date(paste0(as.character(SowingDateYear),"-",as.character(SowingDateMonth),"-",
                             as.character(SowingDateDay)))

HarvestDateYear <- FirstHarvestYear
HarvestDateMonth <- month(HarvestDate)
HarvestDateDay <- day(HarvestDate)
HarvestDate <- as.Date(paste0(as.character(HarvestDateYear),"-",as.character(HarvestDateMonth),"-",
                              as.character(HarvestDateDay)))


SimStartYear <- ifelse (Wintercrop, FirstHarvestYear-1, FirstHarvestYear)
SimStartMonth <- month(SimStart)
SimStartDay <- day(SimStart)
SimStart <- as.Date(paste0(as.character(SimStartYear),"-",as.character(SimStartMonth),"-",as.character(SimStartDay)))

SimEndYear <- FirstHarvestYear
SimEndMonth <- month(SimEnd)
SimEndDay <- day(SimEnd)
SimEnd <- as.Date(paste0(as.character(SimEndYear),"-",as.character(SimEndMonth),"-",as.character(SimEndDay)))


SimYears <- seq(from=FirstHarvestYear, to=LastHarvestYear, by=1)

SowingDates <- as.Date(paste(as.character(ifelse(Wintercrops,SimYears-1, SimYears)),
                             format(SowingDate, format="%m-%d"),sep = "-"))


HarvestYears <- as.character(SimYears)
HarvestDates <- as.Date(paste(as.character(SimYears),
                             format(HarvestDate, format="%m-%d"), sep = "-"))


#
EndTimes <- as.Date(paste(HarvestYears,format(SimEnd, format="%m-%d"),sep = "-"))


#ExStartTime <- as.numeric(StartTimes -as.Date(0, origin="1899-12-30", tz='UTC'))


HarvestDates <- as.Date(HarvestDates)

# the array of start times for the simulations in date format


StartTimes <- as.Date(paste(as.character(ifelse(Wintercrops==TRUE, SimYears-1, SimYears)),
                            format(SimStart, format="%m-%d"),sep = "-"))

ExcelStartTimes <- as.numeric(StartTimes -as.Date(0, origin="1899-12-30", tz='UTC'))
ExcelSowingDates <- as.numeric(SowingDates -as.Date(0, origin="1899-12-30", tz='UTC'))
ExcelHarvestDates <- as.numeric(HarvestDates -as.Date(0, origin="1899-12-30", tz='UTC'))
ExcelEndTimes <- as.numeric(EndTimes -as.Date(0, origin="1899-12-30", tz='UTC'))

return(list(HarvestYears = HarvestYears,
            ExcelStartTimes = ExcelStartTimes,
            ExcelSowingDates = ExcelSowingDates,
            ExcelHarvestDates = ExcelHarvestDates,
            ExcelEndTimes = ExcelEndTimes))

}


#' Contruct a Controlfile for simulations
#'
#' @param IniPath path to the directory where the control file should be stored
#' @param CtrlPath path to the directory where the control file should be stored
#' @param CtrlFileName name of the control file
#' @param HarvestYears array of integers with the years of the simulations
#' @param Treatment1 array of strings with the names of treatment level 1
#' @param Treatment2 array of strings with the names of treatment level 2
#' @param Treatment3 array of strings with the names of treatment level 3
#'
#' @returns nothing
#' @export
MakeControlfile <- function(IniPath="", CtrlPath="", CtrlFileName="Ctrl.fn", HarvestYears,
                            Treatment1, Treatment2, Treatment3){

#  if (substr(IniPath, nchar(IniPath), nchar(IniPath)) != "/"){
#    IniPath <- paste0(IniPath, "/")
#  }
  HarvestYears <- as.character(HarvestYears)
  Treatment1 <- as.character(Treatment1)
  Treatment2 <- as.character(Treatment2)
  Treatment3 <- as.character(Treatment3)
  nTreatments <- 3

  if (is.na(Treatment3[1])){
    nTreatments <- 2
  }
  if (is.na(Treatment2[1])){
    nTreatments <- 1
  }
  if (is.na(Treatment1[1])){
    stop("No treatments specified")
  }

  if (nTreatments == 3) {
    df <- expand.grid(HYear = HarvestYears, Treatment1=Treatment1, Treatment2 = Treatment2,
                           Treatment3=Treatment3)
    df$TreatmentCode <- paste0(df$HYear,"_", df$Treatment1,"_", df$Treatment2,"_", df$Treatment3)
  }
  if (nTreatments == 2) {
    df <- expand.grid(HYear = HarvestYears, Treatment1=Treatment1, Treatment2 = Treatment2)
    df$TreatmentCode <- paste0(df$HYear,"_", df$Treatment1,"_", df$Treatment2)
  }
  if (nTreatments == 1) {
    df <- expand.grid(HYear = HarvestYears, Treatment1=Treatment1)
    df$TreatmentCode <- paste0(df$HYear,"_", df$Treatment1)
  }
  df$IniFN <- paste0(IniPath,"/",df$TreatmentCode, ".ini")

  ctrlfilefn <- file.path(CtrlPath, CtrlFileName)
  df$IniFN <- gsub("//", "/", df$IniFN)
  df$IniFN <- gsub("/", "\\\\", df$IniFN)
  write(file = ctrlfilefn, x = df$IniFN )
  return(list(ControlFileFN=ctrlfilefn, IniFiles=df$IniFN, TreatmentCodes=df$TreatmentCode, df=df))
}


#' Title
#'
#' @param df data frame with the data to be written to the HUME file
#' @param fn file name of the HUME file to be written
#' @param namesHUME vector with the names of the columns in the HUME file
#' @param unitsHUME vector with the units of the columns in the HUME file
#' @returns
#' @export
#'
#' @examples
WriteHumeDataFile <- function(df, fn, namesHUME, unitsHUME) {
  # Validate file path
  #  if (!file.exists(fn)) {
  #    stop("The specified file path does not exist.")
  #  }
  if (!all(namesHUME %in% names(df))) {
    stop("Not all column names for HUME file in data frame.")
  }


  df <- df[,namesHUME]
  # Datei öffnen
  #    fileHUME <- file(fn, open="wt", encoding="latin1")
  fileHUME <- file(fn, open="wb", encoding="UTF8")
  # Namen schreiben
  write(namesHUME, file = fileHUME, ncolumns = length(unitsHUME), append = FALSE, sep=" ")
  # Einheiten schreiben
  write(unitsHUME, file = fileHUME, ncolumns = length(unitsHUME), append = TRUE, sep=" ")
  # Daten schreiben
  write.table(df, file=fileHUME, append=TRUE, quote=FALSE, sep=" ",#sep="\t",
              eol="\n", dec=".", row.names=FALSE, col.names=FALSE)
  # Datei muss explizit geschlossen werden
  close(fileHUME)
}



