# Clear memory
rm(list = ls(all = TRUE))

# Display messages
cat('---------------------------------------\n\n')
cat('Welcome ', Sys.getenv('USERNAME'), '!\n\n',sep = '')
cat('The date today is ', format(Sys.Date(), '%d %B %Y.'), '\n\n', sep = '')
cat('Reporting session started at ', format(Sys.time(), '%I:%M %p'), '\n\n', sep = '')

# Set project path
home.dir <- ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))
m.project.path <- path.expand(file.path(home.dir
                                        ,'Analysis'
                                        ,'r4ds'))
m.project.path <- gsub('\\\\', '/', m.project.path)

if(!file.exists(m.project.path)){
  if(dir.create(m.project.path, recursive = TRUE))
    stop("The project directory \""
         ,m.project.path
         ,"\" has been created! \nPlease fill it with the relevant files and folders!")
  else
    stop("The project dirctory \""
         ,m.project.path
         ,"\" could not be created!")
}
# Run scripts 
# run.app <- function(files) {
#   run.app.internal <- function(file) {
#     if(!file.exists(file)) {
#       stop("The file \"", file, "\" does not exist!\n")
#     }
#     source(file, max.deparse.length =Inf, echo=FALSE)
#   }
#   invisible(sapply(files, run.app.internal))
# }
# files <- sort(list.files(file.path(m.project.path, "src", "r")))
# files <- files[-which(files == "00_main.R")]
# files <- files[order(files)]
# files <- file.path(m.project.path, "src", "r", files)
# invisible(run.app(files))


setwd(m.project.path)
# Packages and plotting options
ipk <- function(pkg){
  new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]
  if(length(new.pkgs)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only =T)
}


list.of.pkgs <- c("data.table", "tidyverse", "magrittr", "repr")
ipk(list.of.pkgs)
options(repr.plot.width = 4, repr.plot.height = 3.5)


# Display message

cat("\n\n", "Report generation ended at ", format(Sys.time(), "%I:%M %p"), sep = "")
cat("\n\nGoodbye ", Sys.getenv("USERNAME"), "!\n\n", sep = "")
cat("----------------------------------------\n\n")
Sys.sleep(5)

#EOF
