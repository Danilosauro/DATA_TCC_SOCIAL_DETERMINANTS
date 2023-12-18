library(dplyr)
library(tidyr)
library(tidyselect) 
library(readr) 
library(readxl)

install.packages('read.dbc')
options(repos='http://cran.rstudio.com/') 

install.packages('devtools')
install_github("danicat/read.dbc") 
install.packages("read.dbc")



data <- read_xlsx('/home/neopct/Downloads/data (1).xlsx') 
sifilis <- read.dbc('/home/neopct/Downloads/aula_introducao_R/arquivo/SIFABR21.dbc')

rm(data_dois)

install.packages('pacman')

pacman::p_load(rio, ggspatial, lubridate)

data <- today(tzone = "")
data

help("today")

install.packages('tidyverse')
library(tidyverse) 
