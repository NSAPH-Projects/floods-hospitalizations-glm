#Run once
install.packages("rgee")

library(googledrive)
drive_auth(email = 'saggarwal@g.harvard.edu')

#Everytime 

library(rgee)
ee_check()
rgee::ee_install()
ee_clean_pyenv() #use as necessary
ee_Initialize('saggarwal', drive = TRUE)

