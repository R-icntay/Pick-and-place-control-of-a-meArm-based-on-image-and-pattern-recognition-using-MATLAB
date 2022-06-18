# Installing the packages to run rest APIs
install.packages("httr")
install.packages("jsonlite")

#Load the packages
library(httr)
library(jsonlite)

#install and load packages that capture audio inputs
install.packages('audio')
#documentation --> https://cran.r-project.org/web/packages/audio/audio.pdf
library(audio)
#view current audio drivers
#current.audio.driver()
#if none set
#view drivers available
#audio.drivers()
#set driver to use
#set.audio.driver (insert_name_here (NULL) --> to set the default from audio.drivers() output
#current.audio.driver --> verifies audio driver in use.

library(audio)
set.audio.driver(NULL)
rec_time <- 5 # Set the time duration of the recording
x_cont<-rep(NA_real_, 44100 * rec_time) #Defining number of samples recorded
inst <-record(x_cont, 44100, 1) #audio instance with sample rate and number of channels = 1
wait(6)
rec <- inst$data # get the result
file.create("sample.wav")
save.wave(rec,"E:/Project_302/sample.wav")
.rs.restartR() # Temporary fix to clear the audio instance object(looking for a more elegant solution)
wait(6)
play(rec)



#Transcription_converting speech recorded to test
require(httr)

headers = c(
  `Ocp-Apim-Subscription-Key` = 'be4e729c99e84c298a78e56049d06643',
  `Content-Type` = 'audio/wav'
)

params = list(
  `language` = 'en-US'
)

data = rec #upload_file("E:/Project_302/sample.wav")#'C:/Users/ianmu/Downloads/test_1_JJ.wav')
res <- httr::POST(url = 'https://eastus.stt.speech.microsoft.com/speech/recognition/conversation/cognitiveservices/v1', httr::add_headers(.headers=headers), query = params, body = data)
result.list <- fromJSON(content(res, as  = 'text')) #%>% as.data.frame()
txt_output <- as.data.frame(result.list)

View(txt_output)