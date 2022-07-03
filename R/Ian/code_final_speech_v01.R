library(stringr)
library(audio)
set.audio.driver(NULL)
rec_time <- 5 # Set the time duration of the recording
x_cont<-rep(NA_real_, 44100 * rec_time) #Defining number of samples recorded
inst <-record(x_cont, 44100, 1) #audio instance with sample rate = 44100 and number of channels = 1
wait(6)
rec <- inst$data # get the result from the audio instance object

save.wave(rec,"C:/Users/Hp/OneDrive/Desktop/R/sample4.wav")
.rs.restartR() # Temporary fix to clear the audio instance object(looking for a more elegant solution)
wait(6)
play(rec)


#Transcription_converting speech recorded to test
require(httr)
require(jsonlite)

headers = c(
  `Ocp-Apim-Subscription-Key` = 'be4e729c99e84c298a78e56049d06643',
  `Content-Type` = 'audio/wav'
)

params = list(
  `language` = 'en-US'
)

data = upload_file("C:/Users/Hp/OneDrive/Desktop/R/sample4.wav")  
res <- httr::POST(url = 'https://eastus.stt.speech.microsoft.com/speech/recognition/conversation/cognitiveservices/v1', httr::add_headers(.headers=headers), query = params, body = data)
result.list <- fromJSON(content(res, as  = 'text')) #%>% as.data.frame()
txt_output <- as.data.frame(result.list)

#View(txt_output)

txt_input <- txt_output[1,2]
View(txt_input)

library(assert)
assert(is.character(txt_input),msg = "Wrong input format")

list.a <- as.list(c(LETTERS[1:8]))
list.b <- as.list(c(1:8))
result.df <- expand.grid(list.a, list.b)

library(purrr)
possible_cmds<- list(
  first_element = c(LETTERS[1:8]),
  second_element = c(1:8),
  sep = ""
)
combo <- as.list(possible_cmds) %>% cross() %>% map(lift(paste)) 

#Extract text from transcribed string
word(txt_input, start = 1,end = -1, sep = fixed("and"))
init_pos <- word(txt_input, start = 1,end = 1)
final_pos <- trimws(word(txt_input, start = 2,end = -1, sep = fixed("and"))) #trims leading and trailing whitespaces

#Capture move exception here
possible_cmds<- list(
  first_element = c(LETTERS[1:8]),
  second_element = c(1:8),
  third_element = " capture.",
  sep = ""
)
capt_comb <- as.list(possible_cmds) %>% cross() %>% map(lift(paste)) 
assert(init_pos %in% combo,msg = "Start position should be a chess board location",stop = TRUE)

if(final_pos %in% capt_comb){
  print("KATA SIMU TUKO SITE TUKO SITE") #Capture algorithm here
} else {
  assert(final_pos %in% combo,msg = "Final position should be a chess board location",stop = TRUE)
}
init_pos
final_pos