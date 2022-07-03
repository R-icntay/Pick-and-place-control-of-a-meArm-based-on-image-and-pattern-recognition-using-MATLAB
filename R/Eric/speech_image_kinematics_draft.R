# Eric and Ian: A touch of R in Robotics

# speech to text ----------------------------------------------------------
# Installing the packages to run rest APIs
#install.packages("httr")
#install.packages("jsonlite")

#Load the packages
library(here)
library(httr)
library(jsonlite)

#install and load packages that capture audio inputs
#install.packages('audio')
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


# To do Ian: --------------------------------------------------------------

# convert speech to keywords such as A1, B2, Capture


# Speech_text to manipulator workspace ------------------------------------

# Vector of board arangement
x <- c(A= 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1)
y <- c(A= 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1)

# Create checker boxes
w1 = waffle(x, rows = 8, flip = TRUE, colors = c("black", "white", "black", "white", "black", "white", "black", "white"), legend_pos = "", , size = 0.1) + 
  theme(plot.margin = margin(0, 0, 0, 0))
w2 = waffle(y, rows = 8, flip = TRUE, colors = c("white", "black", "white", "black", "white", "black", "white", "black"), legend_pos = "", size = 0.1) +
  theme(plot.margin = margin(0, 0, 0, 0))

# Make checker board
checkerboard <- w1 / w2 / w1 / w2 / w1 / w2 / w1 / w2
#checkerboard
ggsave("checkerboard.png", plot = checkerboard, width = 7, height = 7)

# Read Image into environment
library(magick)
img <- image_read("checkerboard.png") %>% 
  image_convert(type = "grayscale")

# Specifying dimensions of virtual board
dim_x = 160 * 4
dim_y = 160 *4
img <- image_resize(img, paste(dim_x, dim_y, sep = "x"))


# Assign names to checker boards e.g A1, G7
row_names = tibble(x = 1:dim_x, y = rep(LETTERS[1:8], each = dim_y/8), z = paste(x, "_", y, sep = "")) %>% pull(z)

col_names = tibble(x = 1:dim_x, y = rep(1:8, each = dim_x/8), z = paste(x, "_", y, sep = "")) %>% pull(z)


# Create array and number rows and columns
img_array <- drop(as.integer(pluck(img, 1)))
# rownames(img_array) <- 1:nrow(img_array)
# colnames(img_array) <- 1:ncol(img_array)
rownames(img_array) <- row_names
colnames(img_array) <- col_names


# Create data frame of checker board i.e pixel coordinates and location
img_df <- img_array %>% 
  as_tibble() %>% 
  mutate(y = row_names) %>% 
  #rowid_to_column(var = "y") %>% 
  pivot_longer(!y, names_to = "x", values_to = "pv") %>% 
  mutate(pv = scales::rescale(pv, to = c(0, 1))) %>% 
  # binarize
  mutate(pv = case_when(
    pv > 0.5 ~ 1,
    TRUE ~ 0))


img_dfx = img_df %>% 
  separate(y, c("y", "pl")) %>% 
  separate(x, c("x", "pn")) %>% 
  mutate(pos = paste(pl, pn, sep = "")) %>% 
  select(-c(pn, pl)) %>% 
  mutate(across(c(y, x, pv), as.numeric)) %>% 
  group_by(pos) %>% 
  mutate(centroidx = round(mean(x)), centroidy = round(mean(y))) %>% 
  ungroup()


# theme_set(theme_void())
# img_dfx %>% 
#   #filter(str_detect(pos, "A|B")) %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y, color = pv), show.legend = F) +
#   geom_point(aes(x = centroidx, y = centroidy), color = "white", size = 2) +
#   geom_text(aes(x = centroidx, y = centroidy + 2, label = paste(centroidx, centroidy, sep = ",")), color = "white", size = 2.5) +
#   scale_color_gradientn(colors = c("black", "white")) +
#   coord_equal()

#ggsave("checkfill.png", width = 7, height = 7)

# Obtaining checker box centroid coordinates
centroids <- img_dfx %>% 
  ungroup() %>% 
  distinct(pos, centroidx, centroidy)



# Function that maps virtual board to manipulator workspace
map_fun <- function(value, from_low, from_high, to_low, to_high){
  
  mapped_val = (value - from_low) * (to_high - to_low) / (from_high - from_low) + (to_low)
  
  return(mapped_val)
}

# Map virtual board coordinates to manipulator workspace
centroids <- centroids %>% 
  mutate(x_mapped = map_fun(centroidx, from_low = 0, from_high = dim_x, to_low = 9, to_high = -9),
         y_mapped = map_fun(centroidy, from_low = 0, from_high = 192, to_low = 11, to_high = 17)) %>% 
  mutate(across(where(is.numeric), round))

# Function that returns xy coordinates give a box name
get_centroid <- function(position){
  x = centroids %>% 
    filter(str_detect(pos, position)) %>% 
    pull(x_mapped)
  
  y = centroids %>% 
    filter(str_detect(pos, position)) %>% 
    pull(y_mapped)
  
  return(c(x, y))
}

### Result of speech to text goes here!!
# Example
# xy=get_centroid(position = "A2")
# xy


# To do: Get what Ian outputs as text and pass it to get_centroid ---------


# Forward & Inverse kinematics ------------------------------------------------------

# Function that calculates forward kinematics
fkin <- function(motor_angles){
  
  # Convert to radians
  angles = motor_angles * pi/180
  
  # Extract angles
  theta1 = angles[1] 
  theta2 = angles[2]
  theta3 = pi - angles[3]
  
  # Calculate x, y, z
  x <- 4 * cos(theta1) *( (3*cos(theta2 + theta3)) + (2*cos(theta2)))
  
  y <- 4 * sin(theta1) *( (3*cos(theta2 + theta3)) + (2*cos(theta2)))
  
  z <- (12 * sin(theta2 + theta3)) + (8*sin(theta2)) - (11/2)
  
  
  # Return a tibble
  fkin <- tibble(
    
    orientation = c("x", "y", "z"),
    
    # Multiply by -1 to re-orient y and z
    # mistake made during finding DH
    
    position = round(c(x, y * -1, z * -1))
    
  )
  
  return(fkin)
  
  
  
  
}

# Function that calculates inverse kinematics
ikin <- function(xyz_coordinates){
  
  # Extract xyz coordinates
  x = xyz_coordinates[1]
  y = xyz_coordinates[2]
  z = xyz_coordinates[3]
  
  # Account for manipulator moving right or left
  if (x >= 0){
    theta1 = atan(x/y) + pi/2
  } else {
    theta1 = atan(y/x) %>% abs()
  }
  
  # Calculate theta 3 since its needed in theta 2
  theta3 = acos((x^2 + y^2 + (z-5)^2 - 8^2 - 12^2) / (2*8*12))
  # 8 and 12 are the dimensions of manipulator arms
  
  
  # Calculate theta 2
  theta2 = atan((5.5 - z) / (sqrt(x^2 + y^2)) ) + atan((12 * sin(theta3)) / (8 + 12*cos(theta3)))
  
  if(theta2 > 0){
    theta2 = pi - abs(theta2)
  }
  
  tbl <- tibble(
    ef_position = c(x, y, z),
    motor_angles = (c(theta1, theta2, pi-theta3)*180/pi) %>% round()
  )
  
  return(tbl)
  
}

## Example
#ikin(xyz_coordinates = c(0, 13, 5))

# Define xyz
#z = ?
#xyz = c(xy, z)

# Motor angles for xyz
motor_angles <- ikin(xyz_coordinates = xyz)


# Trajectory --------------------------------------------------------------

# Tracing a trajectory can help achieve a smoother motion
library(matlib)


# Ian: How do we modify this for multiple values?
#Values chose arbitrary (play with the input values and check the effect on the output)
init_theta = 65 
fin_theta =  135


#Setting the constraints parameters
v_init = 0
v_fin = 0
acc_init = 0
acc_fin = 0

#We desire our motion to be executed in a span of two seconds
t0 = 0
tfin= 2

#finding the coefficients a0 - a5 using velocity and acceleration constraints set above
t_elements = c(1, t0, t0^2, t0^3, t0^4, t0^5,
               0, 1, 2*t0, 3*t0^2, 4*t0^3, 5*t0^4,
               0, 0, 2, 6*t0, 12*t0^2, 20*t0^3,
               1, tfin, tfin^2, tfin^3, tfin^4, tfin^5,
               0, 1, 2*tfin, 3*tfin^2, 4*tfin^3, 5*tfin^4,
               0, 0, 2, 6*tfin, 12*tfin^2, 20*tfin^3
)

t_mtx = matrix(t_elements, nrow = 6, ncol = 6, byrow = TRUE)
const_mtx_elmnts = c(init_theta, v_init, acc_init, fin_theta, v_fin, acc_fin) #constraints matrix
const_mtx = matrix(const_mtx_elmnts,nrow = 6, ncol = 1, byrow = TRUE)
a_mtx = inv(t_mtx) %*% const_mtx
a_mtx = as.data.frame(a_mtx)

#Substituting coefficient values into the quintic polynomial 
#defining matrix time interval
t = c(seq(from = 0, to = 2, by = 0.05))
time <- matrix(t,nrow = 41, ncol = 1, byrow = TRUE )
traj <- vector("numeric", 41)
x = 1
for(x in 1:length(t)){
  t = time[x,1]
  
  q_poly = a_mtx[1,1] + a_mtx[2,1]*t + a_mtx[3,1]*t^2 + a_mtx[4,1]*t^3 + a_mtx[5,1]*t^4 + a_mtx[6,1]*t^5 #equation 7 with coefficients substituted
  
  x =x+1
  
  traj[x] <- q_poly
}
#traj = round(traj)
traj


# To do: Modify trajectory to take multiple values ------------------------


# R to Arduino ------------------------------------------------------------

library(serial)

# See the ports available
# listPorts()

# Create an Arduino object and set up the interface parameters.
arduino = serialConnection(name = "test1",
                           port = "COM5",
                           mode = "9600,n,8,1" ,
                           buffering = "none",
                           newline = TRUE,
                           eof = "",
                           translation = "cr",
                           handshake = "none",
                           buffersize = 8096)

# Initialize the interface
open(arduino)

# testing whether the connection is open or not
# isOpen(arduino)


# Create dummy data
# n = 42
# arduino_input <- tibble(
#   c = sample(10:100, size = n, replace = T) %>%
#                      paste('C', sep = ''))


# To do: Eric Connect trajectory to motors --------------------------------



# Load in sample data
arduino_input = readRDS("traj.gz") %>% 
  mutate(traj = round(traj) %>% paste("C", sep = ""))

# Write data to serial port which will be read by Arduino
close(arduino)
open(arduino)
Sys.sleep(3)
for (r in 1:nrow(arduino_input)){
  Sys.sleep(0.3)
  write.serialConnection(arduino, paste(arduino_input[r,], collapse = ''))
  # data=capture.output(cat(read.serialConnection(arduino,n=0)))
  # df = tibble(rbind(df,data))
  
  #Sys.sleep(0.5)
}
Sys.sleep(3)


# read the values sent to the serial port connection by 
# Arduino script 
data_frm_arduino <- tibble(from_arduino = capture.output(cat(read.serialConnection(arduino)))) %>% 
  filter(if_any(where(is.character), ~ .x != "")) %>% 
  mutate_from_ard


data_frm_arduino

