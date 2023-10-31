# Load the required packages
library(tuneR)
library(seewave)
library(stringr)
library(dplyr)
library(jsonlite)
library(gsignal)

# Define a function to extract the audio track from a video file
extract_audio <- function(video_file, audio_file) {
  # Use ffmpeg command to extract the audio track and save it as a wav file
  system(paste("ffmpeg -i", video_file, "-vn -acodec pcm_s16le -ar 44100 -ac 1", audio_file))
}

# Define a function to read the subtitle file and extract the start and end times of each line
read_subtitles <- function(subtitle_file) {
  # Read the subtitle file as a character vector
  subtitles <- readLines(subtitle_file)
  
  # Find the indices of the lines that contain the time information
  time_indices <- which(str_detect(subtitles, "-->"))
  
  # Extract the start and end times from the time lines
  start_times <- str_extract(subtitles[time_indices], "^\\d{2}:\\d{2}:\\d{2},\\d{3}")
  end_times <- str_extract(subtitles[time_indices], "\\d{2}:\\d{2}:\\d{2},\\d{3}$")
  
  # Convert the start and end times to seconds
  start_seconds <- as.numeric(str_sub(start_times, 1, 2)) * 3600 +
    as.numeric(str_sub(start_times, 4, 5)) * 60 +
    as.numeric(str_sub(start_times, 7, 8)) +
    as.numeric(str_sub(start_times, 10, 12)) / 1000
  
  end_seconds <- as.numeric(str_sub(end_times, 1, 2)) * 3600 +
    as.numeric(str_sub(end_times, 4, 5)) * 60 +
    as.numeric(str_sub(end_times, 7, 8)) +
    as.numeric(str_sub(end_times, 10, 12)) / 1000
  
  # Extract the text from the subtitle lines
  text <- subtitles[time_indices + 1]
  
  # Return a data frame with the start and end times in seconds and the text
  return(data.frame(start = start_seconds, end = end_seconds, text = text))
}

# Define a function to extract the melodic content from an audio segment using wavelet analysis
extract_melody <- function(audio_segment) {
  
   # Convert the audio segment to a Wave object
   wave <- Wave(audio_segment, samp.rate = 44100)
   
   # Generate a complex Morlet wavelet with center frequency of 500 Hz and bandwidth of 100 Hz
   wavf <- cmorwavf(500,100)
   
   # Perform the continuous wavelet transform (CWT) of the wave object using the wavelet function
   cwt_res <- cwt(wave,wavf)
   
   # Get the wavelet coefficients matrix
   coef <- cwt_res$coef
   
   # Get the scales and frequencies vectors
   scales <- cwt_res$scales
   freqs <- cwt_res$frequencies
   
   # Find the indices of frequencies that are within one octave of tones (from C4 to B4)
   tone_indices <- which(freqs >= midi2freq(60) & freqs <= midi2freq(71))
   
   # Subset the coefficients matrix to keep only those frequencies
   coef_tones <- coef[tone_indices,]
   
   # Compute some statistics of the absolute values of the wavelet coefficients for each frequency band
   mean_coef <- colMeans(abs(coef_tones))
   sd_coef <- apply(abs(coef_tones),2,sd)
   max_coef <- apply(abs(coef_tones),2,max)
   min_coef <- apply(abs(coef_tones),2,min)
   
   # Return a list with the statistics and the frequencies
   return(list(mean_coef = mean_coef,
               sd_coef = sd_coef,
               max_coef = max_coef,
               min_coef = min_coef,
               freqs = freqs[tone_indices]))
}

# Define a function to identify the beginning, duration and end of each sound in an audio segment
identify_sounds <- function(audio_segment) {
  
   # Convert the audio segment to a Wave object
   wave <- Wave(audio_segment, samp.rate = 44100)
   
   # Apply a simple energy-based thresholding method to detect the onset and offset of sounds
   # See https://www.mathworks.com/help/audio/ug/detect-onset-and-offset-of-speech.html for details
   energy <- envelope(wave, f = 1000, envt = "abs")
   threshold <- 0.01 * max(energy)
   above_threshold <- energy > threshold
   diff_threshold <- diff(above_threshold)
   onset_indices <- which(diff_threshold == 1) + 1
   offset_indices <- which(diff_threshold == -1) + 1
   
   # Convert the indices to time values in seconds
   onset_times <- onset_indices / samp.rate
   offset_times <- offset_indices / samp.rate
   
   # Compute the duration of each sound in seconds
   duration_times <- offset_times - onset_times
   
   # Return a data frame with the onset, offset and duration of each sound
   return(data.frame(onset = onset_times, offset = offset_times, duration = duration_times))
}

# Define a function to correlate the detected sounds with the expected sounds based on the subtitles
correlate_sounds <- function(sounds, subtitles) {
  
  # Initialize an empty list to store the results
  results <- list()
  
  # Loop through each subtitle line
  for (i in seq_len(nrow(subtitles))) {
    
    # Get the start and end time of the line
    start_time <- subtitles$start[i]
    end_time <- subtitles$end[i]
    
    # Get the text of the line
    text <- subtitles$text[i]
    
    # Get the corresponding segment of sounds
    sounds_segment <- sounds[sounds$onset >= start_time & sounds$offset <= end_time,]
    
    # Split the text into words and remove punctuation marks
    words <- str_split(text, " ")[[1]]
    words <- str_remove_all(words, "[[:punct:]]")
    
    # Initialize an empty vector to store the matched sounds for each word
    matched_sounds <- vector()
    
    # Loop through each word
    for (j in seq_along(words)) {
      
      # Get the current word
      word <- words[j]
      
      # Find the index of the sound that matches the word best, using a simple string matching algorithm
      # See https://en.wikipedia.org/wiki/Levenshtein_distance for details
      sound_index <- which.min(sapply(seq_len(nrow(sounds_segment)), function(k) {
        levenshteinDistance(word, as.character(sounds_segment$name[k]))
      }))
      
      # Append the matched sound to the vector
      matched_sounds[j] <- sound_index
      
    }
    
    # Create a data frame with the words and the matched sounds
    result <- data.frame(word = words, sound = matched_sounds)
    
    # Append the result to the list
    results[[i]] <- result
    
  }
  
  # Convert the list to a data frame
  results <- bind_rows(results)
  
  # Return the results
  return(results)
}



# Define a function to encode the output using a musical notation suitable for JSON text file
encode_output <- function(melody, sounds, correlation) {
  
  # Initialize an empty list to store the events
  events <- list()
  
  # Loop through each sound in the sounds data frame
  for (i in seq_len(nrow(sounds))) {
    
    # Get the onset, offset and duration of the sound in seconds
    onset <- sounds$onset[i]
    offset <- sounds$offset[i]
    duration <- sounds$duration[i]
    
    # Get the corresponding segment of melody statistics and frequencies
    melody_segment_mean_coef <- melody$mean_coef[onset * samp.rate : offset * samp.rate]
    melody_segment_sd_coef <- melody$sd_coef[onset * samp.rate : offset * samp.rate]
    melody_segment_max_coef <- melody$max_coef[onset * samp.rate : offset * samp.rate]
    melody_segment_min_coef <- melody$min_coef[onset * samp.rate : offset * samp.rate]
    melody_segment_freqs <- melody$freqs
    
    # Find the index of the frequency band that has the highest mean coefficient value
    max_index <- which.max(melody_segment_mean_coef)
    
    # Get the frequency value of that band
    freq_value <- melody_segment_freqs[max_index]
    
    # Convert the frequency value to a MIDI note number (rounded to integer)
    note_value <- round(freq2midi(freq_value))
    
    # Get the velocity value as a fraction of one (scaled by the maximum coefficient value)
    velocity_value <- melody_segment_max_coef[max_index] / max(melody$max_coef)
    
    # Create an event array with the onset time, type "note", note value, velocity value and duration value
    event <- c(onset, "note", note_value, velocity_value, duration)
    
    # Append the event to the list
    events[[i]] <- event
    
  }
  
  # Convert the list to a matrix
  events <- do.call(rbind, events)
  
  # Create a JSON object with the events matrix and the correlation data frame
  output <- list(events = events, correlation = correlation)
  
  # Return the output
  return(output)
}


