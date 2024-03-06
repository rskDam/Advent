library("magrittr")
library("data.table")
library("stringr")
data <- data.table::fread("data.txt", header = FALSE)

d <- data$V1

digits <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

digits_table <- data.table(text = digits, values = c(0:9))

recognize_digit <- function(text_array, from_right) {
   collected_rest_string <- paste0(text_array, collapse = "")
   detected_digits_entries <- str_detect(collected_rest_string, digits)

   if (sum(detected_digits_entries) == 0) {
      return()
   }
   detected_digits <- digits_table[detected_digits_entries, values]
   n <- length(text_array)
   if (length(detected_digits) != 1) {
      if (from_right) {
         smaller_text <- text_array[2:n]
      } else {
         smaller_text <- text_array[1:(n - 1)]
      }
      recognize_digit(smaller_text, from_right)
   } else {
      return(detected_digits)
   }
}

first_digit <- function(text, from_right) {
   split <- stringr::str_split(text, "", simplify = FALSE)[[1]]
   n <- length(split)
   if (!from_right) {
      for (i in 1:n) {
         if (!is.na(as.numeric(split[i]) %>% suppressWarnings())) {
            return(split[i])
         } else {
            recognized <- recognize_digit(split[1:i], from_right)
            if (!is.null(recognized)) {
               return(recognized)
            }
         }
      }
   } else if (from_right) {
      for (i in 1:n) {
         i_from_right <- n - i + 1
         if (!is.na(as.numeric(split[i_from_right]) %>% suppressWarnings())) {
            return(split[i_from_right])
         } else {
            recognized <- recognize_digit(split[i_from_right:n], from_right)
            if (!is.null(recognized)) {
               return(recognized)
            }
         }
      }
   }
}

sum_first_last_digit <- function(text) {
   first <- first_digit(text, FALSE)
   last <- first_digit(text, TRUE)
   return(paste0(first, last) %>% as.numeric() %>% sum())
}

test_data <- c(
   "two1nine",
   "eightwothree",
   "abcone2threexyz",
   "xtwone3four",
   "4nineeightseven2",
   "zoneight234",
   "7pqrstsixteen"
)

final <- sapply(d, sum_first_last_digit) %>% sum()
final
sapply(test_data, sum_first_last_digit) %>% sum()
