library(rvest)
library(dplyr)
library(tibble)
library(tidytext)
library(stringr)
library(readr)

# Scrape content of the Act (English)
read_html("https://laws-lois.justice.gc.ca/eng/acts/I-2.75/FullText.html") %>% 
  html_element(".docContents") %>% 
  html_text2() -> text
  
# Replace accented French characters for easier filtering:
text %>% 
  str_replace_all("\u00E9", "e") %>% 
  str_replace_all("\u00FB", "u") -> filtered_text

# Remove any terms in parenthesis to drop French translations.
filtered_text %>%
  str_remove_all("\\([A-z ]+\\)") -> filtered_text

# Break contents into a word list
filtered_text %>% 
  tibble(text = .) %>%
  unnest_tokens(word, text) -> wordlist

# Filter numerical entries (including entries like 21st)
# Remove "Marginal" and entries that are prefixed by "note:"
# Remove posessives
# Drop stop words (using tidytext's stopwords database)
wordlist %>%
  filter(!str_detect(word, "^[0-9.,]+[a-z]{0,2}$") & word != "Marginal") %>% 
  mutate(word = str_remove_all(word, "^note:|\'s$|’s$")) %>% 
  anti_join(stop_words) -> filtered

# Check word frequencies
filtered %>%
  group_by(word) %>%
  count() -> wordcounts

# check word lengths
wordcounts %>% 
  mutate(length = nchar(word)) -> wordlengths

# find the most common lengths
wordlengths %>%
  group_by(length) %>%
  count()

# from a cursory review, it looks like a lot of our most used terms are 
# between 5 and 7 characters 
# also drop low frequency words which removed missed translations and antiguated legal terms
wordlengths %>%
  filter(length %in% 5:7 & n > 3) -> answers

# now we need a word list to test our guesses against
words::words %>% 
  filter(word_length %in% 5:7) -> valid_guesses

# sanity check that all of our answers are in the guesses
anti_join(answers, valid_guesses) -> missing_answers

# write the two files
write_lines(c("export const WORDS = [",
              paste0("  '", sort(answers$word), "',"),
              "]"), file = "src/constants/wordlist.ts")
write_lines(c("export const VALID_GUESSES = [",
              paste0("  '", sort(valid_guesses$word), "',"),
              "]"), file = "src/constants/validGuesses.ts")
