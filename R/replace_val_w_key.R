replace_val_w_key <- function(val) {
  if (val %in% mapping$team) {
    unique(mapping[mapping$team == val,]$key)
    # mapping %>% filter(team == val) %>% pull(key)
  } else {
    NA_character_
  }
}

replace_val_w_key_v <- Vectorize(replace_val_w_key)