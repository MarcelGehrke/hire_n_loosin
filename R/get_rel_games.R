get_rel_games <-
  function(games,
           coaches,
           club = all_teams[[1]],
           direction = "forward") {
    
    games_club <- games[[club]]
    coaches_club <- coaches[[club]]
    
    if (length(coaches_club) > 0) {
      if (direction == "forward") {
        tmp <-  map_dfr(coaches_club,
                        ~ games_club %>%
                          filter(Date > .x) %>%
                          arrange(Date) %>%
                          slice(1))
      } else {
        tmp <-  map_dfr(coaches_club,
                        ~ games_club %>%
                          filter(Date < .x) %>%
                          arrange(desc(Date)) %>%
                          slice(1))
      }
      
      tmp %>%
        mutate(
          result = case_when(
            winner == club ~ "win",
            draw1 == club | draw2 == club ~ "draw",
            TRUE ~ "lost"
          ),
          homegame = Home == club,
          club = club,
          direction = direction
        )
      
    } else {
      warning(paste("Bei", club, "ist kein Trainer geflogen."))
      return(NULL)
    }
  }
