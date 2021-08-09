translink = function(tweet_date, clean_timeline_df){
  
  detour = c()
  clear = c()
  delays=c()
  
  
  for (i in 1:nrow(clean_timeline_df)){
    raw_date = as.Date(clean_timeline_df$created[i])
    if(grepl(tweet_date, raw_date, fixed=TRUE)){
      raw_tweet = clean_timeline_df$text[i]
      
      m_tweet = regexpr('.?[1-9]{0-9}{0,2}\\d+ ', raw_tweet)
      bus = unique(unlist(regmatches(raw_tweet, m_tweet)))
      bus = trimws(bus)
      
      detour_match = grepl("detour", raw_tweet)
      clear_match = grepl("clear|over|end|back", raw_tweet)
      delay_match = grepl("delay", raw_tweet)
      if(detour_match&!clear_match)
      {
        detour = c(detour, bus)
      }
      if(clear_match)
      {
        clear = c(clear, bus)
      }
      if(delay_match)
      {delays = c(delays, bus)}
    }
  }
  
  return(list(detour=detour, 
              clear=clear, 
              delays = delays))
}

to_df <- function(L) {
  pad.na <- function(x,len) {
    c(x,rep(NA,len-length(x)))
  }
  maxlen <- max(sapply(L,length))
  do.call(data.frame,lapply(L,pad.na,len=maxlen))
}

