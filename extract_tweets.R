library(RSQLite)
library(tidyverse)
library(rtweet)

token<-create_token(
  app = "your_app_name",
  consumer_key = 'XXXXXXXXX',
  consumer_secret = 'XXXXXXXXXXXXXXXXXX',
  access_token = 'XXXXXXXXXXXXXXXX',
  access_secret = 'XXXXXXXXXXXXXXXXXX'
)

conn <- dbConnect(RSQLite::SQLite(), "translink_tweets.db")
query <- "(#RiderAlert) (from:TransLink) -filter:replies"

clean_tweets <- function(df){
  
  new_df <- df[,c("status_id","created_at", "text")]
  names(new_df) <- c("id","created","text")
  new_df$created = format(new_df$created, tz="America/Los_Angeles", usetz=TRUE)
  
  return(new_df)
}

#initial search, clean and transform
search_df = search_tweets(q = query, include_rts = FALSE)
search_df_clean = clean_tweets(search_df)

#load to database
dbWriteTable(conn, "alerts", search_df_clean)
dbDisconnect(conn)
             
# to stream tweets in every 1 hour for 24hours
# hour_counter <- 0
# 
# while(hour_counter <= 24){
#   
#   streamtime <- 3600
#   filename <- paste0("translink_stream", format(Sys.time(),'%d_%m_%Y__%H_%M_%S'),".json")
#   stream_tweets(q = query, timeout = streamtime, file_name = filename)
#   clean_stream <- clean_tweet_stream(filename, remove_rts = TRUE)
#   dbWriteTable(conn, "alerts", clean_stream, append = TRUE)
#   file.remove(filename)
#   hour_counter <- hour_counter + 1
# }
# clean_tweets <- function(filename, remove_rts = TRUE){
#   df <- parse_stream(filename)
#   
#   if(remove_rts == TRUE){
#     df <- filter(df,df$is_retweet == FALSE)
#   }
#   
#   new_df <- df[,c("status_id","created_at", "text")]
#   names(new_df) <- c("id","created","text")
#   new_df$created = format(new_df$created, tz="America/Los_Angeles",usetz=TRUE)
#   
#   return(new_df)
# }