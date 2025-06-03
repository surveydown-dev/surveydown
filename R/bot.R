bot_checker <- function(db, session_id) {
    #change the is_bot value to 0 1 or 2
    #we pass the db through because we need the connection to update it database_uploading() in db.r
    # we have df, db, table (using Sys.getenv("SD_TABLE")), and changed fields (will have to see how its formatted).
    #we need the session_id as well so we know what we are actually comparing just getting the db is not enough
    df <- sd_get_data(db)






}

#Next we will bot checker online -> we can make run during sd_dashboard() calls or sd_get_data

