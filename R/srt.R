hour_secs <- 60 * 60
min_secs  <- 60    
srt_time_ptrn <- "^(\\d\\d):(\\d\\d):(\\d\\d),(\\d\\d\\d).+(\\d\\d):(\\d\\d):(\\d\\d),(\\d\\d\\d)$"

srt_validator <- function(s){
    valid_id(s$id)
    valid_times(s)
    valid_text(s$text)
}


valid_id <- function(x) {
    ok <- !is.na(as.integer(x))
    if (!ok) stop("not valid id for: ", x)
}

valid_text <- function(x) {
    ok <- is.character(x) && length(x) > 0L
    if (!ok) stop("there are subs with no text: ", x)
}

valid_times <- function(x){
    ## questi sono già interi, testare solo che siano nel range giusto
    valid_hour(x$start_hour)
    valid_min(x$start_min)
    valid_sec(x$start_sec)
    valid_msec(x$start_msec)
    valid_hour(x$stop_hour)
    valid_min(x$stop_min)
    valid_sec(x$stop_sec)
    valid_msec(x$stop_msec)
}

valid_hour <- function(x) {
    ok <- in_range(x, c(0,24)) #qualche dubbio sull'estremo superiore per srt
    if (!ok) stop("not valid hour for: ", x)
}

valid_min <- function(x) {
    ok <- in_range(x, c(0,59))
    if (!ok) stop("not valid min for: ", x)
}

valid_sec <- function(x) {
    ok <- in_range(x, c(0,59))
    if (!ok) stop("not valid sec for: ", x)
}

valid_msec <- function(x) {
    ok <- in_range(x, c(0,999))
    if (!ok) stop("not valid sec for: ", x)
}

in_range <- function(t, r) {
    t %in% seq(from = r[1], to = r[2])
}


srt_parser <- function(x){
    ## dati originali
    id_pos    <- 1L
    times_pos <- 2L
    text_pos  <- seq_along(x) %without% 1:2
    id    <- x[id_pos]
    times <- x[times_pos]
    text  <- lbmisc::rm_spaces(x[text_pos])
    ## elaborazioni
    times <- gsub(" ", "", times) # togliere eventuali spazi bianghi
    ## estrazione tempo inizio 
    start_hour      <- as.integer(gsub(srt_time_ptrn, "\\1", times))
    start_min       <- as.integer(gsub(srt_time_ptrn, "\\2", times))
    start_sec       <- as.integer(gsub(srt_time_ptrn, "\\3", times))
    start_msec      <- as.integer(gsub(srt_time_ptrn, "\\4", times))
    ## estrazione tempo fine 
    stop_hour      <- as.integer(gsub(srt_time_ptrn, "\\5", times))
    stop_min       <- as.integer(gsub(srt_time_ptrn, "\\6", times))
    stop_sec       <- as.integer(gsub(srt_time_ptrn, "\\7", times))
    stop_msec      <- as.integer(gsub(srt_time_ptrn, "\\8", times))
    ## metto a posto il pattern che si sa mai ...
    p <- "%02.0f:%02.0f:%02.0f,%03.0f --> %02.0f:%02.0f:%02.0f,%03.0f"
    times <- sprintf(p,
                     start_hour, start_min, start_sec, start_msec,
                     stop_hour, stop_min, stop_sec, stop_msec) 
    ## calcolo del tempo in secondi complessivi
    start_secs <-
        start_hour * hour_secs +
        start_min  * min_secs +
        start_sec             +
        start_msec/1000
    stop_secs <-
        stop_hour * hour_secs +
        stop_min  * min_secs +
        stop_sec             +
        stop_msec/1000
    ## output
    list(id    = id,
         times = times,
         text  = text,
         ## processed start time
         start_hour = start_hour,
         start_min  = start_min, 
         start_sec  = start_sec, 
         start_msec = start_msec,
         ## processed stop time
         stop_hour = stop_hour,
         stop_min  = stop_min, 
         stop_sec  = stop_sec, 
         stop_msec = stop_msec,
         ## processed msecs
         start_secs = start_secs,
         stop_secs = stop_secs
         )
}


## --------
## READ SRT
## --------

read_srt <- function(f = NULL, comment = '##', set_id_as_prog = TRUE, validate = TRUE){
    lines <- readLines(f)
    ## remove comments
    comment_lines <- grepl(sprintf("^%s", comment), lines)
    lines <- lines[!comment_lines]
    ## splitta sulla base di righe vuote (almeno una necessaria
    ## tra uno spezzone e l'altro
    srt <- split(lines, cumsum(lines == ''))
    ## now remove the first element if it's a "" and remove (all blank chunks)
    srt <- lapply(srt, function(x) if (x[1] == '') x[-1] else x)
    srt <- Filter(function(x) length(x) > 0L, srt)
    parsed_srt <- lapply(srt, srt_parser)
    ids <- unlist(lapply(parsed_srt, function(x) x$id))
    names(parsed_srt) <- ids
    ## check che id e progressivo coincidano
    ## -------------------------------------
    ## id è quello che si trova dentro
    ## prog è il progressivo post parsing
    ## auspicabilmente dovrebbero coincidere a fine file se no qualcosa
    ## è andato storto (manca una linea da qualche parte)
    progs <- seq(from = min(as.integer(ids), na.rm = TRUE),
                 length = length(parsed_srt))
    if (!all(as.integer(ids) == progs)){
        msg <- c("problemi nel parsing: id del srt origine e\n",
                 "prog del parsato non coincidono, probabilmente manca\n",
                 "una linea bianca necessaria tra due sub,\n",
                 "oppure l'srt è un collage di più file che partono da 1.\n",
                 "Il confronto tra i due: ")
        check <- data.frame('id' = ids, 'prog'  = progs)
        check <- check[with(check, id != prog), ]
        rownames(check) <- NULL
        warning(msg)
        print(check)
    }
    ## check/validazione del contenuto del srt parsato
    if (validate) lapply(parsed_srt, srt_validator)
    ## per fixare i collage (o fare quello che normalmente fanno i subeditor)
    if (set_id_as_prog) {
        id_to_prog <- function(s, p) {s$id <- p; s}
        parsed_srt <- Map(id_to_prog, parsed_srt, as.list(progs))
        names(parsed_srt) <- progs
    }
    ## return
    private$subs <- parsed_srt
    invisible(NULL)
}

initialize_srt <- function(id, ...){
    private$id <- id
    self$read(...)
}


## --------
## WRITESRT
## --------


write_srt <- function(f) {
    sel <- lapply(private$subs, function(s) {
        c(s$id, s$times, s$text, "")
    })
    sel <- unlist(sel)
    writeLines(sel, con = f)
    invisible(NULL)
}


print_srt <- function(){
    print(private$subs)
}


## ------------------------------------------------------------
## Statistiche
## ------------------------------------------------------------

keep_chars <- function(x) gsub('([^[:alpha:]])', '', x)

sub_id     <- function(x) x$id
sub_text   <- function(x) paste(x$text, collapse = '\\N')
sub_start  <- function(x) sprintf("%02.0f:%02.0f:%02.0f,%03.0f",
                                  x$start_hour,
                                  x$start_min,
                                  x$start_sec,
                                  x$start_msec)
sub_stop  <- function(x) sprintf("%02.0f:%02.0f:%02.0f,%03.0f",
                                 x$stop_hour,
                                 x$stop_min, 
                                 x$stop_sec, 
                                 x$stop_msec)
sub_length <- function(x) x$stop_secs - x$start_secs
sub_nlines <- function(x) length(x$text)
sub_nchars <- function(x) {
    only_chars <- keep_chars(x$text)
    nchar(paste0(only_chars, collapse = ''))
}
sub_longestline <- function(x) {
    only_chars <- keep_chars(x$text)
    max(nchar(only_chars))
}
sub_ytlink <- function(x, yt_id) {
    sprintf("https://youtu.be/%s?t=%d", yt_id,
            floor(x$start_secs) - 2)
}

srt_stats <- function(view = TRUE){
    
    ## id & stats
    id                  <- unlist(lapply(private$subs, sub_id))
    text                <- unlist(lapply(private$subs, sub_text))
    start               <- unlist(lapply(private$subs, sub_start))
    stop                <- unlist(lapply(private$subs, sub_stop))
    secs                <- unlist(lapply(private$subs, sub_length))
    nlines              <- unlist(lapply(private$subs, sub_nlines))
    nchars              <- unlist(lapply(private$subs, sub_nchars))
    nchars_longest_line <- unlist(lapply(private$subs, sub_longestline))
    cps          <- nchars / secs
    ## link <- unlist(lapply(private$subs, sub_ytlink, yt_id = yt_id))
    
    ## checks
    too_many_lines <- nlines > 2
    long_line      <- nchars_longest_line > 42
    high_cps       <- cps > 30
    nfails         <- too_many_lines + long_line + high_cps
    ## results
    rval <- data.frame(id                  ,
                       text                ,
                       start               ,
                       stop                ,
                       secs                ,
                       nchars              ,
                       cps                 ,
                       nchars_longest_line ,
                       nlines              ,
                       ## link                ,
                       high_cps            ,      
                       long_line           ,     
                       too_many_lines      ,
                       nfails)
    
    ord <- with(rval, order(nfails, cps, nchars_longest_line, nlines,
                            decreasing = TRUE))
    rval <- rval[ord, ]
    rownames(rval) <- NULL
    ## class(rval$link) <- 'hyperlink'

    ## Export
    outfile <- sprintf('/tmp/%s_stats.xlsx', private$id)
    message("srt stats exported in ", outfile)
    openxlsx::write.xlsx(rval,
                         file = outfile,
                         as.Table = TRUE)
    ## View
    if (view){
        cmd <- sprintf('libreoffice --calc %s &', outfile)
        system(cmd)
    }
}


## ------------
## split
## ------------

header_template <- "## --------------------------------------------------------
## Inizio spezzone: %s
## --------------------------------------------------------\n"

srt_chunks_maker <-
    function(srt = NULL,
             chunks_len_mins  = 5L,
             yt_id = "")
{

    if (yt_id == '') stop("You must give the YouTube id of the video")
    
    ## generazione dei secondi dei chunk
    chunks_len_secs <- chunks_len_mins * 60L
    max_secs <-
        floor(max(unlist(lapply(srt, function(x) x$start_secs)))) + 1L
    chunks_start_secs <- seq(0L, max_secs, by = chunks_len_secs)
    chunks_id <- seq_along(chunks_start_secs)
    chunks_digits <- secs_to_digits(chunks_start_secs)
    chunks_fname <- sprintf("subs_%s.srt", chunks_digits)

    ## add chunk number
    srt <- lapply(srt, function(x) {
        x$chunk <- max(which(x$start_secs >= chunks_start_secs))
        x
    })

    ## altri dati del singolo chunk
    yt_links <- sprintf("https://youtu.be/%s?t=%d",
                        yt_id,
                        chunks_start_secs)
    chunks_headers <- sprintf(header_template, yt_links)

    ## lista che splitta i subs in base al chunk
    srt_sel <- lapply(chunks_id, function(id){
        Filter(function(x) x$chunk %in% id, srt)
    })

    ## lista dati chunks
    chunk_worker <- function(dig, fn, id, yt, subs){
        list("dig" = dig, "fn" = fn, "id" = id, "yt" = yt, "subs" = subs)
    }

    res <- Map(chunk_worker,
               as.list(chunks_digits),
               as.list(chunks_fname),
               as.list(chunks_id),
               as.list(chunks_headers),
               srt_sel)
    res
}

## printer per esportazione del singolo chunk
srt_chunk_printer <- function(chunk,
                              output_dir = ".",
                              con_des = c('stdout', 'file'))
{
    ## creazione della connessione
    con_des <- match.arg(con_des)
    if (con_des == 'stdout') {
        con <- stdout()
    } else if (con_des == 'file') {
        ## add subs to sec to easy identify the splitting produced files
        con <- file(paste(output_dir, chunk$fn, sep = '/'), open = 'w')
    }
    ## stampa del chunk
    ## header: indirizzo youtube
    writeLines(chunk$yt, con)
    writeLines(c("", ""), con)
    ## stampa dei singoli subs del chunk
    av_sub_printer <- function(s, my_con = con) {
        lines <- c(s$id,
                   s$times,
                   ## inglese commentato
                   sprintf("## %s", s$text),
                   ## linee bianche, meglio abbondare che deficere
                   rep("", 4))
        writeLines(lines, con = my_con)
    }
    lapply(chunk$subs, av_sub_printer)
    ## chiusura della connessione
    if (con_des == 'file') close(con)
    invisible(NULL)
}


split_srt <- function(chunks_len_mins, yt_id, output_dir){

    chunks <- srt_chunks_maker(srt = private$subs,
                               chunks_len_mins = chunks_len_mins,
                               yt_id = yt_id)

    
    tmp <- lapply(chunks,
                  srt_chunk_printer,
                  con_des = 'file',
                  output_dir = output_dir)

    ## have them sorted by my ls
    system(sprintf("touch %s/*", output_dir))

    invisible(chunks)
}






## ----------
## Main class
## ----------
srt <- R6::R6Class(classname = "srt",
                   public = list(
                       initialize = initialize_srt,
                       read       = read_srt,
                       write      = write_srt,
                       print      = print_srt,
                       stats      = srt_stats,
                       split      = split_srt
                   ),
                   private = list(
                       id = NULL,
                       subs = NULL
                   ))
