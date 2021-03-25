subs_dir <- "subs"
source_dir <- "source"
users_file <- "data/users.csv"
sandbox_template_revisori1 <- "subs/sandbox/template_revisori1.srt"
sandbox_template_traduttori <- "subs/sandbox/template_traduttori.srt"

listing <- function(x) {
    ## lista caratteri separati da newline (per nomi file tipicamente)
    cat(x, "\n", sep = '\n')
}

secs_to_digits <- function(s){# s is seconds as integer
    ## return numeric second to a XXYYZZ character format where
    ## XX=hours, YY=minutes, ' ZZ=seconds
    sec_in_a_hour <- 1L * 60L * 60L
    sec_in_a_min  <- 60L
    remaining <- as.integer(s)
    hours <- as.integer(floor(remaining / sec_in_a_hour))
    remaining <- remaining - hours * sec_in_a_hour
    minutes <- as.integer(floor(remaining / sec_in_a_min))
    secs <- remaining - minutes * sec_in_a_min
    sprintf("%s%s%s",
            lbmisc::to_00_char(hours, 2),
            lbmisc::to_00_char(minutes, 2),
            lbmisc::to_00_char(secs, 2))
}

digits_to_time <- function(d){
    ## from "000500" to "00:05:00"
    ptrn <- "([[:digit:]]{2})([[:digit:]]{2})([[:digit:]]{2})"
    gsub(ptrn, "\\1:\\2:\\3", d)
}

## ------------------------------------------------------
## Filename patterns (monitoraggio etc)
## -----------------------------------------------------
## subs_000000.srt             traduzione da iniziare
## subs_000000_nome.srt        traduzione iniziata
## subs_000000_nome_t.srt      tradotto
## subs_000000_nome_t_r.srt    in revisione 1
## subs_000000_nome_t_r_c.srt  revisione 1 completa
## -----------------------------------------------------
## revs_000000_00050000.srt         revisione2 da iniziare
## revs_000000_00050000_nome.srt    revisione2 assegnata
## revs_000000_00050000_nome_c.srt    revisione2 completata
## -----------------------------------------------------

## file patterns
compl_ptrn     <- "^subs_[[:digit:]]{6}_[[:alnum:]]+_c.srt" 
extract_ptrn     <- "^subs_([[:digit:]]{6})_([[:alnum:]]+)_c.srt" 

## patterns for filenames
trn_assigned_ptrn   <- '.+[[:digit:]]{6}.+\\.srt$'
trn_completed_ptrn  <- '.+[[:digit:]]{6}_.+_t\\.srt$'
rev1_assigned_ptrn  <- '.+[[:digit:]]{6}_.+_r\\.srt$'
rev1_completed_ptrn <- '.+[[:digit:]]{6}_.+_c\\.srt$'
rev2_assigned_ptrn  <- '.+[[:digit:]]{6}_[[:digit:]]{6}_.+\\.srt$'
rev2_completed_ptrn <- '.+[[:digit:]]{6}_[[:digit:]]{6}_.+_c\\.srt$'

## time extraction for translates and revs
rev2_starting_time_ptrn <- '.+([[:digit:]]{6})_[[:digit:]]{6}.+'
rev2_ending_time_ptrn   <- '.+[[:digit:]]{6}_([[:digit:]]{6}).+'
trn_starting_time_ptrn  <- '.+([[:digit:]]{6}).+'

all_revs_ptrn <- '^revs_[[:digit:]]{6}_[[:digit:]]{6}'
