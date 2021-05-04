## ----------------------------------------------------------------------
## Initialize the project in memory (every time) and setup (at beginning)
## ----------------------------------------------------------------------
initialize <- function(id = NULL, yt_id = NULL){
    if (is.null(id) || is.null(yt_id)) stop("must specify id and yt_id")
    ## id
    private$id      <- id
    private$yt_id   <- yt_id
    ## utenti
    self$users <- users$new(users_f = users_file)
    ## directory e file del progetto
    private$prj_dir  <- sprintf('%s/%s', subs_dir, id)
    private$avanz_f  <- sprintf('%s/%s', private$prj_dir, 'zz_avanzamento.csv')
    private$avanz    <- avanz$new(private$avanz_f, id)
    private$source_srt_f <- sprintf('%s/%s.srt', source_dir, id)
}

## funzione della main class
setup <- function(chunks_len_mins = 5, trn_to_rev_ratio = 6){

    if (dir.exists(private$prj_dir))
        stop(private$prj_dir, " already existing. Aborting.")
    else dir.create(private$prj_dir)

    ## import and split source
    private$source_srt <- srt$new(id = 'source',
                                  f = private$source_srt_f)
    chunks <- private$source_srt$split(chunks_len_mins = chunks_len_mins,
                                       yt_id = private$yt_id,
                                       output_dir = private$prj_dir)
    fnames <- unlist(lapply(chunks, function(c) c$fn))
    ## setup monitoring
    private$avanz$setup(trn_filenames = fnames, trn_to_rev_ratio = trn_to_rev_ratio)
}

## --------------
## Create sandbox
## --------------
## importa le linee da un file o da un menu con scelte pre-specificate
from_file_or_menu <- function(f = NULL,
                              ## parameters passed to lbmisc::menu2
                              m_choices  = NULL,
                              m_title    = NULL,
                              m_multiple = TRUE,
                              m_strict   = TRUE)
{
    ## dai la priorità al file se specificato, altrimento prendi da menu
    if (is.null(f)){
        if (length(m_choices) > 0) {
            rval <- lbmisc::menu2(choices   = m_choices,
                                  title    = m_title,
                                  multiple = m_multiple,
                                  return   = "values",
                                  strict   = m_strict)
            if (length(rval) == 1 && is.na(rval))
                rval <- NULL
        } else {
            rval <- NULL
        }
    } else {
        rval <- if (file.exists(f)) readLines(f) else NULL
        rval <- rval  %without% ''
    }
    rval
}

## genera il nome file (srt) di un sandbox in base a login e tipologia
sandbox_file <- function(user, role = c("translator", "revisor1", 'revisor2')){
    role <- match.arg(role)
    postfix <- switch(role,
                      translator = "trn",
                      revisor1   = "rev1",
                      revisor2   = "rev2")
    sprintf('subs/sandbox/%s_%s.srt', user, postfix)
}

## funzione che prende in input dei login github e fa il check che
## siano stati abilitati in data/users.csv (a seconda del permesso
## specificato) restituisce gli utenti abilitati, segnala se ve ne
## sono di non abilitati e interrompe se nessuno è abilitato
check_allowed_users <- function(users, role = c('translator', 'revisor1', 'revisor2')){
    role <- match.arg(role)
    allowed <- if      (role == 'translator') self$users$translators()
               else if (role == 'revisor1')   self$users$revisors1()
               else if (role == 'revisor2')   self$users$revisors2()
    ## do the check
    not_allowed_users <- users[users %nin% allowed]
    allowed_users     <- users[users %in% allowed]
    if (length(not_allowed_users) > 0)
        warning('Vi sono richieste per utenti non abilitati (ignorate). ',
                '\n  Gli utenti sono: ',
                paste(not_allowed_users, collapse = ', '),
                '.')
    if (length(allowed_users) == 0L)
        stop("Non vi sono utenti abilitati per questa richiesta.",
             "\n  Processo terminato.")
    allowed_users
}

## funzione della main class
create_sandbox <- function(sandbox_f = NULL, rev1_sandbox_f = NULL){

    sandbox_users<- from_file_or_menu(
        sandbox_f,
        m_title    = 'Specificare utenti (TRN) per sandbox',
        m_choices  = self$users$translators(),
        m_multiple = TRUE,
        m_strict   = TRUE)
    rev1_sandbox_users <- from_file_or_menu(
        rev1_sandbox_f,
        m_title    = 'Specificare utenti (REV1) per sandbox',
        m_choices  = self$users$revisors1(),
        m_multiple = TRUE,
        m_strict   = TRUE)

    ## sandbox di translators
    if (length(sandbox_users) > 0L) {
        ## notify: header
        lbmisc::ascii_header('sandbox traduttori')
        ## check permissions
        allowed_users <- private$check_allowed_users(sandbox_users, 'translator')
        ## files and dirs
        files <- sandbox_file(allowed_users, role = "translator")
        ## copia file
        tmp <- Map(file.copy,
                   from = list(sandbox_template_traduttori),
                   to = files,
                   overwrite = TRUE)
        ## notify: file list
        listing(files)
    }
    ## sandbox di revisori (per revisione 1)
    if (length(rev1_sandbox_users) > 0L) {
        ## notify: header
        lbmisc::ascii_header('sandbox revisori (fase 1)')
        ## check permissions
        allowed_users <- private$check_allowed_users(rev1_sandbox_users, 'revisor1')
        ## files and dirs
        files <- sandbox_file(allowed_users, role = "revisor1")
        ## copia file
        tmp <- Map(file.copy,
                   from = list(sandbox_template_revisori1),
                   to = files,
                   overwrite = TRUE)
        ## notify: file list
        listing(files)
    }
}

## ----------------------------------------------------------------
## Assign
## ----------------------------------------------------------------

## funzione che prende in input dei login github, una tipologia di
## utente e verifica che l'utente abbia un sandbox per la
## tipologia considerata
## restituisce gli utenti che hanno una sandbox o si lamenta e
## blocca similmente ad allowed users
check_sandbox <- function(users, role = c('translator', 'revisor1', 'revisor2')){
    
    role <- match.arg(role)
    users_sandbox <- sandbox_file(users, role = role)
    no_sandbox <- users[!file.exists(users_sandbox)]
    if (length(no_sandbox) > 0L){
        msg <- c("Alcuni utenti non hanno ancora il file di sandbox da",
                 role, ".\n ",
                 "Sono: ", paste(no_sandbox, collapse = ', '))
        warning(msg)
    }
    users_with_sandbox <- users %without% no_sandbox
    if (length(users_with_sandbox) == 0L)
        stop("No allowed users for this request")
    users_with_sandbox
}
    
## funzione che dato un progetto controlla che gli utenti di un tipo non
## abbia task non finiti (prima di richiedere) nuove assegnazioni
## da translator o revisor
check_homework <- function(users, role = c("translator", "revisor1", "revisor2")){
    role <- match.arg(role)
    homework_done <- function(u) {
        ## controlla se un singolo utente ha fatto i compiti
        file_non_finiti <-
            private$avanz$unfinished_homeworks(user = u, role = role)

        if (length(file_non_finiti) > 0L) {
            warning(u, ' has unfinished files: ',
                    paste(file_non_finiti, collapse = ' '),
                    '\n  Ignoring his/her request.')
        }
        all_complete <- length(file_non_finiti) == 0L
        all_complete
    }
    allowed_users <- Filter(f = homework_done, users)
    if (length(allowed_users) == 0L) stop("No allowed users for this request")
    allowed_users
}


## main class method
assign <- function(translate_f = NULL, revise2_f = NULL)
{
    ## aggiorna dati in memoria e salva su disco alla fine
    ## private$avanz$from_disk()
    on.exit(private$avanz$to_disk())

    assignable_translate <-
        length(private$avanz$assignable_files('translator')) > 0
    assignable_revise2 <-
        length(private$avanz$assignable_files('revisor2')) > 0
    
    ## se ci sono file da assegnare richiedi i traduttori
    translate_users <- if (assignable_translate){
                           from_file_or_menu(
                               translate_f,
                               m_title    = 'Specificare utenti (TRN) per assegnazione traduzione',
                               m_choices  = self$users$translators(),
                               m_multiple = TRUE,
                               m_strict   = TRUE)
                       } else NULL
    
    revise2_users <- if (assignable_revise2){
                         from_file_or_menu(
                             revise2_f,
                             m_title    = 'Specificare utenti (REV2) per assegnazione revisione',
                             m_choices  = self$users$revisors2(),
                             m_multiple = TRUE,
                             m_strict   = TRUE)
                     } else NULL

    ## -----
    ## Utils
    ## -----
    assign_worker <- function(old_f, assignee, new_f, from_path, to_path, role){
        ## worker di assegnazione: modifica file su disco e aggiorna avanzamento
        ## modifica il nome su disco
        file.rename(from = from_path, to = to_path)
        ## modifica stato avanzamento e salva su disco
        private$avanz$assign(old_f = old_f, assignee = assignee, new_f = new_f, role = role)  
    }

    excluded_users_message <- function(u)                
        message('per alcuni utenti non vi sono file assegnabili ',
                '(terminati, yee).', '\nSono: ',
                paste(u, collapse = ', '), '\n')

    try_assign <- function(users, role = c('translator', 'revisor2')){
        ## only translator and revisor2 file are assigned by me
        role <- match.arg(role)
        lbmisc::ascii_header(role)
        ## check for user permissions
        allowed_users <- private$check_allowed_users(users, role)
        ## check for unavailable sandboxes
        allowed_users <- check_sandbox(allowed_users, role)
        ## controllo compiti
        allowed_users <- private$check_homework(allowed_users, role)
        ## determina i file assegnabili sulla base del nome
        assignable_files <- private$avanz$assignable_files(role)
        ## assegnazioni massime sono il minimo tra i file assegnabili e gli
        ## utenti ammessi
        if (length(assignable_files) > 0L) {
            max_assignments <- min(length(allowed_users), length(assignable_files))
            assignment_seq <- seq_len(max_assignments)
            ## messaggio per coloro che hanno fatto domanda ma non vi sono più
            ## file da assegnare/tradurre
            if (length(allowed_users) > length(assignable_files)){
                excluded_users <- allowed_users[- assignment_seq]
                excluded_users_message(excluded_users)
            }
            ## accoppiamento e assegnazione
            assigned_files <- assignable_files[assignment_seq]
            assigned_users <- allowed_users[assignment_seq]
            from_path <- private$prj_path(assigned_files)
            new_filenames <- sprintf("%s_%s.srt",
                                     file_path_sans_ext(assigned_files),
                                     assigned_users)
            to_path <- private$prj_path(new_filenames)
            Map(assign_worker,
                as.list(assigned_files),
                as.list(assigned_users),
                as.list(new_filenames),
                as.list(from_path),
                as.list(to_path),
                as.list(role))
            ## notify: file listing (trn) or github paths (rev2)
            if (role == 'translator') {
                listing(to_path)
            } else if (role == 'revisor2'){
                gh_path <- "https://raw.githubusercontent.com/lbraglia/av_it_subs/main"
                rev2_urls <- sprintf("%s/%s", gh_path, to_path)
                listing(rev2_urls)
            }
        } else {
            message('Non vi sono file assegnabili: (terminati, yee).')
        }
    }
    
    ## assign translate
    ## ----------------
    if (length(translate_users) > 0L) {
        try_assign(translate_users, 'translator')
    }

    ## assign translate
    ## ----------------
    if (length(revise2_users) > 0L) {
        try_assign(revise2_users, 'revisor2')
    }
}


## -------------------------------------------------------

basename2 <- function(x) {
    if (is.null(x)) NULL
    else basename(x)
}

mark_progresses <- function(trn_completed_f  = NULL,
                            rev1_started_f   = NULL,
                            rev1_completed_f = NULL,
                            rev2_completed_f = NULL)
{
    ## updates dei dati interni e salvataggio a fine lavoro
    ## private$avanz$from_disk()
    on.exit(private$avanz$to_disk())
    
    ## TRANSLATE COMPLETATE
    translate_in_progress <- length(private$avanz$to_be_completed_files('translator')) > 0   
    compl_trn  <-
        if (translate_in_progress){
            basename2(from_file_or_menu(
                trn_completed_f,
                m_title    = 'Specificare files (TRN) per i quali è stata COMPLETATA la TRADUZIONE',
                m_choices  = private$avanz$to_be_completed_files('translator'),
                m_multiple = TRUE,
                m_strict   = TRUE))
        } else {
            NULL
        }

    ## check here
    ## browser()
    ## 
    if (length(compl_trn) > 0L){
        Map(private$avanz$mark_as_completed,
            as.list(compl_trn), as.list('trn'))
        cat("\n\n")
        self$available_rev1()
    }

    ## REV1 INIZIATE
    translate_compl_non_rev1 <- length(private$avanz$assignable_files('revisor1')) > 0
    started_rev1 <-
        if (translate_compl_non_rev1) {
            basename2(from_file_or_menu(
                rev1_started_f,
                m_title    = 'Specificare files (REV1) per i quali è INIZIATA la PRIMA REVISIONE (LINGUISTICA)',
                m_choices  = private$avanz$assignable_files('revisor1'),
                m_multiple = TRUE,
                m_strict   = TRUE))
        } else {
            NULL
        }
    if (length(started_rev1) > 0L) {
        get_rev1_user <- function(f) {
            lbmisc::menu2(title = sprintf('Chi è il revisore di: %s', f),
                          choices = self$users$revisors1())
        }
        rev1_assignee <- unlist(lapply(started_rev1, get_rev1_user))
        Map(private$avanz$mark_as_started,
            as.list(started_rev1),
            as.list(rev1_assignee),
            as.list('revisor1'))
    }

    ## REV1 COMPLETATE
    rev1_iniziate_non_compl <- length(private$avanz$to_be_completed_files('revisor1')) > 0
    compl_rev1 <-
        if (rev1_iniziate_non_compl) {
            basename2(from_file_or_menu(
                rev1_completed_f,
                m_title    = 'Specificare files (TRN) per i quali è stata COMPLETATA la PRIMA REVISIONE (LINGUISTICA)',
                m_choices  = private$avanz$to_be_completed_files('revisor1'),
                m_multiple = TRUE,
                m_strict   = TRUE))
        } else {
            NULL
        }
    if (length(compl_rev1) > 0L){
        Map(private$avanz$mark_as_completed,
            as.list(compl_rev1), as.list('rev1'))
    }
    
    ## REV2 COMPLETATE
    rev2_iniziate_non_compl <- length(private$avanz$to_be_completed_files('revisor2')) > 0
    compl_rev2 <-
        if (rev2_iniziate_non_compl){
            basename2(from_file_or_menu(
                rev2_completed_f,
                m_title    = 'Specificare files (REV) per i quali è stata COMPLETATA la SECONDA REVISIONE (LEGGIBILITA)',
                m_choices  = private$avanz$to_be_completed_files('revisor2'),
                m_multiple = TRUE,
                m_strict   = TRUE))
        } else {
            NULL
        }
    if (length(compl_rev2) > 0L){
        Map(private$avanz$mark_as_completed,
            as.list(compl_rev2), as.list('rev2'))
    }
    
    ## controlla se ci sono file revisione da creare, nel caso fallo e
    ## notifica
    revs_todo <- private$avanz$revs2_todo()
    if (length(revs_todo) > 0){
        create_rev <- function(rf){
            ## obtain trn to cat
            trns <- private$avanz$get_trn_fn_for_rev2(rf)
            ## create the file
            cmd <- sprintf("cat %s", paste(private$prj_path(trns),
                                           collapse = ' '))
            input <- pipe(cmd)
            on.exit(close(input))
            ## file per check con subs editor: parsa proprio l'srt e pulisci
            rev <- srt$new(id = rf, f = input)
            rev$write(f = private$prj_path(rf))
            ## update monitoring
            private$avanz$revs2_created(rf)
        }
        lapply(as.list(revs_todo), create_rev)
        ## Notifica
        self$available_rev2()
        ## ascii_header('Ready for revision check: file da assegnare')        
        ## cat("\n")
        ## cat(private$prj_path(revs_todo), sep = '\n')
        ## cat("\n\n")
        ## self$users$mention('revisor2')
        ## cat("\n")
    }
}

## -----------------------------------------------------------
## create the final srt after revisions
make_final_srt <- function(stats = TRUE){
    ## full paths to revision files
    revs <- private$prj_path(private$avanz$filenames("rev2"))
    ## remove BOM by Aegisub
    rm_bom <- paste(c('dos2unix', revs), collapse = ' ')
    system(rm_bom)
    cmd <- sprintf("cat %s", paste(revs, collapse = ' '))
    input <- pipe(cmd)
    on.exit(close(input))
    final_srt <- srt$new(id = private$id, f = input)
    ## exporting
    outfile <- private$prj_path(sprintf("%s_final.srt", private$id))
    final_srt$write(f = outfile)
    if (stats) final_srt$stats()
}

final_srt_stats <- function(){
    file <- private$prj_path(sprintf("%s_final.srt", private$id))
    final_srt <- srt$new(id = private$id, f = file)
    final_srt$stats()
}

list_assignee <- function(){
    private$avanz$list_assignee()
}

list_users <- function(){
    self$users$list_users()
}

monitoring <- function(){
    private$avanz$monitoring()
}

available_rev1  <- function(){
    self$users$mention("revisor1")
    cat(": files attualmente disponibili per la revisione linguistica:\n\n")
    cat(private$avanz$assignable_files('revisor1'), sep = '\n')
    cat("\n")
}

available_rev2  <- function(){
    self$users$mention("revisor2")
    cat(": files attualmente disponibili per la revisione di leggibilità:\n\n")
    cat(private$avanz$assignable_files('revisor2'), sep = '\n')
    cat("\n")
}


git_log_analysis <- function(){
    projs <- list.files('subs', full.names = TRUE)
    exclude_dirs <- projs %without% private$prj_dir
    exclude_dirs <- paste0(exclude_dirs, '/')
    exclude_dirs <- paste(sprintf("-x file:'%s'", exclude_dirs),
                          collapse = ' ')
    log_file <- sprintf("/tmp/av_it_subs_%s_log_analysis.html", private$id)
    cmd <- paste("gitinspector  -x author:'Luca Braglia'",
                 exclude_dirs,
                 "-f srt -F html -mrT . >",
                 log_file,
                 " && firefox ",
                 log_file,
                 collapse = ' ')
    cat(c("Executing:", "\n", cmd, '\n'), sep = '')
    system(cmd)
}

## one interactive method to rule them all
menu <- function(){

    choices <- matrix(c(
        "1", "Setup",
        "2", "Create sandboxes",
        "3", "Assign TRN or REV2",
        "4", "Mark progresses",
        "5", "Monitoring",
        "6", "List available REV1",
        "7", "List available REV2",
        "8", "Make final srt",
        "9", "Final SRT stats",
        "10", "List assignee",
        "11", "List users"
        ),
        ncol = 2,
        byrow = TRUE
    )

    repeat {
        ascii_header('        MAIN MENU        ')
        rval <- lbmisc::menu2(choices  = choices[, 2],
                              title    = "Select a number or 0 to exit",
                              multiple = FALSE,
                              return   = "index",
                              strict   = TRUE)
        switch(as.character(rval),
               "1" = self$setup(),
               "2" = self$create_sandbox(),
               "3" = self$assign(),
               "4" = self$mark_progresses(),
               "5" = self$monitoring(),
               "6" = self$available_rev1(),
               "7" = self$available_rev2(),
               "8" = self$make_final_srt(),
               "9" = self$final_srt_stats(),
               "10" = self$list_assignee(),
               "11" = self$list_users())
        if (is.na(rval)) break
    }
}

## ----------------------------------------------------------------
## Main class
## ----------------------------------------------------------------

#' classe che rappresenta un progetto
#' 
#' @export
prj <- R6::R6Class(classname = "prj",
                   public = list(
                       initialize = initialize,
                       setup = setup,
                       ## utenti
                       users = NULL,
                       create_sandbox = create_sandbox,
                       assign = assign,
                       mark_progresses = mark_progresses,
                       make_final_srt = make_final_srt,
                       final_srt_stats = final_srt_stats,
                       list_assignee =  list_assignee,
                       list_users =  list_users,
                       available_rev1 = available_rev1,
                       available_rev2 = available_rev2,
                       monitoring = monitoring,
                       git_log_analysis = git_log_analysis,
                       menu = menu
                   ),
                   private = list(
                       id = NULL,
                       yt_id = NULL,
                       ## directory e file
                       prj_dir = NULL,
                       prj_path = function(file) sprintf("%s/%s", private$prj_dir, file),
                       ## filepath e oggetto avanzamento
                       avanz_f = NULL,
                       avanz   = NULL,
                       ## file e oggetto sorgente
                       source_srt_f = NULL,
                       source_srt   = NULL,
                       check_allowed_users = check_allowed_users,
                       check_homework = check_homework
                   ))



