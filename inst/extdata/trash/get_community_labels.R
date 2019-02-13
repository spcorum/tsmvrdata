get_community_labels = function(filepath,limit=1000) {
    names=list(rep(NULL,limit))
    i = 0
    con = file(filepath, "r")
    while ( TRUE ) {
        line = readLines(con, n = 1)
        if ( length(line) == 0 ) break
        x = substring(line, 1, 10)
        if (x == '@attribute') {
            i = i + 1
            splitted = strsplit(line, split = ' ')
            names[[i]] = splitted[[1]][2]
        }
    }
    close(con)
    return(names)
}
