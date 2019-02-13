process_community = function(out_file = NULL, data_file,
                         name_file) {
    df = read.csv(data_file, header = FALSE, na.strings = "?")
    labs = get_community_labels(name_file)
    colnames(df) <- labs
    return(df)
}
