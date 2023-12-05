data_txt <- readLines("OUT_lee2022-02-Crossvalidate.txt")
new_subcohort <- grepl("subcohort:  ", data_txt)
new_subcohort_n <- cumsum(new_subcohort)
data_txt_subcohort <- split(data_txt, new_subcohort_n)
names(data_txt_subcohort) <- sapply(data_txt_subcohort, function(x) gsub("subcohort:  ([^ ]*) .*datatype:  ([^ ]*).*", "\\1___\\2", x[1]))
lapply(data_txt_subcohort, function(x) {
    new_repeat <- grepl("------------------------------------------  Starting repeat", x)
    new_repeat_n <- cumsum(new_repeat)
    txt_splitted <- split(x, new_repeat_n)[-1]
    names(txt_splitted) <- sapply(txt_splitted, function(x) gsub(".*repeat  ([^ ]*).*cv  ([^ ]*).*FS  ([^ ]*).*", "repeat.\\1___cv.\\2___FS.\\3", x[1]))

    all_aucs <- sapply(txt_splitted, function(x) as.numeric(sub(".*: ", "", x[sapply(x, function(x) grepl("Area under the curve:", x))])))
    return(all_aucs)
})
