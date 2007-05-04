#!/usr/bin/awk -f

#usage:
#cvs log | cvsstat.awk


/^Working file:/ { currentFile=$3 
                   files[$3] = 1
		   verbose = 1
		 }
/^revision [0-9]+\.[0-9]+/      { initial = ($2 == "1.1") }

/^date:/         { a = substr($6, 1, length($6) - 1)
		   author[a] += 1 
                   total_commits += 1
		   author_file[a " " currentFile] += 1
                   if (initial) {
                       initial_checkins[a] += 1
                       total_initial_checkins += 1
                   }
                 }

END {
    total_files = 0
    for (f in files) {
        total_files += 1
    }
    for (x in author) {
    print x ":"
        printf "\t%i (%.0f%%) initial checkins\n", initial_checkins[x], ((initial_checkins[x] / total_initial_checkins) * 100)
        printf "\t%i (%.0f%%) commits\n", author[x], ((author[x] / total_commits) * 100)
        a_files = 0
        for (c in author_file) {
            split(c, arr, " ")
            if (arr[1] == x) {
                a_files += 1
            }
        }
        printf "\t%i (%.0f%%) touched files\n", a_files, ((a_files / total_files) * 100)
        if (verbose) {
            for (c in author_file) {
                split(c, arr, " ")
                if (arr[1] == x) {
                    print "\t" arr[2] ":\t" author_file[c]
                }
            }
        }
    }
}
