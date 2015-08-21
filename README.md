# mergeELFS

R code for merging different waves of Estonian Labor Force Survey data.

ELFS has been conducted more-or-less regularily since 1995.  However,
almost every year there are some changes.  The major breaks were in
2000, when the survey switched from yearly retrospective setup to
quarterly rotating panel.  In 2009, single individual data file was
split into three separate files, one for working age members, one for
households, and one for household members.  Smaller changes in
variable names, codings, etc happen almost every year.

This code pulls data from all available waves and merges this into one
single panel, focused on income: for every wage observation we have
one observation in the panel.

Note that while this code is free (GPL 3), it is probably of little
use unless you also have the corresponding data.  This is individual
data and _not_ publicly available.  Your should contact Statistics
Estonia if you are interested.
