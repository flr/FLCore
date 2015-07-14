# FLArray.R - DESC
# FLArray.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# expand {{{

flq <- FLQuant(1:25, dim=c(5,5), quant='age')

# 1 to 4 seasons, new names
expand(flq, season=1:4)

# 5 to 10 ages, no new names
expand(flq, age=1:10)

# both
expand(flq, age=1:10, season=1:4)

# three options
expand(flq, age=1:10, season=1:4, unit=c('unique', 'new'))

# wrong dimname
expand(flq, llap=1:10)

# }}}
