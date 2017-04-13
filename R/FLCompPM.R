# FLCompPM.R - DESC
# /FLCompPM.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# iter {{{
setMethod("iter", signature(obj="FLCompPM"),
	  function(obj, iter) {

		# copy the iterate into the new slots
		names. <- c(getSlotNamesClass(obj, 'FLArray'), getSlotNamesClass(obj, 'FLPar'),
                getSlotNamesClass(obj, 'predictModel'))

		for(s. in names.)
		{
			if(dims(slot(obj, s.))$iter == 1)
				slot(obj, s.) <- iter(slot(obj, s.), 1)
			else
				slot(obj, s.) <- iter(slot(obj, s.), iter)
		}

		return(obj)
	  }
) # }}}
