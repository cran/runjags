/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The only modification is the change in namespace to prevent clashes

*/

#ifndef DPQ_FUNCTION_H_
#define DPQ_FUNCTION_H_

#include <function/ScalarFunction.h>

namespace runjags {

    class RScalarDist;

    class DPQFunction : public ScalarFunction
    {
	RScalarDist const *_dist;
    public:
	DPQFunction(std::string const &name, RScalarDist const *dist);
	/**
         * Returns the distribution from which the function derives its
         * value
         */
	RScalarDist const *dist() const;
	/**
         * Strips off the first argument and then checks that the remaining
         * arguments are valid parameters for the distribution
         */
	bool checkArgs(std::vector<double const *> const &args) const;
    };

}

#endif /* DPQ_FUNCTION_H_ */
