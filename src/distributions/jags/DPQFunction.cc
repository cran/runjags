/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The only modification is the change in namespace to prevent clashes

*/

#include "DPQFunction.h"
#include "RScalarDist.h"

using std::vector;
using std::string;

namespace runjags {

    DPQFunction::DPQFunction(string const &name, RScalarDist const *dist) 
	: ScalarFunction(name, dist->npar() + 1), _dist(dist)
    {}
    
    RScalarDist const *DPQFunction::dist() const
    {
	return _dist;
    }
    
    bool DPQFunction::checkArgs(vector<double const *> const &args) const
    {
	vector<double const *> param(_dist->npar());
	for (unsigned int i = 0; i < param.size(); ++i) {
	    param[i] = args[i+1];
	}
	
	return _dist->checkParameterValue(param);
    }
}
