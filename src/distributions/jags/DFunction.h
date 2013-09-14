/*

	This file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2
	
	The only modification is the change in namespace to prevent clashes

*/

#ifndef D_FUNCTION_H_
#define D_FUNCTION_H_

#include "DPQFunction.h"

namespace runjags {

    class DFunction : public DPQFunction
    {
    public:
	DFunction(RScalarDist const *dist);
	bool checkParameterValue(std::vector<double const *> const &args) const;
	double evaluate(std::vector <double const *> const &args) const;
    };

}

#endif /* D_FUNCTION_H_ */
