/*
	Copyright (C) 2013 Matthew Denwood <matthewdenwood@mac.com>
	
	This code is based on the pareto distribution in JAGS version 3.3,
	and specifies the half cauchy distribution
	
    This file is part of runjags
	Original DPar.cc file is Copyright (C) 2002-10 Martyn Plummer, 
	from the source for JAGS version 3.3, licensed under GPL-2

    runjags is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    runjags is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with runjags  If not, see <http://www.gnu.org/licenses/>.
	
 */

#include "DHCauchy.h"
#include <util/nainf.h>
#include <rng/RNG.h>

#include <cmath>
#include <cfloat>

using namespace std;

// M_PI is defined in cmath for some architecures but not all
#define MYPI 3.14159265358979323846
#define ALPHA(par) (*par[0])

namespace runjags {

DHCauchy::DHCauchy()
    : RScalarDist("dhcauchy", 1, DIST_POSITIVE)
{
}

bool DHCauchy::checkParameterValue (vector<double const *> const &par) const
{
  return ALPHA(par) > 0;
}

double 
DHCauchy::d(double x, PDFType type,
vector<double const *> const &par, bool give_log) const
{

  if (x < 0)
    return give_log ? JAGS_NEGINF : 0;
    
  if (give_log)
  	return (log(2.0) + log(ALPHA(par))) - (log(MYPI) + log(pow(x,2.0) + pow(ALPHA(par),2.0)));
  else
    return (2*ALPHA(par)) / (MYPI * (pow(x,2.0) + pow(ALPHA(par),2.0)));
  
}

double 
DHCauchy::p(double x, vector<double const *> const &par, bool lower, bool give_log)
  const
{

  if (x < 0)
    return give_log ? JAGS_NEGINF : 0;

  // survival:
  double q = 1 - (2 * atan(x/ALPHA(par))) / MYPI;
  if (!lower) {
    return give_log ? log(q) : q;
  }
  else {
    return give_log ? log(1 - q) : (1-q);
  }
}

double 
DHCauchy::q(double p, vector<double const *> const &par, bool lower, 
	bool log_p) const
{
    if ( (log_p  && p > 0) || (!log_p && (p < 0 || p > 1)) )          
	return JAGS_NAN;
    
    double tp;
	double x;

    if (!lower) {
	if (log_p)
	    tp = 1-exp(p);
	else
	    tp = 1-p;
    }
    else {
	if (log_p)
	    tp = exp(p); 
	else
	    tp = p;
    }

	x = ALPHA(par) * tan(MYPI*tp / 2);
	
    return x;
}

double DHCauchy::r(vector<double const *> const &par, RNG *rng) const
{
    return q(rng->uniform(), par, false, false);
}

}
