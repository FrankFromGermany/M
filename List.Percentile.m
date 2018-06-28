// from NIST 7.2.6.2 Percentiles
// https://www.itl.nist.gov/div898/handbook/prc/section2/prc262.htm
// Note that there are other ways of calculating percentiles in common use
// Hyndman and Fan (1996) in an American Statistician article evaluated nine different methods (R1 to R9)
// for computing percentiles relative to six desirable properties. 
// Their goal was to advocate a "standard" definition for percentiles that would be implemented in statistical software. 
// Although this has not in fact happened, most statistical and spreadsheet software use one of the methods described in Hyndman and Fan.
// The method used here is patterned after the R6, R7, and R8 methods; R7 is the default method used in Excel and R and thus the default for this function
let 
    func = (sourceList as list, p as number, optional givenMethod as number) as number =>
    let
        method =
			if givenMethod < 6 or givenMethod is null then
				7 
			else 
				givenMethod,
        N = List.Count(sourceList),
        list = List.Buffer(List.Sort(sourceList,Order.Ascending)),
        max = list{N-1},
        pth = 
            if method = 6 then 
                p * (N + 1) 
            else if method = 7 then 
                1 + p * (N - 1) 
            else
                if p < (2/3)/(N + (1/3)) or p = (2/3)/(N + (1/3)) then 
                    list{0} 
                else if p > (N - (1/3))/(N + (1/3)) or p = (N - (1/3))/(N + (1/3)) then 
                    list{N-1}
                else
                    p * (N + (1/3) + (1/3)),
        k = Number.IntegerDivide(pth,1),
        d = Number.Mod(pth,1),
        Yp = 
            if k > 0 and k < N  then 
                list{k-1} + d * (list{k} - list{k-1}) 
            else if k = 0 then 
                list{0} 
            else if k > N or k = N then 
                list{N-1} 
            else null
    in
        Yp,
		
    doc = [
        Documentation.Name =  "List.Percentile",
        Documentation.Description = "Estimate a proportion of the data that falls above and below a given value.",
        Documentation.LongDescription = "Estimate a proportion of data above and below a percentage. The sourceList is the source list for the method. The percentile, p, denotes a value, such that at most (100 * p)% of the measurements are less than this value and at most 100(1 − p)% are greater. The optional parameter, method, is an integer between 1 and 9 that selects one of the nine quantile algorithms detailed in Hyndman and Fan (1996). Note: only methods 6, 7, and 8 are initialized currently.",
        Documentation.Category = "List",
        Documentation.Examples = {
            [
                Description =  "Calculate the 50th percentile (50%) value from an ordered list of values.", 
                Code = " Percentile({95.1772,95.1567,95.1937,95.1959,95.1442,95.061,95.1591,95.1195,95.1065,95.0925,95.199,95.1682}, 0.5)", 
                Result = "95.1579"
            ]
        }
    ] 
in 
    Value.ReplaceType(func, Value.ReplaceMetadata(Value.Type(func), doc))
