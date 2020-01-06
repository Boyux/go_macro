#define LEFT { #enddef
#define RIGHT } #enddef
#define F prog #enddef
#define Item(F, T)
    struct {
        F T
    }
#enddef
#define get(T, F, R)
    func(item T) R {
        return item.F
    }
#enddef
#define name str(prog) #enddef