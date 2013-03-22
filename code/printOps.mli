open Print

val unop: 'reg printer -> (MIPSOps.unop * 'reg * 'reg) printer
val binop: MIPSOps.binop -> string
val uncon: 'reg printer -> (MIPSOps.uncon * 'reg) printer
val bincon: MIPSOps.bincon -> string
