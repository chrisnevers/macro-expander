open Gensym

type scope = int

let scope () = Gensym.gen_int ()
