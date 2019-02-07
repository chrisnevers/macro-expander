module Gensym =
  struct
    let cnt = ref 0

    let gen_int () =
      let c = !cnt in
      cnt := !cnt + 1;
      c

    let gen_str str =
      let c = !cnt in
      cnt := !cnt + 1;
      str ^ (string_of_int c)

    let reset () =
      cnt := 0
end
