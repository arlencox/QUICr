let max_time = ref 0

let args = [
  ("-timeout", Arg.Set_int max_time, "Maximum run time")
]
let check_timeout () =
  if !max_time > 0 then begin
    let t = Unix.times ()  in
    let user = t.Unix.tms_utime in
    if user >= (float_of_int !max_time) then begin
      Format.printf "TIMEOUT@.";
      exit 17
    end
  end
  
