open MarkovChain

let () = 
  Printexc.record_backtrace true;
  TextGenerator.run  
    ~files: ["../../../../data/mi_swann.txt"] 
    ~window_size: 8
    ~output_length: 200
  |> Format.printf "%s@."