open MarkovChain

let () = 
  TextGenerator.run  
    ~files: ["../../../../data/small_swann.txt"] 
    ~window_size: 4
    ~output_length: 200
  |> Format.printf "%s@."