open MarkovChain

let () = 
  TextGenerator.run  
    ~files: ["../../../../data/small_swann.txt"] 
    ~window_size: 5
    ~output_length: 1000
  |> Format.printf "%s@."