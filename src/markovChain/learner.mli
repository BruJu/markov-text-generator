val learn_markov_chain :
  token_of_arc:(int -> int -> 'token) ->
  max_state_id:int ->
  walks:int list list -> 'token Definitions.MarkovChain.markov_chain
