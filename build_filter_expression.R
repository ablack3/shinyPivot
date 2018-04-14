library(rlang)

     
     map(head(starwars)$name, ~expr(!!. %in% !!letters[1])) %>% 
          reduce(function(a,b) expr(!!a || !!b))

function(){
     if(T) return(NULL)
}