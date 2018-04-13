// mycode.js

$(document).ready(function() {
     // add event handler to generate random number and pass to shiny
    // e = document.getElementById("mydiv")
     //e = document.querySelector(".box");
     //e = document.querySelector(".ui-sortable-handle");
     const b = document.querySelectorAll("#source_vars div");
     var click_counter = 0;

     function sendDataToShiny(clicked_button) {
          //var number = Math.random();
          click_counter++;
          var val = clicked_button.getAttribute("data-value");
          Shiny.onInputChange("varname", val);
          Shiny.onInputChange("click_counter", click_counter);
     }
     
     for (var i = 0, len = b.length; i < len; i++) {
        b[i].addEventListener("click", function(){sendDataToShiny(this);}, false);
    }     
    
    
         Shiny.addCustomMessageHandler("myCallbackHandler", function(val) {
             console.log(val);
         });
     //b.addEventListener("click", function(){sendDataToShiny(this);}, false);
     
     // recieve data from shiny


     
});