// mycode.js

$(document).ready(function() {
     // add event handler to generate random number and pass to shiny
    // e = document.getElementById("mydiv")
     //e = document.querySelector(".box");
     //e = document.querySelector(".ui-sortable-handle");
     const b = document.querySelectorAll("#source_vars div");

     function sendDataToShiny(clicked_button) {
          //var number = Math.random();
          var val = clicked_button.getAttribute("data-value");
          Shiny.onInputChange("mydata", val);
     }
     
     for (var i = 0, len = b.length; i < len; i++) {
        b[i].addEventListener("click", function(){sendDataToShiny(this);}, false);
    }     
    
     //b.addEventListener("click", function(){sendDataToShiny(this);}, false);
     
     // recieve data from shiny
     Shiny.addCustomMessageHandler("myCallbackHandler",
                                   function(color) {
                                        b.style.backgroundColor = color;
                                   }
     );
     
});