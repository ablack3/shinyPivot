// mycode.js

$(document).ready(function() {
     // add event handler to generate random number and pass to shiny
    // e = document.getElementById("mydiv")
     //e = document.querySelector(".box");
     //e = document.querySelector(".ui-sortable-handle");
     const b = document.querySelector("#source_vars div");
     console.log(b.getAttribute("data-value"));
     
     function sendDataToShiny(b) {
          //var number = Math.random();
          var val = b.getAttribute("data-value");
          Shiny.onInputChange("mydata", val);
     }
     
     b.addEventListener("click", function(){sendDataToShiny(this);}, false);
     
     // recieve data from shiny
     Shiny.addCustomMessageHandler("myCallbackHandler",
                                   function(color) {
                                        b.style.backgroundColor = color;
                                   }
     );
     
});