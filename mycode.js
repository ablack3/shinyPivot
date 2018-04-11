// mycode.js

$(document).ready(function() {
     // add event handler to generate random number and pass to shiny
    // e = document.getElementById("mydiv")
     
     
     e = document.querySelector(".box");
     e = document.querySelector(".ui-sortable-handle");
     e = document.querySelector("#source_vars");
     e = document.querySelector("#source_vars div");
     console.log(e);
     e.onclick = function() {
          var number = Math.random();
          Shiny.onInputChange("mydata", number);
     };
     // recieve data from shiny
     Shiny.addCustomMessageHandler("myCallbackHandler",
                                   function(color) {
                                        e.style.backgroundColor = color;
                                   }
     );
     
});