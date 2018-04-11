// mycode.js
//alert("hi there");
 //    alert("hi");
//$(document).ready(function() {
     // add event handler to generate random number and pass to shiny
     b = document.querySelectorAll(".ui-sortable-handle");
     console.log(b);
     b[1].onclick = function() {
          alert("hi");
          var number = Math.random();
          Shiny.onInputChange("mydata", number);
     };
     // recieve data from shiny
     Shiny.addCustomMessageHandler("myCallbackHandler",
          function(color) {
               document.getElementById("mydiv").style.backgroundColor = color;
          }
     );
     
//});