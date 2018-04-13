     Shiny.addCustomMessageHandler("myCallbackHandler", function(val) {
              for (var i = 0, len = b.length; i < len; i++) { 
               if (val == b[i].getAttribute("data-value")){
                    b.style.backgroundColor = "#7A0E81";
               }  
              }
          }
     );
     
     
  