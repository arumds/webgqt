
// Execute function body when the HTML document is ready
$(document).ready(function() {
  
    // javascript code to send data to shiny server
    document.getElementById("mydiv").onclick = function() {
        var number = Math.random();
        Shiny.onInputChange("mydata", number);
    };
  
  // handler to receive data from server
    Shiny.addCustomMessageHandler("myCallbackHandler",     
        function(color) {
          document.getElementById("mydiv").style.backgroundColor = color;
        }
    );

});
